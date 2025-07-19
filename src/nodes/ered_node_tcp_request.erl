-module(ered_node_tcp_request).

-include("ered_nodes.hrl").
-include("ered_tcpnodes.hrl").

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Create a tcp request node
%%

%% {
%%         "id": "1e2a217f2ba03c2f",
%%         "type": "tcp request",
%%         "z": "1c1d2cb981cf9f01",
%%         "name": "",
%%         "server": "localhost",
%%         "port": "1001",
%%         "out": "immed", <<-+- "immed" = fire & forget mode,
%%                             \- "time" timeout after splitc milliseconds
%%         "ret": "string",
%%         "splitc": " ", <<-- timeout in milliseconds
%%         "newline": "",
%%         "trim": false,
%%         "tls": "",
%%         "x": 776.666748046875,
%%         "y": 681.9166870117188,
%%         "wires": [
%%             [
%%                 "18383a17a3db3322"
%%             ]
%%         ]
%% }
%%
%%

-import(ered_nodes, [
    check_config/4,
    jstr/2,
    send_msg_to_connected_nodes/2
]).

-import(ered_nodered_comm, [
    node_status/5,
    send_out_debug_msg/4,
    unsupported/3
]).

-import(ered_messages, [
    convert_to_num/1,
    create_outgoing_msg/1
]).

% erlfmt:ignore - alignment
start(#{ <<"out">> := <<"char">> } = NodeDef, WsName) ->
    unsupported(NodeDef, {websocket, WsName}, "when is seen connection"),
    ered_node:start(NodeDef, ered_node_ignore);
start(#{ <<"out">> := <<"count">> } = NodeDef, WsName) ->
    unsupported(NodeDef, {websocket, WsName}, "char count connection"),
    ered_node:start(NodeDef, ered_node_ignore);
start(#{ <<"out">> := <<"sit">> } = NodeDef, WsName) ->
    unsupported(NodeDef, {websocket, WsName}, "keep connection open"),
    ered_node:start(NodeDef, ered_node_ignore);

start(NodeDef, WsName) ->
    case {
          check_config(<<"ret">>,     <<"string">>, NodeDef, WsName),
          check_config(<<"newline">>, <<"">>,       NodeDef, WsName),
          check_config(<<"trim">>,    false,        NodeDef, WsName),
          check_config(<<"tls">>,     <<"">>,       NodeDef, WsName)
    } of
        {ok, ok, ok, ok} ->
            ered_node:start(NodeDef#{?SetWsName, ?EmptyBacklog}, ?MODULE);
        _ ->
            ered_node:start(NodeDef, ered_node_ignore)
    end.

%%
%%
%erlfmt:ignore - alignment
handle_event(
  {registered, WsName, _MyPid},
  #{
     <<"out">>    := <<"time">>,
     <<"splitc">> := TimeoutMS,
     ?GetPort,
     ?GetServer
   } = NodeDef
) ->
    case
        ered_tcp_manager:register_connector(Server, convert_to_num(Port), self())
    of
        {connected, SessionId} ->
            erlang:start_timer(
              convert_to_num(TimeoutMS), self(), treq_disconnect
            ),
            node_status(WsName, NodeDef, "connected", "green", "dot"),
            NodeDef#{?SetSessionId};
        connecting ->
            node_status(WsName, NodeDef, "connecting", "grey", "ring"),
            NodeDef
    end;

handle_event(
  {tcpc_initiated, {SessionId, _Host, _Port}},
  #{ ?GetWsName,
     ?GetBacklog,
     <<"splitc">> := TimeoutMS
   } = NodeDef
) ->
    node_status(WsName, NodeDef, "connected", "green", "dot"),
    erlang:start_timer(convert_to_num(TimeoutMS), self(), treq_disconnect),
    [ered_tcp_manager:send(SessionId, P) || P <- Backlog],
    NodeDef#{?SetSessionId, ?EmptyBacklog};

handle_event(
  {tcpc_data, {Data, _SessionID, _Host, _Port}},
  #{?GetWsName, <<"ret">> := <<"string">> } = NodeDef
) when is_list(Data) ->
    {outgoing, Msg} = create_outgoing_msg(WsName),
    send_msg_to_connected_nodes(
      NodeDef,
      Msg#{<<"payload">> => list_to_binary(Data)}
    ),
    NodeDef;

handle_event(
  {tcpc_data, {Data, _SessionID, _Host, _Port}},
  #{?GetWsName} = NodeDef
) ->
    {outgoing, Msg} = create_outgoing_msg(WsName),
    send_msg_to_connected_nodes(NodeDef, Msg#{ <<"payload">> => Data }),
    NodeDef;

handle_event(
  treq_disconnect,
  #{ ?GetWsName,
     ?GetSessionId,
     ?GetBacklog,
     ?GetPort,
     ?GetServer
   } = NodeDef
) ->
    [ered_tcp_manager:send(SessionId, P) || P <- Backlog],
    ered_tcp_manager:unregister_connector(Server, convert_to_num(Port), self()),
    ered_tcp_manager:close(SessionId),
    node_status(WsName, NodeDef, "disconnected", "grey", "ring"),
    maps:remove('_sessionid', NodeDef#{?EmptyBacklog});

handle_event(
  {stop, WsName},
  #{ ?GetSessionId,
     ?GetPort,
     ?GetServer
   } = NodeDef
) ->
    ered_tcp_manager:unregister_connector(Server, convert_to_num(Port), self()),
    ered_tcp_manager:close(SessionId),
    node_status(WsName, NodeDef, "disconnected", "grey", "ring"),
    maps:remove('_sessionid', NodeDef#{?EmptyBacklog});


%% fall through
handle_event(_Event, NodeDef) ->
    NodeDef.

%%
%%
%% --> send & forget --> immediately close connection after sending data
handle_msg(
    {incoming, #{?GetWsName, ?GetPayload} = Msg},
    #{
        <<"out">> := <<"immed">>,
        ?GetPort,
        ?GetServer
    } = NodeDef
) ->
    case tcp_connect(Server, Port) of
        {ok, Sock} ->
            gen_tcp:send(Sock, Payload),
            inet:close(Sock),
            node_status(WsName, NodeDef, "disconnected", "grey", "ring");
        {error, _Error} ->
            node_status(WsName, NodeDef, "error connecting", "grey", "ring")
    end,

    {handled, NodeDef, Msg};
%% --> connect and disconnet after timeout of milliseconds
handle_msg(
    {incoming, #{?GetPayload} = Msg},
    #{
        <<"out">> := <<"time">>,
        ?GetSessionId,
        ?GetBacklog
    } = NodeDef
) ->
    %% TODO here we ignore send errors, such as that the process is no longer
    %% TODO running - living dangerously in the year of the dragon.
    [ered_tcp_manager:send(SessionId, P) || P <- [Payload | Backlog]],
    {handled, NodeDef#{?EmptyBacklog}, Msg};
handle_msg(
    {incoming, #{?GetPayload} = Msg},
    #{
        <<"out">> := <<"time">>,
        ?GetBacklog
    } = NodeDef
) ->
    {handled, NodeDef#{?DefineBacklog([Payload | Backlog])}, Msg};
handle_msg(_Msg, NodeDef) ->
    {unhandled, NodeDef}.

%%
%% ----------------- Helpers
%%
tcp_connect(HostName, PortNum) ->
    gen_tcp:connect(
        binary_to_list(HostName),
        convert_to_num(PortNum),
        [{active, false}]
    ).
