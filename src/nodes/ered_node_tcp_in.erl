-module(ered_node_tcp_in).

-behaviour(ered_node).

-include("ered_nodes.hrl").
-include("ered_tcpnodes.hrl").

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Tcp In node for doing tcp stuff
%%
%% Configuration:
%%     {
%%         "id": "ae4e5598c44f091d",
%%         "type": "tcp in",
%%         "z": "96f6f83dcab507e5",
%%         "name": "",
%%         "server": "server", <<--- this is 'client' for connect-to config
%%         "host": "", <<--- for "server", this is ignored
%%         "port": "1201",
%%         "datamode": "stream",
%%         "datatype": "utf8",
%%         "topic": "", <<---- add this topic to each msg generated
%%         "newline": "",  <<--\-- delimiter for strings (escaped)
%%         "trim": false, <<----+--- remove the delimitor from the payload
%%         "base64": false,
%%         "tls": "",
%%         "x": 467,
%%         "y": 287.5,
%%         "wires": [
%%             [
%%                 "588192fa298107ad"
%%             ]
%%         ]
%%     }
%%
%% REMARK: 'host' is not settable when using "server" mode, this is potentially
%% REMARK: sub-optimal becausae a device can have multiple interfaces.
%% REMARK: See https://discourse.nodered.org/t/tcp-in-node-listen-to-mode-listen-on-non-localhost/97849
%% REMARK: Not possible to select an interface to listen on but it's also not
%% REMARK: needed in ten years. Alternatively the message returned contains
%% REMARK: an `ip` attribute which is the ip of the interface that received
%% REMARK: the data packet.
%%
%% TODO: Ensure that this works with the supervisor node.
%% TODO: <<< this line is intentionally left blank. >>>
%%
-import(ered_nodes, [
    check_config/4,
    jstr/2,
    send_msg_to_connected_nodes/2
]).

-import(ered_messages, [
    convert_to_num/1,
    create_outgoing_msg/1
]).

-import(ered_nodered_comm, [
    node_status/5,
    unsupported/3
]).

-define(IsServer, <<"server">> := <<"server">>).
-define(IsClient, <<"server">> := <<"client">>).

%%
%%
%% check the server configuration, first the "listen on"
% erlfmt:ignore - alignment
start(#{?IsServer} = NodeDef, WsName) ->
    %% TODO: this is the todo list for this node - support the various
    %% TODO: configuration possibilities.
    case {
        check_config(<<"tls">>,      <<"">>,       NodeDef, WsName),
        check_config(<<"host">>,     <<"">>,       NodeDef, WsName),
        check_config(<<"datamode">>, <<"stream">>, NodeDef, WsName)
    } of
        {ok, ok, ok} ->
            ered_node:start(NodeDef#{?SetWsName, conn_count => 0}, ?MODULE);
        _ ->
            ered_node:start(NodeDef, ered_node_ignore)
    end;
%% server connect to
start(#{?IsClient} = NodeDef, WsName) ->
    case {
        check_config(<<"tls">>,      <<"">>,       NodeDef, WsName),
        check_config(<<"datatype">>, <<"utf8">>,   NodeDef, WsName),
        check_config(<<"datamode">>, <<"stream">>, NodeDef, WsName),
        check_config(<<"trim">>,     true,         NodeDef, WsName)
    } of
        {ok, ok, ok, ok} ->
            ered_node:start(NodeDef#{?SetWsName, conn_count => 0}, ?MODULE);
        _ ->
            ered_node:start(NodeDef, ered_node_ignore)
    end;
%% unknown server configuration.
start(#{<<"server">> := Unsupported} = NodeDef, WsName) ->
    ErrMsg = jstr("unsupported server mode ~p", [Unsupported]),
    unsupported(NodeDef, {websocket, WsName}, ErrMsg),
    ered_node:start(NodeDef, ered_node_ignore).

%%
%% Node Lifecycle handler
%%
% registering a listener
handle_event(
    {registered, _WsName, MyPid},
    #{?IsServer, ?GetHost, ?GetPort} = NodeDef
) ->
    %% TODO here we leave the scoping across WsName, a server can only bind
    %% TODO once on a port and to decide which Websocket receives which packets
    %% TODO of data is too much.
    case
        ered_tcp_manager:register_listener(Host, convert_to_num(Port), MyPid)
    of
        ok ->
            update_conn_count(NodeDef, 0);
        {error, Reason} ->
            post_status_error(NodeDef, Reason)
    end;
% registering a listener that first connects to a host + port
handle_event(
    {registered, WsName, MyPid},
    #{?IsClient, ?GetHost, ?GetPort} = NodeDef
) ->
    case
        ered_tcp_manager:register_connector(Host, convert_to_num(Port), MyPid)
    of
        connecting ->
            ?NodeStatus("connecting", "blue", "ring");
        {connected, _SessionId} ->
            ?NodeStatus("connected", "green", "dot");
        R ->
            ?NodeStatus(jstr("~p", [R]), "blue", "ring")
    end,
    NodeDef;
%%
%%
handle_event({stop, _WsName}, #{?IsServer, ?GetPort} = NodeDef) ->
    ered_tcp_manager:unregister_listener(convert_to_num(Port), self()),
    NodeDef;
%%
handle_event({stop, _WsName}, #{?IsClient, ?GetPort, ?GetHost} = NodeDef) ->
    ered_tcp_manager:unregister_connector(Host, convert_to_num(Port), self()),
    NodeDef;
%%
%% TCP status and traffic handlers
%%

%% listener events
handle_event({tcpl_data, Tuple}, NodeDef) ->
    send_out_msg(Tuple, <<"connected">>, NodeDef),
    NodeDef;
handle_event({tcpl_initiated, Tuple}, NodeDef) ->
    send_out_msg(Tuple, <<"initiated">>, NodeDef),
    update_conn_count(NodeDef, maps:get(conn_count, NodeDef) + 1);
handle_event({tcpl_closed, Tuple}, NodeDef) ->
    send_out_msg(Tuple, <<"closed">>, NodeDef),
    update_conn_count(NodeDef, maps:get(conn_count, NodeDef) - 1);
%%
%% connector events
handle_event({tcpc_data, Tuple}, NodeDef) ->
    send_out_msg(conn, Tuple, <<"connected">>, NodeDef),
    NodeDef;
handle_event({tcpc_initiated, _Tuple}, #{?GetWsName} = NodeDef) ->
    ?NodeStatus("connected", "green", "dot"),
    NodeDef;
handle_event({tcpc_retry, _Tuple}, #{?GetWsName} = NodeDef) ->
    ?NodeStatus("connecting", "blue", "ring"),
    NodeDef;
handle_event(
    {tcpc_closed, _Tuple},
    #{?GetWsName, ?GetHost, ?GetPort} = NodeDef
) ->
    ered_tcp_manager:unregister_connector(Host, convert_to_num(Port), self()),
    case
        ered_tcp_manager:register_connector(Host, convert_to_num(Port), self())
    of
        connecting ->
            ?NodeStatus("connecting", "blue", "ring");
        {connected, _SessionId} ->
            ?NodeStatus("connected", "green", "dot");
        R ->
            ?NodeStatus(jstr("~p", [R]), "blue", "ring")
    end,
    NodeDef;
%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%% ------------------------ Helpers
%%

%%
%%
stay_positive(V) when V < 0 ->
    0;
stay_positive(V) ->
    V.

%%
%%
post_status_error(#{?GetWsName} = NodeDef, Error) ->
    ?NodeStatus(io_lib:format("~p", [Error]), "red", "dot"),
    NodeDef#{conn_count => 0}.

update_conn_count(#{?GetWsName} = NodeDef, Cnt) ->
    ?NodeStatus(io_lib:format("~p connections", [stay_positive(Cnt)]), "", ""),
    NodeDef#{conn_count => stay_positive(Cnt)}.

%%
%%
send_out_msg(
    conn,
    {Payload, SessionId, _Hostname, _Port},
    Status,
    #{?GetTopic, ?GetWsName} = NodeDef
) ->
    {outgoing, Msg} = create_outgoing_msg(WsName),
    Msg2 = Msg#{
        <<"_session">> => #{
            <<"type">> => <<"tcp">>,
            <<"id">> => SessionId,
            <<"status">> => Status
        },
        ?SetTopic,
        ?SetPayload
    },
    send_msg_to_connected_nodes(NodeDef, Msg2).

send_out_msg({SessionId, InterfaceIp}, Status, NodeDef) ->
    send_out_msg({[], SessionId, InterfaceIp}, Status, NodeDef);
send_out_msg(
    {Payload, SessionId, InterfaceIp},
    Status,
    #{?GetTopic, ?GetWsName} = NodeDef
) ->
    {outgoing, Msg} = create_outgoing_msg(WsName),
    Msg2 = Msg#{
        <<"_session">> => #{
            <<"type">> => <<"tcp">>,
            <<"id">> => SessionId,
            <<"status">> => Status
        },
        <<"ip">> => InterfaceIp,
        ?SetTopic,
        ?SetPayload
    },
    send_msg_to_connected_nodes(NodeDef, Msg2).
