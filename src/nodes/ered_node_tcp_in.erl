-module(ered_node_tcp_in).

-behaviour(ered_node).

-include("ered_nodes.hrl").

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
    ws_from/1
]).

-define(AllChecksOK, {ok, ok, ok, ok, ok}).

-define(IsServer, <<"server">> := <<"server">>).
-define(IsClient, <<"server">> := <<"client">>).

-define(GetPort, <<"port">> := Port).
-define(GetHost, <<"host">> := Host).

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
        check_config(<<"datatype">>, <<"utf8">>,   NodeDef, WsName),
        check_config(<<"datamode">>, <<"stream">>, NodeDef, WsName),
        check_config(<<"newline">>,  <<"">>,       NodeDef, WsName)
    } of
        ?AllChecksOK ->
            ered_node:start(?PUT_WS(NodeDef#{conn_count => 0}), ?MODULE);
        _ ->
            ered_node:start(NodeDef, ered_node_ignore)
    end;
%% server connect to
start(#{?IsClient} = NodeDef, WsName) ->
    case {
        check_config(<<"tls">>,      <<"">>,       NodeDef, WsName),
        check_config(<<"datatype">>, <<"utf8">>,   NodeDef, WsName),
        check_config(<<"datamode">>, <<"stream">>, NodeDef, WsName),
        check_config(<<"newline">>,  <<"\\n">>,    NodeDef, WsName),
        check_config(<<"trim">>,     true,         NodeDef, WsName)
    } of
        ?AllChecksOK ->
            ered_node:start(?PUT_WS(NodeDef#{conn_count => 0}), ?MODULE);
        _ ->
            ered_node:start(NodeDef, ered_node_ignore)
    end;
%% unknown server configuration.
start(#{<<"server">> := _ServerType} = NodeDef, WsName) ->
    check_config(<<"server">>, <<"Unsupported">>, NodeDef, WsName),
    ered_node:start(NodeDef, ered_node_ignore).

%%
%% Node Lifecycle handler
%%
% registering a listener
handle_event({registered, _WsName, MyPid}, #{?IsServer} = NodeDef) ->
    %% TODO here we leave the scoping across WsName, a server can only bind
    %% TODO once on a port and to decide which Websocket receives which packets
    %% TODO of data is too much.
    R = ered_tcp_manager:register_listener(
        maps:get(<<"host">>, NodeDef),
        convert_to_num(maps:get(<<"port">>, NodeDef)),
        MyPid
    ),

    case R of
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
            node_status(WsName, NodeDef, "connecting", "blue", "ring");
        connected ->
            node_status(WsName, NodeDef, "connected", "green", "dot");
        R ->
            node_status(WsName, NodeDef, jstr("~p", [R]), "blue", "ring")
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
handle_event({tcpc_initiated, _Tuple}, #{?GET_WS} = NodeDef) ->
    node_status(WsName, NodeDef, <<"connected">>, "green", "dot"),
    NodeDef;
handle_event({tcpc_retry, _Tuple}, #{?GET_WS} = NodeDef) ->
    node_status(WsName, NodeDef, <<"connecting">>, "blue", "ring"),
    NodeDef;
handle_event({tcpc_closed, _Tuple}, #{?GET_WS, ?GetHost, ?GetPort} = NodeDef) ->
    ered_tcp_manager:unregister_connector(Host, convert_to_num(Port), self()),
    case
        ered_tcp_manager:register_connector(Host, convert_to_num(Port), self())
    of
        connecting ->
            node_status(WsName, NodeDef, "connecting", "blue", "ring");
        connected ->
            node_status(WsName, NodeDef, "connected", "green", "dot");
        R ->
            node_status(WsName, NodeDef, jstr("~p", [R]), "blue", "ring")
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
post_status_error(NodeDef, Error) ->
    node_status(
        ws_from(NodeDef),
        NodeDef,
        io_lib:format("~p", [Error]),
        "red",
        "dot"
    ),
    NodeDef#{conn_count => 0}.

update_conn_count(NodeDef, Cnt) ->
    node_status(
        ws_from(NodeDef),
        NodeDef,
        io_lib:format("~p connections", [stay_positive(Cnt)]),
        "",
        ""
    ),
    NodeDef#{conn_count => stay_positive(Cnt)}.

%%
%%
send_out_msg(conn, {Payload, SessionId, _Hostname, _Port}, Status, NodeDef) ->
    {outgoing, Msg} = create_outgoing_msg(ws_from(NodeDef)),
    Msg2 = Msg#{
        <<"_session">> => #{
            <<"type">> => <<"tcp">>,
            <<"id">> => SessionId,
            <<"status">> => Status
        },
        <<"topic">> => maps:get(<<"topic">>, NodeDef),
        <<"payload">> => Payload
    },
    send_msg_to_connected_nodes(NodeDef, Msg2).

send_out_msg({SessionId, InterfaceIp}, Status, NodeDef) ->
    send_out_msg({[], SessionId, InterfaceIp}, Status, NodeDef);
send_out_msg({Payload, SessionId, InterfaceIp}, Status, NodeDef) ->
    {outgoing, Msg} = create_outgoing_msg(ws_from(NodeDef)),
    Msg2 = Msg#{
        <<"_session">> => #{
            <<"type">> => <<"tcp">>,
            <<"id">> => SessionId,
            <<"status">> => Status
        },
        <<"ip">> => InterfaceIp,
        <<"topic">> => maps:get(<<"topic">>, NodeDef),
        <<"payload">> => Payload
    },
    send_msg_to_connected_nodes(NodeDef, Msg2).
