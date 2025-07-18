-module(ered_node_status).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Status nodes receives status updates from those nodes it listens to.
%% It passes these on to those nodes connected to in a the form of a
%% particular message:
%%
%% status: {
%%    fill: "grey"
%%    shape: "dot"
%%    text: "e"
%% },
%% source: {
%%    id: "97abdd5fa249b712"
%%    type: "debug"
%%    name: "debug 395"
%% },
%% _msgid: "cb8474786455d81c"
%%
%%
%% {
%%     "id": "c78e4af8c2d24782",
%%     "type": "status",
%%     "z": "84a5a362cafe703f",
%%     "name": "",
%%     "scope": null,  <<<------ null an implicit "all", "group" all in group
%%     "x": 1005,
%%     "y": 699,
%%     "wires": [
%%         [
%%             "65ead454e78ca028"
%%         ]
%%     ]
%% }
%%
-import(ered_ws_event_exchange, [
    subscribe/4
]).
-import(ered_messages, [
    create_outgoing_msg/1
]).
-import(ered_nodes, [
    check_config/4,
    jstr/1,
    send_msg_to_connected_nodes/2
]).

-import(ered_nodered_comm, [
    unsupported/3
]).

%%
%%
start(#{<<"scope">> := <<"group">>} = NodeDef, WsName) ->
    unsupported(NodeDef, {websocket, WsName}, jstr("group scope")),
    ered_node:start(NodeDef, ered_node_ignore);
start(#{<<"scope">> := null} = NodeDef, WsName) ->
    unsupported(NodeDef, {websocket, WsName}, jstr("all scope")),
    ered_node:start(NodeDef, ered_node_ignore);
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event({registered, WsName, _Pid}, NodeDef) ->
    {ok, NodePid} = maps:find('_node_pid_', NodeDef),
    {ok, NodesToListenTo} = maps:find(<<"scope">>, NodeDef),

    [
        ered_ws_event_exchange:subscribe(
            WsName, TgtNodeId, status, NodePid
        )
     || TgtNodeId <- NodesToListenTo
    ],
    NodeDef;
handle_event({stop, WsName}, NodeDef) ->
    {ok, NodePid} = maps:find('_node_pid_', NodeDef),
    ered_ws_event_exchange:unsubscribe(WsName, NodePid),
    NodeDef;
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_websocket({status, WsName, NodeId, Txt, Clr, Shp}, NodeDef) ->
    {outgoing, Msg} = create_outgoing_msg(WsName),
    %% TODO should really find the name and type of the source node
    %% TODO but on the other hand, with the node id, the frontend can
    %% TODO do that itself.
    send_msg_to_connected_nodes(
        NodeDef,
        Msg#{
            <<"source">> => #{
                <<"id">> => NodeId,
                <<"type">> => <<"">>,
                <<"name">> => <<"">>
            },
            <<"status">> => #{
                <<"fill">> => jstr(Clr),
                <<"shape">> => jstr(Shp),
                <<"text">> => jstr(Txt)
            }
        }
    ),
    NodeDef;
handle_websocket(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg({ws_event, Details}, NodeDef) ->
    {handled, handle_websocket(Details, NodeDef), empty};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
