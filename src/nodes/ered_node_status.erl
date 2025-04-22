-module(ered_node_status).

-export([node_status/2]).
-export([handle_event/2]).
-export([handle_ws_event/2]).

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

-import(ered_node_receivership, [enter_receivership/3]).
-import(ered_ws_event_exchange, [
    subscribe/4
]).
-import(ered_msg_handling, [
    create_outgoing_msg/1
]).
-import(ered_nodes, [
    jstr/1,
    send_msg_to_connected_nodes/2
]).

%%
%%
handle_event({registered, WsName, _Pid}, NodeDef) ->
    {ok, NodePid} = maps:find('_node_pid_', NodeDef),
    {ok, NodesToListenTo} = maps:find(scope, NodeDef),

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
handle_ws_event({status, WsName, NodeId, Txt, Clr, Shp}, NodeDef) ->
    {outgoing, Msg} = create_outgoing_msg(WsName),
    %% TODO should really find the name and type of the source node
    %% TODO but on the other hand, with the node id, the frontend can
    %% TODO do that itself.
    MsgWithSrc = maps:put(
        source,
        #{id => NodeId, type => "", name => ""},
        Msg
    ),
    FinalMsg = maps:put(
        status,
        #{fill => jstr(Clr), shape => jstr(Shp), text => Txt},
        MsgWithSrc
    ),
    send_msg_to_connected_nodes(NodeDef, FinalMsg),
    NodeDef;
handle_ws_event(_, NodeDef) ->
    NodeDef.

node_status(NodeDef, _WsName) ->
    enter_receivership(?MODULE, NodeDef, websocket_events).
