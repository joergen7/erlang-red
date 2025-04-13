-module(node_assert_status).


-export([node_assert_status/1]).
-export([handle_ws_event/2]).
-export([handle_stop/2]).

handle_stop(NodeDef,_WsName) ->
    NodeDef.

handle_ws_event(NodeDef, {status,WsName,NodeId,Txt,Clr,Shp}) ->
    io:format("got an websocket event ~p ~p ~p ~p ~p~n",[WsName,NodeId,Txt,Clr,Shp]),
    NodeDef;

handle_ws_event(_,_) ->
    ignore.

node_assert_status(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE,NodeDef,only_ws_events_and_stop).
