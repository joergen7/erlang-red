-module(ered_node_trigger).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

handle_event(_, NodeDef) ->
    NodeDef.

handle_msg({incoming, Msg}, NodeDef) ->
    io:format("Trigger got an incoming message ~p~n", [Msg]),
    timer:sleep(2000),
    io:format("Trigger got an woke up ~p~n", [Msg]),
    {handled, NodeDef, Msg};
%%
%% Most import to define this, else this node will crash with any
%% unrecognised message.
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
