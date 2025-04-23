-module(ered_node_ignore).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% This is for nodes that appear in the flow data but that its ok not to
%% implement and really they do nothing.
%%
%% For example, tab and comment nodes are both ignored.
%%

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
