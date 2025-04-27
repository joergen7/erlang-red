-module(ered_node_http_response).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Node to be used in conjunction with the http in node, This nodes sends
%% a response to a http connection.
%%

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%% outgoing message is triggered by the ered_http_node_http_in_handler module
handle_msg({incoming, Msg}, NodeDef) ->
    {ok, ReqPid} = maps:find(reqpid, Msg),
    ReqPid ! {reply, #{}, maps:get(payload, Msg)},
    {handled, NodeDef, Msg};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
