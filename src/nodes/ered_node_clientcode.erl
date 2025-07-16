-module(ered_node_clientcode).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Trigger code execution in the browser.
%%

-import(ered_nodes, [
    send_msg_to_connected_nodes/2
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

handle_msg({incoming, #{'_ws' := WsName} = Msg}, NodeDef) ->
    WsName ! {client_code_exec, NodeDef, Msg},
    {handled, NodeDef, dont_send_complete_msg};
handle_msg({client_code, Msg}, NodeDef) ->
    send_msg_to_connected_nodes(NodeDef, Msg),
    {handled, NodeDef, Msg};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
