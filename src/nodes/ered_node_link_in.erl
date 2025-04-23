-module(ered_node_link_in).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Strangely a "link in" node also has a "links" attribute (as does the link
%% out node). Strange because the links in this case are the backward links
%% to the link out nodes that are linked to this "link in" node. But messages
%% don't go backward so this attribute seems to be pointless. Unless there
%% is a deeper meaning that I'm not understanding.
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

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    send_msg_to_connected_nodes(NodeDef, Msg),
    {handled, NodeDef, Msg};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
