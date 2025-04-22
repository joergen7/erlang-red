-module(ered_node_link_in).

-export([node_link_in/2]).
-export([handle_event/2]).
-export([handle_incoming/2]).

%%
%% Strangely a "link in" node also has a "links" attribute (as does the link
%% out node). Strange because the links in this case are the backward links
%% to the link out nodes that are linked to this "link in" node. But messages
%% don't go backward so this attribute seems to be pointless. Unless there
%% is a deeper meaning that I'm not understanding.
%%

-import(ered_node_receivership, [enter_receivership/3]).
-import(ered_nodes, [
    send_msg_to_connected_nodes/2
]).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_incoming(NodeDef, Msg) ->
    send_msg_to_connected_nodes(NodeDef, Msg),
    {NodeDef, Msg}.

node_link_in(NodeDef, _WsName) ->
    enter_receivership(?MODULE, NodeDef, only_incoming).
