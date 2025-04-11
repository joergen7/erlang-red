-module(node_link_in).

-export([node_link_in/1]).
-export([handle_incoming/2]).

%%
%% Strangely a "link in" node also has a "links" attribute (as does the link out node).
%% Strange because the links in this case are the backward links to the link out nodes
%% that are linked to this "link in" node. But messages don't go backward so this attribute
%% seems to be pointless. Unless there is a deeper meaning that I'm not understanding.
%%
handle_incoming(NodeDef,Msg) ->
    nodes:send_msg_to_connected_nodes(NodeDef,Msg).

node_link_in(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE, NodeDef, only_incoming).
