-module(node_junction).

-export([node_junction/1]).
-export([handle_incoming/2]).

%%
%% junctions are decorative elements that are "transparent" - they just
%% pass through the messages that they receive (having cloned the messages)
%%

handle_incoming(NodeDef,Msg) ->
    nodes:send_msg_to_connected_nodes(NodeDef,Msg).

node_junction(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE,NodeDef).
