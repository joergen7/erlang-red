-module(ered_node_junction).

-export([node_junction/2]).
-export([handle_incoming/2]).

%%
%% junctions are decorative elements that are "transparent" - they just
%% pass through the messages that they receive (having cloned the messages)
%%

-import(ered_node_receivership, [enter_receivership/3]).
-import(ered_nodes, [
    send_msg_to_connected_nodes/2
]).

handle_incoming(NodeDef, Msg) ->
    send_msg_to_connected_nodes(NodeDef, Msg),
    {NodeDef, Msg}.

node_junction(NodeDef, _WsName) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, only_incoming).
