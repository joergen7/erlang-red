-module(node_assert_values).

-export([node_assert_values/1]).
-export([handle_incoming/2]).


handle_incoming(NodeDef,Msg) ->
    %%
    %% TODO fill this with life.
    %%
    nodes:send_msg_to_connected_nodes(NodeDef,Msg),
    NodeDef.


node_assert_values(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE, NodeDef, only_incoming).
