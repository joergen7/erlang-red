-module(ered_node_file_in).

%%
%% No Operation node that is used for all unknown types. It represents
%% a deadend for a message, it stops here.

-export([node_file_in/1]).
-export([handle_incoming/2]).

-import(node_receivership, [enter_receivership/3]).

handle_incoming(NodeDef, Msg) ->
    ered_nodes:send_msg_to_connected_nodes(NodeDef, Msg),
    NodeDef.

node_file_in(NodeDef) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, only_incoming).
