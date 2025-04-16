-module(ered_node_disabled).

%%
%% No Operation node that is used for all unknown types. It represents
%% a deadend for a message, it stops here.
%%

%%
%% if an disabled node receives a message its not an error, the design is
%% such that nodes do not know anything about other nodes so if a disabled
%% node is replaced by this, then so be it. Ignore the message and move on.
%%
-export([node_disabled/2]).
-export([handle_incoming/2]).
-export([handle_outgoing/2]).

-import(ered_node_receivership, [enter_receivership/3]).

handle_incoming(NodeDef, _Msg) ->
    NodeDef.

handle_outgoing(NodeDef, _Msg) ->
    NodeDef.

node_disabled(NodeDef,_WsName) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, incoming_and_outgoing).
