-module(ered_node_disabled).

-export([node_disabled/2]).
-export([handle_event/2]).
-export([handle_incoming/2]).
-export([handle_outgoing/2]).

%%
%% No Operation node that is used for all unknown types. It represents
%% a deadend for a message, it stops here.
%%

%%
%% if an disabled node receives a message its not an error, the design is
%% such that nodes do not know anything about other nodes so if a disabled
%% node is replaced by this, then so be it. Ignore the message and move on.
%%

-import(ered_node_receivership, [enter_receivership/3]).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

handle_incoming(NodeDef, Msg) ->
    {NodeDef, Msg}.

handle_outgoing(NodeDef, Msg) ->
    {NodeDef, Msg}.

node_disabled(NodeDef, _WsName) ->
    enter_receivership(?MODULE, NodeDef, incoming_and_outgoing).
