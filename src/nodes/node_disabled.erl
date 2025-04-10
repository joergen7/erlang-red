-module(node_disabled).

%%
%% No Operation node that is used for all unknown types. It represents
%% a deadend for a message, it stops here.
%%

%%
%% if an disabled node receives a message its not an error, the design is
%% such that nodes do not know anything about other nodes so if a disabled
%% node is replaced by this, then so be it. Ignore the message and move on.
%%
-export([node_disabled/1]).
-export([handle_incoming/2]).
-export([handle_outgoing/2]).

handle_incoming(_NodeDef,_Msg) ->
    ok.

handle_outgoing(_NodeDef,_Msg) ->
    ok.

node_disabled(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE,NodeDef).
