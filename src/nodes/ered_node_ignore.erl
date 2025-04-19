-module(ered_node_ignore).

-export([node_ignore/2]).

%%
%% This is for nodes that appear in the flow data but that its ok not to
%% implement and really they do nothing.
%%
%% For example, tab and comment nodes are both ignored.
%%

-import(ered_node_receivership, [enter_receivership/3]).

node_ignore(NodeDef, _WsName) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, nothing).
