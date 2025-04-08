-module(node_disabled).

%%
%% No Operation node that is used for all unknown types. It represents
%% a deadend for a message, it stops here.

-export([node_disabled/1]).
-export([handle_incoming/2]).
-export([handle_outgoing/2]).

handle_incoming(NodeDef,_Msg) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),
    io:format("Node is disabled (incoming) for [~p](~p)\n",[TypeStr,IdStr]),
    ok.

handle_outgoing(NodeDef,_Msg) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),
    io:format("Node is disabled (outgoing) for [~p](~p)\n",[TypeStr,IdStr]),
    ok.

node_disabled(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE,NodeDef).
