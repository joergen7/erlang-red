-module(node_noop).

%%
%% No Operation node that is used for all unknown types. It represents
%% a deadend for a message, it stops here.

-export([node_noop/1]).
-export([handle_incoming/2]).
-export([handle_outgoing/2]).

handle_incoming(NodeDef,_Msg) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),
    nodes:this_should_not_happen(io_lib:format("NOOP NODE HIT (incoming) Nothing Done for [~p](~p)\n",[TypeStr,IdStr])),
    error.

handle_outgoing(NodeDef,_Msg) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),
    nodes:this_should_not_happen(io_lib:format("NOOP NODE HIT (outgoing) Nothing Done for [~p](~p)\n",[TypeStr,IdStr])),
    error.

node_noop(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE,NodeDef).
