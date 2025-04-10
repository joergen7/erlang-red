-module(node_noop).

%%
%% No Operation node that is used for all unknown types. It represents
%% a deadend for a message, it stops here.

-export([node_noop/1]).
-export([handle_incoming/2]).
-export([handle_outgoing/2]).

handle_incoming(NodeDef,Msg) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),
    nodes:this_should_not_happen(
      NodeDef,
      io_lib:format("Noop Node (incoming). Nothing Done for [~p](~p) ~p\n",
                    [TypeStr,IdStr,Msg])
    ).

handle_outgoing(NodeDef,Msg) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),
    nodes:this_should_not_happen(
      NodeDef,
      io_lib:format("Noop Node (outgoing) Nothing Done for [~p](~p) ~p\n",
                    [TypeStr,IdStr,Msg])
     ).

node_noop(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE,NodeDef).
