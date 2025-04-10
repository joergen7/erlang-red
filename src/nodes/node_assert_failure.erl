-module(node_assert_failure).

-export([node_assert_failure/1]).
-export([handle_incoming/2]).


handle_incoming(NodeDef,Msg) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),

    nodes:this_should_not_happen(
      NodeDef,
      io_lib:format("Assert Error: Node should not have been reached [~p](~p) ~p\n",[TypeStr,IdStr,Msg])
    ),
    nodes:status(NodeDef, "assert failed", "red", "dot").


node_assert_failure(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE, NodeDef, only_incoming).
