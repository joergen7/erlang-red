-module(node_assert_success).

-export([node_assert_success/1]).
-export([handle_stop/1]).


handle_stop(NodeDef) ->
    case maps:find('_mc_incoming',NodeDef) of
        {ok,0} ->
            {ok, IdStr} = maps:find(id,NodeDef),
            {ok, TypeStr} = maps:find(type,NodeDef),
            nodes:this_should_not_happen(io_lib:format("ASSERT FAILED [~p](~p)\n",[TypeStr,IdStr])),
            nodes:status(NodeDef, "assert failed", "red", "dot");
        _ ->
            ok
    end.


node_assert_success(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE,NodeDef,only_stop).
