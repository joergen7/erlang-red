-module(node_assert_failure).

-export([node_assert_failure/1]).
-export([handle_incoming/2]).


handle_incoming(NodeDef,Msg) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),
    nodes:this_should_not_happen(io_lib:format("ASSERT FAILED [~p](~p)\n",[TypeStr,IdStr])),
    nodes:status(NodeDef, "assert failed", "red", "dot"),
    nodes:send_msg_to_connected_nodes(NodeDef,Msg).


node_assert_failure(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE,NodeDef).
