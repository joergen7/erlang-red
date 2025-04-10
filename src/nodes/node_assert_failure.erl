-module(node_assert_failure).

-export([node_assert_failure/1]).
-export([handle_incoming/2]).

to_binary_if_not_binary(Obj) when is_binary(Obj) ->
    Obj;
to_binary_if_not_binary(Obj) when is_list(Obj) ->
    list_to_binary(Obj);
to_binary_if_not_binary(Obj) ->
    Obj.


handle_incoming(NodeDef,Msg) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),

    nodes:this_should_not_happen(
      NodeDef,
      io_lib:format("Assert Error: Node should not have been reached [~p](~p) ~p\n",[TypeStr,IdStr,Msg])
    ),

    IdStr       = nodes:get_prop_value_from_map(id,NodeDef),
    ZStr        = nodes:get_prop_value_from_map(z,NodeDef),
    NameStr     = nodes:get_prop_value_from_map(name,NodeDef,TypeStr),
    TopicStr    = nodes:get_prop_value_from_map(topic,Msg,""),

    Data = #{
             id       => IdStr,
             z        => ZStr,
             '_alias' => IdStr,
             path     => ZStr,
             name     => NameStr,
             topic    => to_binary_if_not_binary(TopicStr),
             msg      => jiffy:encode(Msg),
             format   => <<"Object">>
            },
    nodered:debug(Data,error),
    nodered:node_status(NodeDef, "assert failed", "red", "dot").


node_assert_failure(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE, NodeDef, only_incoming).
