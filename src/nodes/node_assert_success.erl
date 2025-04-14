-module(node_assert_success).

-export([node_assert_success/1]).
-export([handle_stop/2]).

-import(node_receivership, [enter_receivership/3]).

%% erlfmt:ignore equals and arrows should line up here.
handle_stop(NodeDef,WsName) ->
    case maps:find('_mc_incoming',NodeDef) of
        {ok,0} ->
            {ok, IdStr}   = maps:find(id,NodeDef),
            {ok, TypeStr} = maps:find(type,NodeDef),

            nodes:this_should_not_happen(
              NodeDef,
              io_lib:format("Assert Error: Node was not reached [~p](~p)\n",
                            [TypeStr,IdStr])
            ),

            IdStr   = nodes:get_prop_value_from_map(id,   NodeDef),
            ZStr    = nodes:get_prop_value_from_map(z,    NodeDef),
            NameStr = nodes:get_prop_value_from_map(name, NodeDef, TypeStr),
            Data = #{
                     id       => IdStr,
                     z        => ZStr,
                     '_alias' => IdStr,
                     path     => ZStr,
                     name     => NameStr,
                     topic    => <<"">>,
                     msg      => <<"Assert Success Not Reached">>,
                     format   => <<"string">>
            },

            nodered:debug(WsName, Data, error),
            nodered:node_status(WsName, NodeDef, "assert failed", "red", "dot");
        _ ->
            ok
    end.

node_assert_success(NodeDef) ->
    nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, only_stop).
