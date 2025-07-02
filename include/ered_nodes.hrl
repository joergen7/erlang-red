-define(NODE_ID_AND_TYPE(NodeDef), {
    maps:get(<<"id">>, NodeDef), maps:get(<<"type">>, NodeDef)
}).

-define(BASE_DATA, #{
    <<"id">> => get_prop_value_from_map(<<"id">>, NodeDef),
    <<"z">> => get_prop_value_from_map(<<"z">>, NodeDef),
    <<"path">> => get_prop_value_from_map(<<"z">>, NodeDef),
    <<"name">> => get_prop_value_from_map(
        <<"name">>,
        NodeDef,
        get_prop_value_from_map(<<"type">>, NodeDef)
    )
}).

-define(PUT_WS(Map),
    Map#{'_ws' => WsName}
).
