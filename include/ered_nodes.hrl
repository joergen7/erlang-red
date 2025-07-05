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

%%
%% Avoid a "Warning: expression updates a literal" warning when using this
%% macro on a Hash directly, i.e., ?PUT_WS(#{....})
%% inspired by
%%     https://github.com/WhatsApp/erlfmt/issues/353#issuecomment-1957166129
-define(PUT_WS(Map), begin
    Map
end#{
    '_ws' => WsName
}).
-define(GET_WS, '_ws' := WsName).

%%
%% Check for supervision
-define(BEING_SUPERVISED, '_being_supervised' := true).

%%
%% Message types
%%
-define(MSG_STOP, {stop, _WsName}).
-define(MSG_STOP_WS, {stop, WsName}).
-define(MSG_INCOMING, {incoming, Msg}).
-define(MSG_REGISTERED, {registered, WsName, NodePid}).
