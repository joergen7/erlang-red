-define(NODE_ID_AND_TYPE(NodeDef), {
    maps:get(<<"id">>, NodeDef), maps:get(<<"type">>, NodeDef)
}).

-define(BASE_DATA, begin
    #{
        <<"id">> => get_prop_value_from_map(<<"id">>, NodeDef),
        <<"z">> => get_prop_value_from_map(<<"z">>, NodeDef),
        <<"path">> => get_prop_value_from_map(<<"z">>, NodeDef),
        <<"name">> => get_prop_value_from_map(
            <<"name">>,
            NodeDef,
            get_prop_value_from_map(<<"type">>, NodeDef)
        )
    }
end).

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
-define(SET_WS, '_ws' => WsName).

%% Readable variation of the above - there is no need to scream.
-define(AddWsName(Map), begin
    Map
end#{
    '_ws' => WsName
}).
-define(GetWsName, '_ws' := WsName).
-define(SetWsName, '_ws' => WsName).

%%
%% Check for supervision
-define(BEING_SUPERVISED, '_being_supervised' := true).

-define(NodeStatus(EM, CLR, SHP), node_status(WsName, NodeDef, EM, CLR, SHP)).

%%
%% Message types
%%
-define(MSG_STOP, {stop, _WsName}).
-define(MSG_STOP_WS, {stop, WsName}).
-define(MSG_INCOMING, {incoming, Msg}).
-define(MSG_REGISTERED, {registered, WsName, NodePid}).

-define(NodePid, '_node_pid_' := NodePid).
-define(GetNodePid, '_node_pid_' := NodePid).

-define(GetPayload, <<"payload">> := Payload).
-define(SetPayload, <<"payload">> => Payload).
-define(AddPayload(V), <<"payload">> => V).

-define(TopicFromMsg, get_prop_value_from_map(<<"topic">>, Msg, "")).
-define(AddTopic(V), <<"topic">> => V).
-define(GetTopic, <<"topic">> := Topic).
-define(SetTopic, <<"topic">> => Topic).

-define(GetIdStr, <<"id">> := IdStr).
-define(GetTypeStr, <<"type">> := TypeStr).
