-module(ered_node_assert_values).

-include("ered_nodes.hrl").

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Assert node that checks the msg for correct values.
%%

-import(ered_nodered_comm, [
    assert_failure/3,
    debug/3,
    node_status/5,
    ws_from/1
]).
-import(ered_nodes, [
    get_prop_value_from_map/2,
    get_prop_value_from_map/3,
    jstr/2,
    send_msg_to_connected_nodes/2,
    this_should_not_happen/2
]).
-import(ered_messages, [
    convert_to_num/1,
    decode_json/1,
    escape_specials/1,
    get_prop/2,
    is_same/2,
    to_bool/1
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

debug_data(NodeDef, ErrMsg) ->
    D = ?BASE_DATA,
    D#{
        <<"msg">> => ErrMsg,
        <<"format">> => <<"string">>
    }.

%%
%%
check_rule_against_msg(<<"notset">>, <<"msg">>, Rule, Msg) ->
    case get_prop(maps:find(<<"p">>, Rule), Msg) of
        {ok, _, Prop} ->
            {failed,
                jstr(
                    "Prop '~p' should not be set on Msg: ~p",
                    [Prop, Msg]
                )};
        _ ->
            true
    end;
check_rule_against_msg(<<"set">>, <<"msg">>, Rule, Msg) ->
    case get_prop(maps:find(<<"p">>, Rule), Msg) of
        {ok, _, _} ->
            true;
        {undefined, Prop} ->
            {failed,
                jstr(
                    "Prop '~p' not set on Msg: ~p",
                    [Prop, Msg]
                )}
    end;
check_rule_against_msg(<<"noteql">>, <<"msg">>, Rule, Msg) ->
    case get_prop(maps:find(<<"p">>, Rule), Msg) of
        {ok, Val, Prop} ->
            {ok, ReqVal} = maps:find(<<"to">>, Rule),
            case is_same(ReqVal, Val) of
                true ->
                    {failed,
                        jstr(
                            "Prop '~p': Unequal but same. Exp: ~n '~p' ~n Was: ~n '~p' ~n",
                            [Prop, ReqVal, Val]
                        )};
                _ ->
                    true
            end;
        {undefined, Prop} ->
            {failed, jstr("Prop not set on msg: '~p'", [Prop])}
    end;
%% eql operator on the msg - about the only thing that is
%% supported at the time of writing this comment.
check_rule_against_msg(<<"eql">>, <<"msg">>, Rule, Msg) ->
    {ok, ToType} = maps:find(<<"tot">>, Rule),
    {ok, ReqVal} = maps:find(<<"to">>, Rule),

    case get_prop(maps:find(<<"p">>, Rule), Msg) of
        {ok, Val, Prop} ->
            eql_msg_op(Prop, Val, ToType, ReqVal, Msg);
        {undefined, Prop} ->
            {failed, jstr("Prop not found on msg: '~p'", [Prop])}
    end;
check_rule_against_msg(<<"mth">>, <<"msg">>, Rule, Msg) ->
    {ok, ToType} = maps:find(<<"tot">>, Rule),
    {ok, ReqVal} = maps:find(<<"to">>, Rule),

    case get_prop(maps:find(<<"p">>, Rule), Msg) of
        {ok, PropVal, Prop} ->
            case re:compile(ReqVal) of
                {ok, ReqPattern} ->
                    match_value_on_msg(
                        Prop,
                        PropVal,
                        ToType,
                        ReqPattern,
                        ReqVal,
                        Msg
                    );
                _ ->
                    {failed, jstr("Match not RegExp: '~p'", [ReqVal])}
            end;
        {undefined, Prop} ->
            {failed, jstr("Propery unfound on Msg: '~p'", [Prop])}
    end;
check_rule_against_msg(_Operator, _ObjectType, _, _) ->
    unsupported.

%%
%%
match_value_on_msg(Prop, MsgVal, <<"str">>, ReqPattern, MatchVal, Msg) when
    is_integer(MsgVal)
->
    match_value_on_msg(
        Prop,
        integer_to_list(MsgVal),
        <<"str">>,
        ReqPattern,
        MatchVal,
        Msg
    );
match_value_on_msg(Prop, MsgVal, <<"str">>, ReqPattern, MatchVal, _Msg) ->
    case re:run(MsgVal, ReqPattern) of
        {match, _} ->
            true;
        _ ->
            {failed,
                jstr(
                    "Prop '~p':~n~nNot matched.~n~nMat~n~n'~p'~n~nVal:~n~n'~p'~n~n",
                    [Prop, MatchVal, MsgVal]
                )}
    end;
match_value_on_msg(_, _, _, _, _, _) ->
    unsupported.

%%
%%
eql_msg_op(Prop, SrcVal, <<"json">>, ReqVal, _Msg) ->
    DecodedVal = decode_json(ReqVal),
    case is_same(DecodedVal, SrcVal) of
        true ->
            true;
        _ ->
            %% TODO this can and will happend as decode_json uses
            %% TODO binary values and jsonata uses string/list values.
            %% TODO best to refer to specific values and do a string compare
            %% TODO is_same(..) converts binaries to lists for comparison.
            {failed,
                jstr(
                    "Prop '~p':~n~nExp:~n~n'~p'~n~nWas:~n~n'~p'~n~n",
                    [Prop, DecodedVal, SrcVal]
                )}
    end;
eql_msg_op(Prop, SrcVal, <<"str">>, ReqVal, _Msg) ->
    % replace/escape all specials so that a test case can use \r \t \n for
    % these things and it becomes visible what is desired. Ie. in the SrcVal
    % we escape these to their visible equivalents.
    case is_same(ReqVal, escape_specials(SrcVal)) of
        true ->
            true;
        _ ->
            {failed,
                jstr(
                    "Prop '~p':~n~nExp:~n~n'~p'~n~nWas:~n~n'~p'~n~n",
                    [Prop, ReqVal, SrcVal]
                )}
    end;
eql_msg_op(Prop, SrcVal, <<"bool">>, ReqVal, _Msg) ->
    case is_same(to_bool(ReqVal), to_bool(SrcVal)) of
        true ->
            true;
        _ ->
            {failed,
                jstr(
                    "Prop '~p':~n~nExp:~n~n'~p'~n~nWas:~n~n'~p'~n~n",
                    [Prop, ReqVal, SrcVal]
                )}
    end;
eql_msg_op(Prop, SrcVal, <<"msg">>, ReqProp, Msg) ->
    %% ReqProp not ReqVal because the value is actually a property name
    %% on the message object. That property then contains the required
    %% value.
    %%
    %% {
    %%     "t": "eql",
    %%     "p": "_msgid",
    %%     "pt": "msg",
    %%     "to": "originalmsgid",
    %%     "tot": "msg"
    %% }
    case get_prop({ok, ReqProp}, Msg) of
        {ok, ReqVal, _} ->
            case is_same(ReqVal, SrcVal) of
                true ->
                    true;
                _ ->
                    {failed,
                        jstr(
                            "Prop '~p':~n~nExp:~n~n'~p'~n~nWas:~n~n'~p'~n~n",
                            [Prop, ReqVal, SrcVal]
                        )}
            end;
        _ ->
            {failed, jstr("Prop not set on msg: '~p': ~p", [ReqProp, Msg])}
    end;
eql_msg_op(Prop, SrcVal, <<"num">>, ReqVal, _Msg) ->
    %% "t": "eql",
    %% "p": "payload",
    %% "pt": "msg",
    %% "to": "8",
    %% "tot": "num"
    case convert_to_num(SrcVal) of
        {error, _} ->
            {failed,
                jstr(
                    "Prop val '~p' was not a num: ~p",
                    [Prop, SrcVal]
                )};
        SrcNum ->
            case convert_to_num(ReqVal) of
                {error, _} ->
                    {failed,
                        jstr(
                            "Required val for ~p was not a num: ~p",
                            [Prop, ReqVal]
                        )};
                ReqNum ->
                    case ReqNum == SrcNum of
                        true ->
                            true;
                        _ ->
                            {failed,
                                jstr(
                                    "Values not equal for ~p Exp: ~p != Was: ~p",
                                    [Prop, ReqNum, SrcNum]
                                )}
                    end
            end
    end;
eql_msg_op(_, _, _, _, _) ->
    unsupported.

%%
%%
check_rules(Rules, NodeDef, Msg) ->
    check_rules(Rules, NodeDef, Msg, 0, []).

check_rules([], NodeDef, Msg, 0, _Failures) ->
    node_status(ws_from(Msg), NodeDef, <<"assert succeed">>, "green", "ring"),
    maps:put(
        <<"assert_succeed">>,
        true,
        maps:remove(<<"assert_failures">>, Msg)
    );
check_rules([], NodeDef, Msg, FCnt, Failures) ->
    ErrMsg = jstr("~p check(s) failed", [FCnt]),
    node_status(ws_from(Msg), NodeDef, ErrMsg, "red", "dot"),
    maps:put(
        <<"assert_succeed">>,
        false,
        maps:put(<<"assert_failures">>, Failures, Msg)
    );
check_rules([H | T], NodeDef, Msg, FCnt, Failures) ->
    {ok, Op} = maps:find(<<"t">>, H),
    {ok, Pt} = maps:find(<<"pt">>, H),

    case check_rule_against_msg(Op, Pt, H, Msg) of
        true ->
            check_rules(T, NodeDef, Msg, FCnt, Failures);
        unsupported ->
            ErrMsg = jstr(
                "Assert values: unsupported Rule: '~p'",
                [H]
            ),
            %%
            %% Unlike other nodes, this is an assertion failure. Can't
            %% be silently ignoring tests.
            assert_failure(NodeDef, ws_from(Msg), ErrMsg),
            check_rules(
                T,
                NodeDef,
                Msg,
                FCnt + 1,
                [{unsupported, H} | Failures]
            );
        {failed, ErrMsg} ->
            this_should_not_happen(
                NodeDef,
                io_lib:format("~p ~p\n", [ErrMsg, Msg])
            ),
            debug(ws_from(Msg), debug_data(NodeDef, ErrMsg), error),
            check_rules(
                T,
                NodeDef,
                Msg,
                FCnt + 1,
                [{failure, H} | Failures]
            )
    end.

%%
%%
handle_event({stop, WsName}, NodeDef) ->
    case maps:find('_mc_incoming', NodeDef) of
        {ok, 0} ->
            {IdStr, TypeStr} = ?NODE_ID_AND_TYPE(NodeDef),

            this_should_not_happen(
                NodeDef,
                io_lib:format(
                    "Assert Values Error: Node was not reached [~p](~p)\n",
                    [TypeStr, IdStr]
                )
            ),

            D = ?BASE_DATA,

            Data = D#{
                <<"msg">> => <<"Assert Values Not Reached">>,
                <<"format">> => <<"string">>
            },

            debug(WsName, Data, error),
            node_status(WsName, NodeDef, "assert failed", "red", "dot");
        _ ->
            ok
    end,
    NodeDef;
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    case maps:find(<<"rules">>, NodeDef) of
        {ok, Ary} ->
            Msg2 = check_rules(Ary, NodeDef, Msg),
            send_msg_to_connected_nodes(NodeDef, Msg2),
            {handled, NodeDef, Msg2};
        _ ->
            send_msg_to_connected_nodes(NodeDef, Msg),
            {handled, NodeDef, Msg}
    end;
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
