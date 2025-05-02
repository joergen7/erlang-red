-module(ered_node_assert_values).

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
-import(ered_msg_handling, [
    convert_to_num/1,
    decode_json/1,
    get_prop/2
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

is_same(Same, Diff) when is_list(Same) and is_binary(Diff) ->
    is_same(Same, binary_to_list(Diff));
is_same(Same, Diff) when is_binary(Same) and is_list(Diff) ->
    is_same(binary_to_list(Same), Diff);
is_same(Same, Same) ->
    true;
is_same(_, _) ->
    false.

%% erlfmt:ignore equals and arrows should line up here.
debug_data(NodeDef, ErrMsg) ->
    IdStr   = get_prop_value_from_map(id,   NodeDef),
    ZStr    = get_prop_value_from_map(z,    NodeDef),
    NameStr = get_prop_value_from_map(name, NodeDef, <<"Assert Values">>),

    #{
       id       => IdStr,
       z        => ZStr,
       path     => ZStr,
       name     => NameStr,
       msg      => ErrMsg,
       format   => <<"string">>
    }.

%%
%%
check_rule_against_msg(<<"notset">>, <<"msg">>, Rule, Msg) ->
    case get_prop(maps:find(p, Rule), Msg) of
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
    case get_prop(maps:find(p, Rule), Msg) of
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
    case get_prop(maps:find(p, Rule), Msg) of
        {ok, Val, Prop} ->
            {ok, ReqVal} = maps:find(to, Rule),
            case is_same(ReqVal, Val) of
                true ->
                    {failed,
                        jstr(
                            "Prop '~p': Unequal but same. Exp: '~p' Was: '~p'",
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
    {ok, Prop} = maps:find(p, Rule),
    {ok, ToType} = maps:find(tot, Rule),
    {ok, ReqVal} = maps:find(to, Rule),

    case get_prop(maps:find(p, Rule), Msg) of
        {ok, Val, Prop} ->
            eql_msg_op(Prop, Val, ToType, ReqVal, Msg);
        {undefined, Prop} ->
            {failed, jstr("Prop not set on msg: '~p'", [Prop])}
    end;
check_rule_against_msg(<<"mth">>, <<"msg">>, Rule, Msg) ->
    {ok, ToType} = maps:find(tot, Rule),
    {ok, ReqVal} = maps:find(to, Rule),

    case get_prop(maps:find(p, Rule), Msg) of
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
            {failed, jstr("Propery not set on Msg: '~p'", [Prop])}
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
                    "Prop '~p': Not matched. Mat '~p' Val: '~p'",
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
                    "Prop '~p': Exp: '~p' Was: '~p'",
                    [Prop, DecodedVal, SrcVal]
                )}
    end;
eql_msg_op(Prop, SrcVal, <<"str">>, ReqVal, _Msg) ->
    case is_same(ReqVal, SrcVal) of
        true ->
            true;
        _ ->
            {failed,
                jstr(
                    "Prop '~p': Exp: '~p' Was: '~p'",
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
    case maps:find(binary_to_atom(ReqProp), Msg) of
        {ok, ReqVal} ->
            case is_same(ReqVal, SrcVal) of
                true ->
                    true;
                _ ->
                    {failed,
                        jstr(
                            "Prop '~p': Exp: '~p' Was: '~p'",
                            [Prop, ReqVal, SrcVal]
                        )}
            end;
        _ ->
            {failed, jstr("Prop not set on msg: '~p'", [ReqProp])}
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
check_rules([], NodeDef, Msg, 0) ->
    node_status(
        ws_from(Msg),
        NodeDef,
        <<"assert succeed">>,
        "green",
        "ring"
    );
check_rules([], NodeDef, Msg, FCnt) ->
    ErrMsg = jstr("~p check(s) failed", [FCnt]),
    node_status(ws_from(Msg), NodeDef, ErrMsg, "red", "dot");
check_rules([H | T], NodeDef, Msg, FCnt) ->
    {ok, Op} = maps:find(t, H),
    {ok, Pt} = maps:find(pt, H),

    case check_rule_against_msg(Op, Pt, H, Msg) of
        true ->
            check_rules(T, NodeDef, Msg, FCnt);
        unsupported ->
            ErrMsg = jstr(
                "Assert values: unsupported Rule: '~p'",
                [H]
            ),
            %%
            %% Unlike other nodes, this is an assertion failure. Can't
            %% be silently ignoring tests.
            assert_failure(NodeDef, ws_from(Msg), ErrMsg),
            check_rules(T, NodeDef, Msg, FCnt + 1);
        {failed, ErrMsg} ->
            this_should_not_happen(
                NodeDef,
                io_lib:format("~p ~p\n", [ErrMsg, Msg])
            ),
            debug(ws_from(Msg), debug_data(NodeDef, ErrMsg), error),
            check_rules(T, NodeDef, Msg, FCnt + 1)
    end.

%%
%%
%% erlfmt:ignore stars are aligned
handle_event({stop, WsName}, NodeDef) ->
    case maps:find('_mc_incoming',NodeDef) of
        {ok,0} ->
            {ok, IdStr}   = maps:find(id,NodeDef),
            {ok, TypeStr} = maps:find(type,NodeDef),

            this_should_not_happen(
              NodeDef,
              io_lib:format(
                "Assert Values Error: Node was not reached [~p](~p)\n",
                [TypeStr,IdStr])
            ),

            IdStr   = get_prop_value_from_map(id,   NodeDef),
            ZStr    = get_prop_value_from_map(z,    NodeDef),
            NameStr = get_prop_value_from_map(name, NodeDef,
                                                         TypeStr),
            Data = #{
                     id       => IdStr,
                     z        => ZStr,
                     path     => ZStr,
                     name     => NameStr,
                     msg      => <<"Assert Values Not Reached">>,
                     format   => <<"string">>
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
handle_incoming(NodeDef, Msg) ->
    case maps:find(rules, NodeDef) of
        {ok, Ary} ->
            check_rules(Ary, NodeDef, Msg, 0);
        _ ->
            ignore
    end,
    send_msg_to_connected_nodes(NodeDef, Msg),
    {NodeDef, Msg}.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    {NodeDef2, Msg2} = handle_incoming(NodeDef, Msg),
    {handled, NodeDef2, Msg2};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
