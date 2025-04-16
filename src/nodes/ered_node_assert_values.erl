-module(ered_node_assert_values).

-export([node_assert_values/1]).
-export([handle_incoming/2]).
-export([handle_stop/2]).

-import(ered_node_receivership, [enter_receivership/3]).
-import(nodered, [
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

is_same(Same, Same) -> true;
is_same(_, _) -> false.

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
    {ok, Prop} = maps:find(p, Rule),

    case maps:find(binary_to_atom(Prop), Msg) of
        {ok, _} ->
            {failed,
                jstr(
                    "Prop '~p' should not be set on Msg: ~p",
                    [Prop, Msg]
                )};
        _ ->
            true
    end;
check_rule_against_msg(<<"set">>, <<"msg">>, Rule, Msg) ->
    {ok, Prop} = maps:find(p, Rule),

    case maps:find(binary_to_atom(Prop), Msg) of
        {ok, _} ->
            true;
        _ ->
            {failed,
                jstr(
                    "Prop '~p' not set on Msg: ~p",
                    [Prop, Msg]
                )}
    end;
check_rule_against_msg(<<"noteql">>, <<"msg">>, Rule, Msg) ->
    {ok, Prop} = maps:find(p, Rule),

    case maps:find(binary_to_atom(Prop), Msg) of
        {ok, Val} ->
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
        _ ->
            {failed, jstr("Prop not set on msg: '~p'", [Prop])}
    end;
%% eql operator on the msg - about the only thing that is
%% supported at the time of writing this comment.
check_rule_against_msg(<<"eql">>, <<"msg">>, Rule, Msg) ->
    {ok, Prop} = maps:find(p, Rule),
    {ok, ToType} = maps:find(tot, Rule),
    {ok, ReqVal} = maps:find(to, Rule),

    case maps:find(binary_to_atom(Prop), Msg) of
        {ok, Val} ->
            eql_msg_op(Prop, Val, ToType, ReqVal, Msg);
        _ ->
            {failed, jstr("Prop not set on msg: '~p'", [Prop])}
    end;
check_rule_against_msg(<<"mth">>, <<"msg">>, Rule, Msg) ->
    {ok, Prop} = maps:find(p, Rule),
    {ok, ToType} = maps:find(tot, Rule),
    {ok, ReqVal} = maps:find(to, Rule),

    case re:compile(ReqVal) of
        {ok, ReqPattern} ->
            mth_msg_op(Prop, ToType, ReqPattern, ReqVal, Msg);
        _ ->
            {failed, jstr("Match not RegExp: '~p'", [ReqVal])}
    end;
check_rule_against_msg(_Operator, _ObjectType, _, _) ->
    unsupported.

%%
%%
mth_msg_op(Prop, <<"str">>, ReqPattern, MatchVal, Msg) ->
    case maps:find(binary_to_atom(Prop), Msg) of
        {ok, ReqVal} ->
            case re:run(ReqVal, ReqPattern) of
                {match, _} ->
                    true;
                _ ->
                    {failed,
                        jstr(
                            "Prop '~p': Not matched. Mat '~p' Val: '~p'",
                            [Prop, MatchVal, ReqVal]
                        )}
            end;
        _ ->
            {failed, jstr("Propery not set on Msg: '~p'", [Prop])}
    end;
mth_msg_op(_, _, _, _, _) ->
    unsupported.

%%
%%
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
                            "Required val was not a num: ~p",
                            [ReqVal]
                        )};
                ReqNum ->
                    case ReqNum == SrcNum of
                        true ->
                            true;
                        _ ->
                            {failed,
                                jstr(
                                    "Values not equal Exp: [~p] != Was: [~p]",
                                    [ReqNum, SrcNum]
                                )}
                    end
            end
    end;
eql_msg_op(_, _, _, _, _) ->
    unsupported.

%%
%%
%% TODO This util and those defined in the switch node should be put
%% TODO into a centralised utility module.
convert_to_num(Val) when is_integer(Val) ->
    Val;
convert_to_num(Val) when is_float(Val) ->
    Val;
convert_to_num(Val) ->
    case string:to_float(Val) of
        {error, _} ->
            case string:to_integer(Val) of
                {error, _} ->
                    {error, "no conversion possible"};
                {V, _} ->
                    V
            end;
        {V, _} ->
            V
    end.

%%
%%
check_rules([], NodeDef, Msg, 0) ->
    node_status(
        ws_from(Msg),
        NodeDef,
        <<"All checks succeed">>,
        "green",
        "dot"
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
            debug(ws_from(Msg), debug_data(NodeDef, ErrMsg), notice),
            check_rules(T, NodeDef, Msg, FCnt);
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
%% erlfmt:ignore equals and arrows should line up here.
handle_stop(NodeDef,WsName) ->
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
    end.

handle_incoming(NodeDef, Msg) ->
    case maps:find(rules, NodeDef) of
        {ok, Ary} ->
            check_rules(Ary, NodeDef, Msg, 0);
        _ ->
            ignore
    end,
    send_msg_to_connected_nodes(NodeDef, Msg),
    NodeDef.

node_assert_values(NodeDef) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, stop_and_incoming).
