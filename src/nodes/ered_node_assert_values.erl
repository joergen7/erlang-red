-module(ered_node_assert_values).

-export([node_assert_values/1]).
-export([handle_incoming/2]).

-import(node_receivership, [enter_receivership/3]).

is_same(Same, Same) -> true;
is_same(_, _) -> false.

%% erlfmt:ignore equals and arrows should line up here.
debug_data(NodeDef,ErrMsg) ->
    IdStr   = ered_nodes:get_prop_value_from_map(id,   NodeDef),
    ZStr    = ered_nodes:get_prop_value_from_map(z,    NodeDef),
    NameStr = ered_nodes:get_prop_value_from_map(name, NodeDef,
                                                 <<"Assert Values">>),

    #{
       id       => IdStr,
       z        => ZStr,
       '_alias' => IdStr,
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
                ered_nodes:jstr(
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
                ered_nodes:jstr(
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
                        ered_nodes:jstr(
                            "Prop '~p': Unequal but same. Exp: '~p' Was: '~p'",
                            [Prop, ReqVal, Val]
                        )};
                _ ->
                    true
            end;
        _ ->
            {failed, ered_nodes:jstr("Prop not set on msg: '~p'", [Prop])}
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
            {failed, ered_nodes:jstr("Prop not set on msg: '~p'", [Prop])}
    end;
check_rule_against_msg(_Operator, _ObjectType, _, _) ->
    unsupported.

eql_msg_op(Prop, SrcVal, <<"str">>, ReqVal, _Msg) ->
    case is_same(ReqVal, SrcVal) of
        true ->
            true;
        _ ->
            {failed,
                ered_nodes:jstr(
                    "Prop '~p': Exp: '~p' Was: '~p'",
                    [Prop, ReqVal, SrcVal]
                )}
    end;
%% {
%%     "t": "eql",
%%     "p": "_msgid",
%%     "pt": "msg",
%%     "to": "originalmsgid",
%%     "tot": "msg"
%% }
eql_msg_op(Prop, SrcVal, <<"msg">>, ReqProp, Msg) ->
    case maps:find(binary_to_atom(ReqProp), Msg) of
        {ok, ReqVal} ->
            case is_same(ReqVal, SrcVal) of
                true ->
                    true;
                _ ->
                    {failed,
                        ered_nodes:jstr(
                            "Prop '~p': Exp: '~p' Was: '~p'",
                            [Prop, ReqVal, SrcVal]
                        )}
            end;
        _ ->
            {failed, ered_nodes:jstr("Prop not set on msg: '~p'", [ReqProp])}
    end;
eql_msg_op(_, _, _, _, _) ->
    unsupported.

%%
%%
check_rules([], NodeDef, Msg, 0) ->
    nodered:node_status(
        nodered:ws(Msg),
        NodeDef,
        <<"All checks succeed">>,
        "green",
        "dot"
    );
check_rules([], NodeDef, Msg, FCnt) ->
    ErrMsg = ered_nodes:jstr("~p check(s) failed", [FCnt]),
    nodered:node_status(nodered:ws(Msg), NodeDef, ErrMsg, "red", "dot");
check_rules([H | T], NodeDef, Msg, FCnt) ->
    {ok, Op} = maps:find(t, H),
    {ok, Pt} = maps:find(pt, H),

    case check_rule_against_msg(Op, Pt, H, Msg) of
        true ->
            check_rules(T, NodeDef, Msg, FCnt);
        unsupported ->
            ErrMsg = ered_nodes:jstr(
                "Assert values: unsupported operator: '~p'",
                [Op]
            ),
            nodered:debug(nodered:ws(Msg), debug_data(NodeDef, ErrMsg), notice),
            check_rules(T, NodeDef, Msg, FCnt);
        {failed, ErrMsg} ->
            ered_nodes:this_should_not_happen(
                NodeDef,
                io_lib:format("~p ~p\n", [ErrMsg, Msg])
            ),
            nodered:debug(nodered:ws(Msg), debug_data(NodeDef, ErrMsg), error),
            check_rules(T, NodeDef, Msg, FCnt + 1)
    end.

handle_incoming(NodeDef, Msg) ->
    case maps:find(rules, NodeDef) of
        {ok, Ary} ->
            check_rules(Ary, NodeDef, Msg, 0);
        _ ->
            ignore
    end,
    ered_nodes:send_msg_to_connected_nodes(NodeDef, Msg),
    NodeDef.

node_assert_values(NodeDef) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, only_incoming).
