-module(node_assert_values).

-export([node_assert_values/1]).
-export([handle_incoming/2]).

is_same(Same,Same) -> true;
is_same(_,_) -> false.

debug_data(NodeDef,ErrMsg) ->
    IdStr       = nodes:get_prop_value_from_map(id,NodeDef),
    ZStr        = nodes:get_prop_value_from_map(z,NodeDef),
    NameStr     = nodes:get_prop_value_from_map(name,NodeDef,
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
check_rule_against_msg(<<"notset">>,<<"msg">>,Rule,Msg) ->
    {ok, Prop} = maps:find(p,Rule),

    case maps:find(binary_to_atom(Prop),Msg) of
        {ok,_} ->
            {failed, nodes:jstr("Prop '~s' should not be set on Msg: ~p",[Prop,Msg])};
        _ ->
            true
    end;

check_rule_against_msg(<<"set">>,<<"msg">>,Rule,Msg) ->
    {ok, Prop} = maps:find(p,Rule),

    case maps:find(binary_to_atom(Prop),Msg) of
        {ok,_} ->
            true;
        _ ->
            {failed, nodes:jstr("Prop '~s' not set on Msg: ~p",[Prop,Msg])}
    end;

check_rule_against_msg(<<"noteql">>,<<"msg">>,Rule,Msg) ->
    {ok, Prop} = maps:find(p,Rule),

    case maps:find(binary_to_atom(Prop),Msg) of
        {ok,Val} ->
            {ok,ReqVal} = maps:find(to,Rule),
            case is_same(ReqVal,Val) of
                true ->
                    {failed, nodes:jstr(
                               "Prop '~s': Unequal but same. Exp: '~s' Was: '~s'",
                               [Prop,ReqVal,Val])};
                _ ->
                    true
            end;
        _ ->
            {failed, nodes:jstr("Prop not set on msg: '~p'",[Prop])}
    end;

%% eql operator on the msg - about the only thing that is
%% supported at the time of writing this comment.
check_rule_against_msg(<<"eql">>,<<"msg">>,Rule,Msg) ->
    {ok, Prop} = maps:find(p,Rule),

    case maps:find(binary_to_atom(Prop),Msg) of
        {ok,Val} ->
            {ok,ReqVal} = maps:find(to,Rule),
            case is_same(ReqVal,Val) of
                true ->
                    true;
                _ ->
                    {failed, nodes:jstr("Prop '~s': Exp: '~s' Was: '~s'",[Prop,ReqVal,Val])}
            end;
        _ ->
            {failed, nodes:jstr("Prop not set on msg: '~p'",[Prop])}
    end;

check_rule_against_msg(_Operator,_ObjectType,_,_) ->
    unsupported.

%%
%%
check_rules([],NodeDef,_,0) ->
    nodered:node_status(NodeDef, <<"All checks correct">>, "green", "dot");

check_rules([],NodeDef,_,FCnt) ->
    ErrMsg = nodes:jstr("~p check(s) failed",[FCnt]),
    nodered:node_status(NodeDef, ErrMsg, "red", "dot");

check_rules([H|T],NodeDef,Msg,FCnt) ->
    {ok, Op} = maps:find(t,H),
    {ok, Pt} = maps:find(pt,H),

    case check_rule_against_msg(Op,Pt,H,Msg) of
        true ->
            check_rules(T,NodeDef,Msg,FCnt);

        unsupported ->
            ErrMsg = nodes:jstr("Assert values: unsupported operator: '~s'",[Op]),
            nodered:debug(debug_data(NodeDef, ErrMsg), notice),
            check_rules(T,NodeDef,Msg,FCnt);

        {failed,ErrMsg} ->
            nodes:this_should_not_happen(
              NodeDef,
              io_lib:format("~p ~p\n",[ErrMsg,Msg])
            ),
            nodered:debug(debug_data(NodeDef,ErrMsg), error),
            check_rules(T,NodeDef,Msg,FCnt + 1)
    end.


handle_incoming(NodeDef,Msg) ->
    case maps:find(rules,NodeDef) of
        {ok, Ary} ->
            check_rules(Ary,NodeDef,Msg,0);
        _ ->
            ignore
    end,
    nodes:send_msg_to_connected_nodes(NodeDef,Msg),
    NodeDef.


node_assert_values(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE, NodeDef, only_incoming).
