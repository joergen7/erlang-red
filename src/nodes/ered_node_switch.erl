-module(ered_node_switch).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% representation of a switch node.
%%
%% Switch nodes represent control flow within Node-RED and are central to
%% creating complex flows.
%%
%% Basic working of a switch node is that it has a collection of rules that are
%% tested for truthfulness. If true, then then message is sent to all wires
%% on the port (Node-RED *port* is the point of connection for wires) for that
%% rule. A switch can either test all rules or stop after the first true rule,
%% that is specified by the "checkall" attribute.
%%

-import(ered_nodes, [
    jstr/2,
    post_exception_or_debug/3,
    send_msg_on/2
]).
-import(ered_message_exchange, [
    post_completed/2
]).
-import(ered_msg_handling, [
    convert_to_num/1,
    get_prop/2,
    is_same/2
]).

-import(ered_nodered_comm, [
    unsupported/3
]).
%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
obtain_operator_value(<<"jsonata">>, OpVal, Msg) ->
    case jsonata_evaluator:execute(OpVal, Msg) of
        {ok, Result} ->
            {ok, Result};
        {error, Error} ->
            {error, jstr("jsonata term: ~p", [Error])};
        {exception, ErrMsg} ->
            {exception, ErrMsg}
    end;
obtain_operator_value(<<"num">>, OpVal, _Msg) ->
    {ok, convert_to_num(OpVal)};
obtain_operator_value(<<"str">>, OpVal, _Msg) ->
    {ok, OpVal};
obtain_operator_value(OpType, _OpVal, _Msg) ->
    {unsupported, jstr("unsupported operator type: ~p", [OpType])}.

%%
%%
does_rule_match(<<"eq">>, OpCompVal, MsgVal) ->
    is_same(OpCompVal, MsgVal);
does_rule_match(<<"gt">>, OpCompVal, MsgVal) ->
    MsgVal > OpCompVal;
does_rule_match(<<"lt">>, OpCompVal, MsgVal) ->
    MsgVal < OpCompVal;
does_rule_match(Op, _, _) ->
    {unsupported, jstr("unsupported operator ~p", [Op])}.

does_rule_match(Op, Type, OpVal, MsgVal, NodeDef, Msg) ->
    case obtain_operator_value(Type, OpVal, Msg) of
        {ok, OpCompVal} ->
            case does_rule_match(Op, OpCompVal, MsgVal) of
                {unsupported, ErrMsg} ->
                    unsupported(NodeDef, Msg, ErrMsg);
                V ->
                    V
            end;
        {error, ErrMsg} ->
            post_exception_or_debug(NodeDef, Msg, ErrMsg),
            false;
        {unsupported, ErrMsg} ->
            unsupported(NodeDef, Msg, ErrMsg),
            false;
        {exception, ErrMsg} ->
            post_exception_or_debug(NodeDef, Msg, ErrMsg),
            false
    end.

%%
%% HasMatch is a indicated whether a match has been made. Used for the
%% else operator.
handle_check_all_rules([], _, _, _, _, _) ->
    ok;
handle_check_all_rules(
    [Rule | Rules], Val, [Wires | MoreWires], NodeDef, Msg, HadMatch
) ->
    {ok, Op} = maps:find(t, Rule),

    %% else operator has no vt nor v values.
    case {Op, HadMatch} of
        {<<"else">>, false} ->
            %% Interesting, interesting - the "otherwise" rule only matches
            %% in the "check all rules" if nothing else matched - mimicing the
            %% behaviour of the stop-after-one mode. I personally would have
            %% thought it would match **all** the time, regardless.
            %% But "otherwise" means something different to "always".
            %% See test flow "ce2f98273da05245" for more details.
            %% Turns other otherwise is context specific: if it is reached
            %% and nothing has matched, it matches. Is it reached and something
            %% has matched, then it doesn't match.
            send_msg_on(Wires, Msg),
            handle_check_all_rules(Rules, Val, MoreWires, NodeDef, Msg, true);
        {<<"else">>, true} ->
            handle_check_all_rules(Rules, Val, MoreWires, NodeDef, Msg, true);
        _ ->
            {ok, Type} = maps:find(vt, Rule),
            {ok, OpVal} = maps:find(v, Rule),

            case does_rule_match(Op, Type, OpVal, Val, NodeDef, Msg) of
                true ->
                    %% ?? switch node does not generate complete message for the
                    %% ?? complete node - make sense since the switch node is only
                    %% ?? control not computation, i.e. it's direct the flow of data
                    %% ?? but not directly altering data.
                    %% post_completed(NodeDef, Msg),
                    send_msg_on(Wires, Msg),
                    handle_check_all_rules(
                        Rules, Val, MoreWires, NodeDef, Msg, true
                    );
                _ ->
                    handle_check_all_rules(
                        Rules, Val, MoreWires, NodeDef, Msg, HadMatch
                    )
            end
    end.

%%
%%
handle_stop_after_one([], _, _, _, _) ->
    ok;
handle_stop_after_one([Rule | Rules], Val, [Wires | MoreWires], NodeDef, Msg) ->
    {ok, Op} = maps:find(t, Rule),

    case Op of
        %% else operator has no vt nor v values.
        <<"else">> ->
            send_msg_on(Wires, Msg);
        _ ->
            {ok, Type} = maps:find(vt, Rule),
            {ok, OpVal} = maps:find(v, Rule),

            case does_rule_match(Op, Type, OpVal, Val, NodeDef, Msg) of
                true ->
                    %% ?? switch node does not generate complete message for the
                    %% ?? complete node - make sense since the switch node is only
                    %% ?? control not computation, i.e. it's direct the flow of data
                    %% ?? but not directly altering data.
                    %% post_completed(NodeDef, Msg),
                    send_msg_on(Wires, Msg);
                _ ->
                    handle_stop_after_one(Rules, Val, MoreWires, NodeDef, Msg)
            end
    end.

%%
%% This function retrieves the value to which the rules are compared to.
%% This value can come from the (1) Msg object or the (2) flow context or the
%% (3) global context or as a (4) jsonata expression or the (5) environment
%% variable.
obtain_compare_to_value({ok, <<"msg">>}, {ok, PropName}, Msg) ->
    case get_prop({ok, PropName}, Msg) of
        {ok, Val, _} ->
            {ok, Val};
        _ ->
            {error, jstr("property not found on msg: [~p]", [PropName])}
    end;
obtain_compare_to_value({ok, <<"jsonata">>}, {ok, PropName}, Msg) ->
    case jsonata_evaluator:execute(PropName, Msg) of
        {ok, Result} ->
            {ok, Result};
        {error, Error} ->
            {error, jstr("jsonata term: ~p", [Error])};
        {exception, ErrMsg} ->
            {exception, ErrMsg}
    end;
obtain_compare_to_value({ok, PropType}, {ok, PropName}, _Msg) ->
    {unsupported,
        jstr("unsupported property: ~p with type ~p", [PropName, PropType])};
obtain_compare_to_value(Err1, Err2, _Msg) ->
    {error, jstr("error finding property Err1: ~p Err2: ~p", [Err1, Err2])}.

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%% Handler for incoming messages
%%
handle_msg({incoming, Msg}, NodeDef) ->
    %% flag unsupported features - recreate message sequence whatever that does
    case maps:find(repair, NodeDef) of
        {ok, true} ->
            unsupported(NodeDef, Msg, "recreate message sequence");
        _ ->
            ignore
    end,

    {ok, Rules} = maps:find(rules, NodeDef),
    {ok, Wires} = maps:find(wires, NodeDef),

    case
        obtain_compare_to_value(
            maps:find(propertyType, NodeDef),
            maps:find(property, NodeDef),
            Msg
        )
    of
        {ok, Val} ->
            case maps:find(checkall, NodeDef) of
                {ok, <<"true">>} ->
                    %% last flag indicates that nothing has yet matched -
                    %% required for the otherwise ('else') operator.
                    handle_check_all_rules(
                        Rules,
                        Val,
                        Wires,
                        NodeDef,
                        Msg,
                        false
                    );
                _ ->
                    handle_stop_after_one(Rules, Val, Wires, NodeDef, Msg)
            end;
        {error, ErrMsg} ->
            post_exception_or_debug(NodeDef, Msg, ErrMsg);
        {unsupported, ErrMsg} ->
            unsupported(NodeDef, Msg, ErrMsg);
        {exception, ErrMsg} ->
            post_exception_or_debug(NodeDef, Msg, ErrMsg)
    end,

    {handled, NodeDef, dont_send_complete_msg};
%%
%%
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
