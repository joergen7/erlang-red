-module(ered_node_switch).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% TODO needs refactoring to use the unsupported/3 function to send off
%% TODO a notification of something missing - currently it just fails
%% TODO silently - not really helping anyone.
%%

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
    send_msg_on/2
]).
-import(ered_message_exchange, [
    post_completed/2
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

is_same(Same, Same) -> true;
is_same(_, _) -> false.

is_gt(A, B) when A > B -> true;
is_gt(_, _) -> false.

is_lt(A, B) when A < B -> true;
is_lt(_, _) -> false.

int_to_float(Val) ->
    case string:to_integer(Val) of
        {error, _} ->
            case is_integer(Val) of
                true ->
                    {Val, ok};
                _ ->
                    io:format("Unable to convert to num ~p\n", [Val]),
                    {0, error}
            end;
        {V, R} ->
            {V, R}
    end.

to_num(Val) ->
    case string:to_float(Val) of
        {error, _} ->
            int_to_float(Val);
        {V, R} ->
            {V, R}
    end.

to_num(V1, V2) ->
    {V1num, _} = to_num(V1),
    {V2num, _} = to_num(V2),
    {V1num, V2num}.

%%
%%
does_rule_match(<<"eq">>, <<"str">>, OpVal, MsgVal) ->
    is_same(OpVal, MsgVal);
does_rule_match(<<"eq">>, <<"num">>, OpVal, MsgVal) ->
    {Vop, Vmsg} = to_num(OpVal, MsgVal),
    is_same(Vmsg, Vop);
does_rule_match(<<"gt">>, _, OpVal, MsgVal) ->
    {Vop, Vmsg} = to_num(OpVal, MsgVal),
    is_gt(Vmsg, Vop);
does_rule_match(<<"lt">>, _, OpVal, MsgVal) ->
    {Vop, Vmsg} = to_num(OpVal, MsgVal),
    is_lt(Vmsg, Vop);
does_rule_match(Op, Type, _OpVal, _MsgVal) ->
    io:format("switch: unsupported operator or type: ~p ~p\n", [Op, Type]),
    false.

%%
%%
get_value_from_msg({ok, <<"msg">>}, {ok, PropName}, Msg) ->
    case maps:find(binary_to_atom(PropName), Msg) of
        {ok, Val} ->
            Val;
        _ ->
            io_lib:format("switch: property not found on msg: [~p] ~p", [
                PropName,
                Msg
            ])
    end;
get_value_from_msg({ok, PropType}, {ok, PropName}, _Msg) ->
    io_lib:format("switch: unsupported property: ~p.~p", [PropType, PropName]);
get_value_from_msg(_, _, _) ->
    io_lib:format("switch: property not found\n", []).

%%
%%
handle_check_all_rules([], _, _, _, _) ->
    ok;
handle_check_all_rules([Rule | Rules], Val, [Wires | MoreWires], NodeDef, Msg) ->
    {ok, Op} = maps:find(t, Rule),
    {ok, Type} = maps:find(vt, Rule),
    {ok, OpVal} = maps:find(v, Rule),

    case does_rule_match(Op, Type, OpVal, Val) of
        true ->
            %% ?? switch node does not generate complete message for the
            %% ?? complete node - make sense since the switch node is only
            %% ?? control not computation, i.e. it's direct the flow of data
            %% ?? but not directly altering data.
            %% post_completed(NodeDef, Msg),
            send_msg_on(Wires, Msg);
        _ ->
            ok
    end,
    handle_check_all_rules(Rules, Val, MoreWires, NodeDef, Msg).

%%
%%
handle_stop_after_one([], _, _, _, _) ->
    ok;
handle_stop_after_one([Rule | Rules], Val, [Wires | MoreWires], NodeDef, Msg) ->
    {ok, Op} = maps:find(t, Rule),
    {ok, Type} = maps:find(vt, Rule),
    {ok, OpVal} = maps:find(v, Rule),

    case does_rule_match(Op, Type, OpVal, Val) of
        true ->
            %% ?? switch node does not generate complete message for the
            %% ?? complete node - make sense since the switch node is only
            %% ?? control not computation, i.e. it's direct the flow of data
            %% ?? but not directly altering data.
            %% post_completed(NodeDef, Msg),
            send_msg_on(Wires, Msg);
        _ ->
            handle_stop_after_one(Rules, Val, MoreWires, NodeDef, Msg)
    end.

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%% Handler for incoming messages
%%
handle_incoming(NodeDef, Msg) ->
    {ok, Rules} = maps:find(rules, NodeDef),
    {ok, Wires} = maps:find(wires, NodeDef),

    Val = get_value_from_msg(
        maps:find(propertyType, NodeDef),
        maps:find(property, NodeDef),
        Msg
    ),

    case maps:find(checkall, NodeDef) of
        {ok, <<"true">>} ->
            handle_check_all_rules(Rules, Val, Wires, NodeDef, Msg);
        _ ->
            handle_stop_after_one(Rules, Val, Wires, NodeDef, Msg)
    end,

    {handled, NodeDef, dont_send_complete_msg}.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    handle_incoming(NodeDef, Msg);
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
