-module(ered_node_switch).

-export([node_switch/1]).
-export([handle_incoming/2]).

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

-import(ered_node_receivership, [enter_receivership/3]).
-import(ered_nodes, [
    send_msg_on/2
]).

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
does_rule_match_catchall([], _, _, _) ->
    ok;
does_rule_match_catchall([Rule | Rules], Val, [Wires | MoreWires], Msg) ->
    {ok, Op} = maps:find(t, Rule),
    {ok, OpVal} = maps:find(v, Rule),
    {ok, Type} = maps:find(vt, Rule),

    io:format("switch: in catch all ~p ~p ~p ~p\n", [Op, OpVal, Type, Val]),

    case does_rule_match(Op, Type, OpVal, Val) of
        true ->
            send_msg_on(Wires, Msg);
        _ ->
            ok
    end,
    does_rule_match_catchall(Rules, Val, MoreWires, Msg).

%%
%%
does_rule_match_stopafterone([], _, _, _) ->
    ok;
does_rule_match_stopafterone([Rule | Rules], Val, [Wires | MoreWires], Msg) ->
    {ok, Op} = maps:find(t, Rule),
    {ok, OpVal} = maps:find(v, Rule),
    {ok, Type} = maps:find(vt, Rule),

    io:format("switch: in first and stop ~p ~p ~p ~p\n", [Op, OpVal, Type, Val]),

    case does_rule_match(Op, Type, OpVal, Val) of
        true ->
            send_msg_on(Wires, Msg);
        _ ->
            does_rule_match_stopafterone(Rules, Val, MoreWires, Msg)
    end.

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
            does_rule_match_catchall(Rules, Val, Wires, Msg);
        {ok, true} ->
            does_rule_match_catchall(Rules, Val, Wires, Msg);
        _ ->
            does_rule_match_stopafterone(Rules, Val, Wires, Msg)
    end,

    NodeDef.

%%
%%
node_switch(NodeDef) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, only_incoming).
