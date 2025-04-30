-module(ered_node_change).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Change node modifies the contents of messages but also sets values in
%% the flow and global contexts. It has many possible features and this
%% implementation only implements a small subset
%%

-import(ered_nodered_comm, [
    unsupported/3
]).
-import(ered_nodes, [
    jstr/2,
    send_msg_to_connected_nodes/2
]).
-import(ered_msg_handling, [
    convert_to_num/1,
    decode_json/1,
    get_prop/2,
    timestamp/0
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

do_move({ok, <<"msg">>}, {ok, <<"msg">>}, {ok, FromProp}, {ok, ToProp}, Msg, _) ->
    case maps:find(binary_to_atom(FromProp), Msg) of
        {ok, Val} ->
            maps:put(
                binary_to_atom(ToProp),
                Val,
                maps:remove(binary_to_atom(FromProp), Msg)
            );
        _ ->
            Msg
    end;
do_move(A, B, C, D, Msg, NodeDef) ->
    unsupported(
        NodeDef,
        Msg,
        jstr("move rule: ~p, ~p, ~p, ~p", [A, B, C, D])
    ),
    Msg.

%%
%% first type check ...
do_change({ok, <<"msg">>}, {ok, <<"str">>}, {ok, <<"str">>}, Rule, Msg, _) ->
    do_change_str(
        maps:find(p, Rule),
        maps:find(from, Rule),
        maps:find(to, Rule),
        Msg
    );
do_change(A, B, C, _, Msg, NodeDef) ->
    unsupported(
        NodeDef,
        Msg,
        jstr("change rule: ~p, ~p, ~p", [A, B, C])
    ),
    Msg.

%%
%%
do_change_str({ok, Prop}, {ok, FromStr}, {ok, ToStr}, Msg) ->
    case maps:find(binary_to_atom(Prop), Msg) of
        {ok, Val} ->
            maps:put(
                binary_to_atom(Prop),
                list_to_binary(
                    lists:flatten(
                        string:replace(
                            binary_to_list(Val),
                            binary_to_list(FromStr),
                            binary_to_list(ToStr),
                            all
                        )
                    )
                ),
                Msg
            );
        _ ->
            Msg
    end.

%%
%%
do_set_value(Prop, Value, <<"num">>, Msg, _NodeDef) ->
    maps:put(binary_to_atom(Prop), convert_to_num(Value), Msg);
do_set_value(Prop, _Value, <<"date">>, Msg, _NodeDef) ->
    maps:put(binary_to_atom(Prop), timestamp(), Msg);
do_set_value(Prop, Value, <<"str">>, Msg, _NodeDef) ->
    maps:put(binary_to_atom(Prop), Value, Msg);
do_set_value(Prop, Value, <<"json">>, Msg, _NodeDef) ->
    maps:put(binary_to_atom(Prop), decode_json(Value), Msg);
do_set_value(Prop, Value, <<"msg">>, Msg, _NodeDef) ->
    %% set a propery on the message to the value of another
    %% property on the message
    case get_prop({ok, Value}, Msg) of
        {ok, Val, _} ->
            maps:put(binary_to_atom(Prop), Val, Msg);
        _ ->
            maps:put(binary_to_atom(Prop), <<>>, Msg)
    end;
do_set_value(Prop, Value, <<"jsonata">>, Msg, NodeDef) ->
    %% "t": "set",
    %% "p": "payload",
    %% "pt": "msg",
    %% "to": "$count($$.payload)",
    %% "tot": "jsonata"
    case Value == <<"$count($$.payload)">> of
        true ->
            case maps:find(payload, Msg) of
                {ok, Val} ->
                    case erlang:is_list(Val) of
                        true ->
                            maps:put(
                                binary_to_atom(Prop),
                                erlang:length(Val),
                                Msg
                            );
                        _ ->
                            unsupported(
                                NodeDef,
                                Msg,
                                jstr("Payload was not a list: ~p", [Val])
                            ),
                            Msg
                    end;
                _ ->
                    unsupported(
                        NodeDef,
                        Msg,
                        "Payload not set on Msg"
                    ),
                    Msg
            end;
        _ ->
            unsupported(
                NodeDef,
                Msg,
                jstr("jsonata term: ~p", [Value])
            ),
            Msg
    end;
do_set_value(_, _, Tot, Msg, NodeDef) ->
    unsupported(NodeDef, Msg, jstr("set ToT: ~p", [Tot])),
    Msg.

%%
%%
handle_rules([], Msg, _NodeDef) ->
    Msg;
handle_rules([Rule | MoreRules], Msg, NodeDef) ->
    {ok, RuleType} = maps:find(t, Rule),
    handle_rules(
        MoreRules,
        handle_rule(RuleType, Rule, Msg, NodeDef),
        NodeDef
    ).

handle_rule(<<"set">>, Rule, Msg, NodeDef) ->
    %%
    %% pt can be many things (flow,global,...) we only support
    %% msg - at least until this comment gets removed.
    %%
    case maps:find(pt, Rule) of
        {ok, <<"msg">>} ->
            {ok, Prop} = maps:find(p, Rule),
            {ok, Value} = maps:find(to, Rule),
            {ok, ToType} = maps:find(tot, Rule),
            do_set_value(Prop, Value, ToType, Msg, NodeDef);
        _ ->
            Msg
    end;
handle_rule(<<"delete">>, Rule, Msg, _NodeDef) ->
    case maps:find(pt, Rule) of
        {ok, <<"msg">>} ->
            {ok, Prop} = maps:find(p, Rule),
            io:format("Removing ~p from Msg\n", [Prop]),
            maps:remove(binary_to_atom(Prop), Msg);
        _ ->
            Msg
    end;
handle_rule(<<"move">>, Rule, Msg, NodeDef) ->
    io:format("Rule ~p~n", [Rule]),
    do_move(
        maps:find(pt, Rule),
        maps:find(tot, Rule),
        maps:find(p, Rule),
        maps:find(to, Rule),
        Msg,
        NodeDef
    );
handle_rule(<<"change">>, Rule, Msg, NodeDef) ->
    do_change(
        maps:find(pt, Rule),
        maps:find(fromt, Rule),
        maps:find(tot, Rule),
        Rule,
        Msg,
        NodeDef
    );
handle_rule(_, Rule, Msg, NodeDef) ->
    unsupported(NodeDef, Msg, jstr("Rule: ~p", [Rule])),
    Msg.

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

handle_incoming(NodeDef, Msg) ->
    {ok, Rules} = maps:find(rules, NodeDef),
    Msg2 = handle_rules(Rules, Msg, NodeDef),
    send_msg_to_connected_nodes(NodeDef, Msg2),
    {NodeDef, Msg2}.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    {NodeDef2, Msg2} = handle_incoming(NodeDef, Msg),
    {handled, NodeDef2, Msg2};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
