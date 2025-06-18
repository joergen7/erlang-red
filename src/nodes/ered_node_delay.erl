-module(ered_node_delay).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Delay pauses the travels of a message by XX units of time.
%%
%%
%% "type": "delay",
%% "pauseType": "random",
%% "timeout": "2",
%% "timeoutUnits": "seconds",
%% "rate": "1",
%% "nbRateUnits": "1",
%% "rateUnits": "second",
%% "randomFirst": "1",
%% "randomLast": "5",
%% "randomUnits": "seconds",
%% "drop": false,
%% "allowrate": false,
%%

-import(ered_nodered_comm, [
    node_status/5,
    unsupported/3,
    ws_from/1
]).
-import(ered_nodes, [
    jstr/2,
    send_msg_to_connected_nodes/2
]).
-import(ered_messages, [
    convert_to_num/1,
    convert_units_to_milliseconds/2
]).

-define(CNTSTATUS(CNT), node_status(ws_from(Msg), NodeDef, CNT, "blue", "dot")).
-define(TO_MILLIS(FName, TUnits),
    convert_units_to_milliseconds(
        maps:find(TUnits, NodeDef),
        maps:find(FName, NodeDef)
    )
).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%% this must return a millisecond value.
compute_pause({ok, <<"delay">>}, NodeDef, Msg) ->
    ConversionResult = ?TO_MILLIS(<<"timeout">>, <<"timeoutUnits">>),
    case ConversionResult of
        {ok, V} ->
            V;
        {error, ErrMsg} ->
            unsupported(NodeDef, Msg, ErrMsg),
            0
    end;
compute_pause({ok, <<"delayv">>}, NodeDef, Msg) ->
    case maps:find(<<"delay">>, Msg) of
        {ok, V} ->
            %% msg.delay is assumed to be milliseconds --> RTFM.
            convert_to_num(V);
        _ ->
            %% if the delay isn't set, then revert to the node configuration
            compute_pause({ok, <<"delay">>}, NodeDef, Msg)
    end;
compute_pause({ok, <<"random">>}, NodeDef, Msg) ->
    Val1 = ?TO_MILLIS(<<"randomFirst">>, <<"randomUnits">>),
    Val2 = ?TO_MILLIS(<<"randomLast">>, <<"randomUnits">>),

    case {Val1, Val2} of
        {{ok, V1}, {ok, V2}} ->
            delay_random(V1, V2);
        _ ->
            unsupported(NodeDef, Msg, "not supported"),
            0
    end;
compute_pause(PType, NodeDef, Msg) ->
    unsupported(NodeDef, Msg, jstr("PauseType: '~p'", [PType])),
    0.

%%
%%
handle_event({registered, _WsName, _Pid}, NodeDef) ->
    maps:put('_delay_counter', 0, NodeDef);
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg({delay_push_out, Msg}, NodeDef) ->
    % Push message out through the delay node to maintain the delay
    % counter even though it might well delay the message since its
    % using the same message queue as incoming messages.
    send_msg_to_connected_nodes(NodeDef, Msg),
    DelayCnt = maps:get('_delay_counter', NodeDef) - 1,
    ?CNTSTATUS(integer_to_binary(DelayCnt)),
    {handled, maps:put('_delay_counter', DelayCnt, NodeDef), Msg};
handle_msg({incoming, Msg}, NodeDef) ->
    DelayCnt = maps:get('_delay_counter', NodeDef) + 1,
    PauseFor = compute_pause(maps:find(<<"pauseType">>, NodeDef), NodeDef, Msg),
    NodePid = self(),
    spawn(fun() -> delay_sending(Msg, PauseFor, NodePid) end),
    ?CNTSTATUS(integer_to_binary(DelayCnt)),

    {handled, maps:put('_delay_counter', DelayCnt, NodeDef),
        dont_send_complete_msg};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%
delay_sending(Msg, PauseFor, NodePid) ->
    timer:sleep(PauseFor),
    gen_server:cast(NodePid, {delay_push_out, Msg}).

%%
%%
delay_random(V1, V2) when V1 > V2 ->
    rand:uniform(V1 - V2);
delay_random(V1, V2) when V2 > V1 ->
    rand:uniform(V2 - V1);
delay_random(_V1, _V2) ->
    0.
