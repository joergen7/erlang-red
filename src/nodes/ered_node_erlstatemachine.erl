-module(ered_node_erlstatemachine).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Implement the gen_statem behaviour.
%%
%% This is a basic implementation likely to change. I did just enough to
%% demonstrate the possibilities of using a state machine inside Erlang-Red.
%%
%%

-import(ered_nodes, [
    send_msg_to_connected_nodes/2
]).

-import(ered_nodered_comm, [
    node_status/5,
    node_status_clear/2,
    ws_from/1
]).

-import(ered_msg_handling, [
    to_bool/1
]).

-define(SEND_MSG(NodeDef, Msg, Result, Action, CurrS, PrevS),
    Msg2 = Msg#{
        <<"payload">> => Result,
        <<"states">> => [PrevS, CurrS],
        <<"action">> => Action
    },
    send_msg_to_connected_nodes(NodeDef, Msg2),
    {handled, NodeDef, Msg2}
).

%%
%% Since the emit on state change option is a node config, it can't be
%% changed, so we can set the function to handle message sending as start
%% time, this won't change during runtime.
%%
%% Doing this saves doing an extra if on each message: if emit on state
%% change is true then ....
start(NodeDef, _WsName) ->
    ered_node:start(
        NodeDef#{
            '_func_send_msg' =>
                case to_bool(maps:get(<<"emit_on_state_change">>, NodeDef)) of
                    true ->
                        fun send_message_on_state_change/6;
                    false ->
                        fun always_send_message/6
                end
        },
        ?MODULE
    ).

%%
%%
handle_event({registered, WsName, _MyPid}, NodeDef) ->
    ModuleName = binary_to_atom(maps:get(<<"module_name">>, NodeDef)),

    {ok, {Pid, _Ref}} = gen_statem:start_monitor(ModuleName, [], []),

    %% State is always: {State, Data}
    %%  --> https://github.com/erlang/otp/blob/1a0e382d3dc14a356cffc7d791813d0b7601c721/lib/stdlib/src/gen_statem.erl#L3742
    %% where State is the state atom and Data is whatever data is associated
    %% with the current state.
    node_status(WsName, NodeDef, element(1, sys:get_state(Pid)), "blue", "dot"),

    maps:put('_statem_pid', Pid, NodeDef);
handle_event({'DOWN', _Ref, process, _Pid, shutdown}, NodeDef) ->
    maps:remove('_statem_pid', NodeDef);
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    Pid = maps:get('_statem_pid', NodeDef),
    SendMsgFunc = maps:get('_func_send_msg', NodeDef),
    WsName = ws_from(Msg),

    PrevState = element(1, sys:get_state(Pid)),
    #{<<"payload">> := Action} = Msg,
    Result = gen_statem:call(Pid, binary_to_atom(Action)),
    CurrState = element(1, sys:get_state(Pid)),

    node_status(WsName, NodeDef, CurrState, "blue", "dot"),
    SendMsgFunc(NodeDef, Msg, Result, Action, CurrState, PrevState);
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%
send_message_on_state_change(NodeDef, Msg, Result, Action, CurrS, PrevS) ->
    case CurrS =:= PrevS of
        true ->
            {handled, NodeDef, dont_send_complete_msg};
        _ ->
            ?SEND_MSG(NodeDef, Msg, Result, Action, CurrS, PrevS)
    end.

always_send_message(NodeDef, Msg, Result, Action, CurrS, PrevS) ->
    ?SEND_MSG(NodeDef, Msg, Result, Action, CurrS, PrevS).
