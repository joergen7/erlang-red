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
    post_exception_or_debug/3,
    unsupported/3,
    ws_from/1
]).

-import(ered_messages, [
    to_bool/1
]).

% erlfmt:ignore
-define(SEND_MSG(NodeDef, Msg, Result, Action, CurrS, PrevS),
    Msg2 = Msg#{
        <<"payload">> => Result,
        <<"_statem">> => #{
          <<"state_prev">> => PrevS,
          <<"state_curr">> => CurrS,
          <<"action">>     => Action
       }
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
start(NodeDef, WsName) ->
    ered_node:start(
        NodeDef#{
            '_func_send_msg' =>
                case to_bool(maps:get(<<"emit_on_state_change">>, NodeDef)) of
                    true ->
                        fun send_message_on_state_change/6;
                    false ->
                        fun always_send_message/6
                end,
            '_ws' => WsName
        },
        ?MODULE
    ).

%%
%%
handle_event({registered, WsName, _MyPid}, NodeDef) ->
    ModuleName = binary_to_atom(maps:get(<<"module_name">>, NodeDef)),
    case module_loaded(ModuleName) of
        false ->
            node_status(
                WsName,
                NodeDef,
                <<"module not available">>,
                "red",
                "dot"
            ),
            maps:remove('_statem_pid', NodeDef);
        _ ->
            {ok, {Pid, _Ref}} = gen_statem:start_monitor(ModuleName, [], []),

            %% State is always: {State, Data}
            %%  --> https://github.com/erlang/otp/blob/1a0e382d3dc14a356cffc7d791813d0b7601c721/lib/stdlib/src/gen_statem.erl#L3742
            %% where State is the state atom and Data is whatever data is
            %% associated with the current state.
            node_status(
                WsName,
                NodeDef,
                element(1, sys:get_state(Pid)),
                "blue",
                "dot"
            ),
            maps:put('_statem_pid', Pid, NodeDef)
    end;
handle_event({being_supervised, _WsName}, NodeDef) ->
    %% need this to obtain the exits when the supervisor kills this node
    %% this then triggers a killing of the state machine process
    process_flag(trap_exit, true),
    NodeDef;
handle_event(
    {stop, _WsName},
    #{
        '_statem_pid' := Pid
    } = NodeDef
) ->
    exit(Pid, normal),
    maps:remove('_statem_pid', NodeDef);
handle_event(
    {'EXIT', _From, Reason},
    #{
        '_statem_pid' := Pid,
        '_ws' := WsName,
        '_being_supervised' := true
    } = NodeDef
) ->
    node_status(WsName, NodeDef, "killed", "red", "ring"),
    exit(Pid, Reason),
    exit(self(), Reason),
    maps:remove('_statem_pid', NodeDef);
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
%% Incoming message with Payload and Action defined, and the state machine
%% process is up and running.
handle_msg(
    {incoming,
        #{
            <<"action">> := Action,
            <<"payload">> := Payload,
            '_ws' := WsName
        } = Msg},
    #{
        '_statem_pid' := Pid,
        '_func_send_msg' := SendMsgFunc
    } = NodeDef
) ->
    PrevState = element(1, sys:get_state(Pid)),
    Result = gen_statem:call(Pid, {Action, Payload}),
    CurrState = element(1, sys:get_state(Pid)),

    node_status(WsName, NodeDef, CurrState, "blue", "dot"),
    SendMsgFunc(NodeDef, Msg, Result, Action, CurrState, PrevState);
%% incoming message with only Action defined, and the state machine process
%% is up and runnning
handle_msg(
    {incoming,
        #{
            <<"action">> := Action,
            '_ws' := WsName
        } = Msg},
    #{
        '_statem_pid' := Pid,
        '_func_send_msg' := SendMsgFunc
    } = NodeDef
) ->
    PrevState = element(1, sys:get_state(Pid)),
    Result = gen_statem:call(Pid, Action),
    CurrState = element(1, sys:get_state(Pid)),

    node_status(WsName, NodeDef, CurrState, "blue", "dot"),
    SendMsgFunc(NodeDef, Msg, Result, Action, CurrState, PrevState);
%% Error situaion, no action defined for a statemachine that is running - this
%% shouldn't happen.
handle_msg(
    {incoming, Msg},
    NodeDef
) ->
    post_exception_or_debug(NodeDef, Msg, <<"no action to perform">>),
    {handled, NodeDef, dont_send_complete_msg};
%%
%%
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%% ------------------ Helpers
send_message_on_state_change(NodeDef, Msg, Result, Action, CurrS, PrevS) ->
    case CurrS =:= PrevS of
        true ->
            {handled, NodeDef, dont_send_complete_msg};
        _ ->
            ?SEND_MSG(NodeDef, Msg, Result, Action, CurrS, PrevS)
    end.

always_send_message(NodeDef, Msg, Result, Action, CurrS, PrevS) ->
    ?SEND_MSG(NodeDef, Msg, Result, Action, CurrS, PrevS).
