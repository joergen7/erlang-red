-module(ered_node_erlstatemachine).

-behaviour(ered_node).

-include("ered_nodes.hrl").

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
start(NodeDef, WsName) ->
    ered_node:start(
        ?PUT_WS(NodeDef#{'_func_send_msg' => define_func_send_msg(NodeDef)}),
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
    %% this then triggers a killing of the state machine process - see EXIT
    %% event below.
    process_flag(trap_exit, true),
    NodeDef;
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
handle_event(
    {stop, _WsName},
    #{
        '_statem_pid' := Pid
    } = NodeDef
) ->
    exit(Pid, normal),
    maps:remove('_statem_pid', NodeDef);
%%
%% state machine shutdown. This generally does not happen since the
%% individual state handlers modules crash but they are isolated from
%% the statemachine process that we're monitoring.
handle_event(
    {'DOWN', _Ref, process, _Pid, Reason},
    #{
        '_being_supervised' := true
    } = NodeDef
) ->
    node_status(ws_from(NodeDef), NodeDef, "stopped", "red", "dot"),
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
    {PrevState, Result, CurrState} = statem_call(Pid, {Action, Payload}),
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
    {PrevState, Result, CurrState} = statem_call(Pid, Action),
    node_status(WsName, NodeDef, CurrState, "blue", "dot"),
    SendMsgFunc(NodeDef, Msg, Result, Action, CurrState, PrevState);
%% Error situaion, no action defined for a statemachine that is running - this
%% shouldn't happen.
handle_msg(
    {incoming, Msg},
    #{
        '_statem_pid' := _Pid
    } = NodeDef
) ->
    post_exception_or_debug(NodeDef, Msg, <<"no action to perform">>),
    {handled, NodeDef, dont_send_complete_msg};
%% Error situaion, statemachine process is dead or not defined
handle_msg(
    {incoming, Msg},
    NodeDef
) ->
    post_exception_or_debug(NodeDef, Msg, <<"no statemachine process">>),
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

statem_call(Pid, CallPayload) ->
    {
        element(1, sys:get_state(Pid)),
        gen_statem:call(Pid, CallPayload),
        element(1, sys:get_state(Pid))
    }.
%%
%%
define_func_send_msg(#{<<"emit_on_state_change">> := true}) ->
    fun send_message_on_state_change/6;
define_func_send_msg(_NodeDef) ->
    fun always_send_message/6.
