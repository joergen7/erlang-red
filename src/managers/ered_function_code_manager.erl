-module(ered_function_code_manager).

-behaviour(gen_server).

-export([
    init/1,
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    stop/0,
    start/2
]).

-export([
    execute_sync/3,
    perform_func_code/3
]).

%%
%% This manager is spun up once per message to handle the execution of the
%% Erlang code contained in the function node. The execution needs to be managed
%% because the code could exit unexpectedly by having an "exit(...)" statement
%% or could timeout because it took too long or it could have an badarg exception
%% because it failed something else. These conditions need to be caught here
%% and communicated back to the original node process that started this manager.
%%
-import(ered_nodes, [
    jstr/1,
    jstr/2,
    send_msg_on/2
]).

-import(ered_nodered_comm, [
    node_status/5,
    post_exception_or_debug/3,
    ws_from/1
]).

-define(CODE_WRAPPER, fun() ->
    Msg2 = Func(NodeDef, Msg),
    Receiver ! {function_eval_result, Msg2, Msg}
end).

-define(POST_OFF_PARSE_ERROR,
    ErrMsg = jstr("Stanza: {{{ ~p }}} Error: ~p", [jstr(ErlangCode), Error]),
    node_status(ws_from(Msg), NodeDef, "parse error", "red", "dot"),
    post_exception_or_debug(NodeDef, Msg, ErrMsg)
).

-define(POST_MISSING_CODE(Txt),
    post_exception_or_debug(NodeDef, Msg, Txt)
).

-define(POST_TO_PARENT(Reason),
    is_process_alive(From) andalso
        (From ! {'DOWN', func_code_mgr, msg, Msg, Reason})
).

%%
%%
start(NodeDef, Msg) ->
    gen_server:start(?MODULE, {NodeDef, Msg, self(), undefined}, []).

init(Args) ->
    {ok, Args}.

%%
%%
%%  ------------------ call

handle_call(_, _From, State) ->
    {reply, ok, State}.

%%
%%
%%  ------------------ cast

handle_cast(_, State) ->
    {noreply, State}.

%%
%%  ------------------ info

% This kicks off a separate process for executing the actual code. It stores
% the process id into the state so that the timeout trigger can kill it
% if needed.
handle_info(perform_func_code, {NodeDef, Msg, From, undefined}) ->
    % if the subprocess code contains an "exit(self(),...)" call then we
    % want to know about it. So we can pass up the exit reason to the node
    % and the node process can decided whether to go down (if supervised) or
    % ignore the exit.
    process_flag(trap_exit, true),

    % we monitor our parent, if it dies, then kill everything here.
    % this happens when a supervisor kills the node because another node
    % went down and the restart policy is "one for all" or "rest for one".
    erlang:monitor(process, From),

    case maps:get(<<"timeout">>, NodeDef) of
        infinity ->
            ignore;
        Timeout ->
            erlang:start_timer(Timeout, self(), kill_process)
    end,

    ExecPid = erlang:spawn_link(
        ?MODULE,
        perform_func_code,
        [NodeDef, Msg, From]
    ),
    {noreply, {NodeDef, Msg, From, ExecPid}};
% timeout was triggered but has the execute process already completed?
% if so, then we can silently stop. Else we raise hell: kill the execute
% process, inform the parent and post exception.
handle_info(
    {timeout, _TRef, kill_process},
    {_NodeDef, Msg, From, ExecPid} = State
) ->
    case is_process_alive(ExecPid) of
        true ->
            exit(ExecPid, kill),
            ?POST_TO_PARENT(timeout_killing);
        false ->
            ignore
    end,
    {stop, normal, State};
% our parent was killed, we kill the exec pid and stop ourselves.
handle_info(
    {'DOWN', _, process, From, killed},
    {_NodeDef, _Msg, From, ExecPid} = State
) ->
    % ignore any timeout process that is running, it will eventually
    % trigger and find that this process has disappeared. QED.
    exit(ExecPid, kill),
    {stop, normal, State};
% so the execute pid exited because there was an exit(self(),...) call
% in the code. We pass this up to our parent and let them deal with it.
handle_info(
    {'EXIT', ExecPid, Reason},
    {_NodeDef, Msg, From, ExecPid} = State
) ->
    ?POST_TO_PARENT(Reason),
    {stop, normal, State};
% ignore all.
handle_info(_, State) ->
    {noreply, State}.

%%
%%
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

stop() ->
    gen_server:cast(?MODULE, stop).

%%
%%
terminate(normal, _State) ->
    ok;
terminate(_, _State) ->
    ok.

%%
%% ------------------------ Helpers
%%

%%
%%
perform_func_code(NodeDef, Msg, From) ->
    case maps:find(<<"func">>, NodeDef) of
        {ok, <<>>} ->
            ?POST_MISSING_CODE(<<"empty function code, doing nothing">>);
        {ok, Code} ->
            NewMsg = execute_sync(
                io_lib:format("fun(NodeDef,Msg) -> ~n ~s ~n end.", [Code]),
                NodeDef,
                Msg
            ),

            case
                send_message_on_ports(maps:get(<<"wires">>, NodeDef), NewMsg)
            of
                unacceptable_response ->
                    Msg2 = Msg#{failed_content => NewMsg},
                    post_exception_or_debug(
                        NodeDef,
                        Msg2,
                        <<"unacceptable response">>
                    );
                _ ->
                    is_process_alive(From) andalso
                        gen_server:cast(From, {func_completed_with, Msg})
            end;
        _ ->
            ?POST_MISSING_CODE(<<"function code not found">>)
    end.

%%
%%
execute_sync(ErlangCode, NodeDef, Msg) ->
    % this execute is performed by start and finalize code and returns
    % a NodeDef map.
    case evaluate_erlang(ErlangCode) of
        {ok, Func} ->
            Func(NodeDef, Msg);
        Error ->
            ?POST_OFF_PARSE_ERROR,
            NodeDef
    end.

%%
%%
handle_local_function(FunctionName, Args) ->
    io:format("~p(~p)", [FunctionName, Args]).

%%
%% This does no run the code, this compiles a function that contains the
%% the code. The funcion is then executed in the execute/4 function.
evaluate_erlang(Expression) when is_list(Expression) ->
    evaluate_erlang(list_to_binary(Expression));
evaluate_erlang(Expression) ->
    case erl_scan:string(binary_to_list(Expression)) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, Parsed} ->
                    case
                        erl_eval:exprs(
                            Parsed,
                            [],
                            {value, fun handle_local_function/2}
                        )
                    of
                        {value, Result, _} ->
                            {ok, Result}
                    end;
                Error ->
                    {error, Error}
            end;
        Error ->
            {error, Error}
    end.

%%
%% Support multiple output ports. For that the message returned has to be
%% a list containing message objects, i.e., maps.
send_message_on_ports(_, undefined) ->
    done;
send_message_on_ports(Wires, NewMsg) when is_map(NewMsg) ->
    send_message_on_ports(Wires, [NewMsg]);
send_message_on_ports([], _) ->
    done;
send_message_on_ports(_, []) ->
    done;
send_message_on_ports([_ | MoreWires], [undefined | MoreMsgs]) ->
    send_message_on_ports(MoreWires, MoreMsgs);
send_message_on_ports([Wires | MoreWires], [Msg | MoreMsgs]) when is_map(Msg) ->
    send_msg_on(Wires, Msg),
    send_message_on_ports(MoreWires, MoreMsgs);
send_message_on_ports(_, _) ->
    unacceptable_response.
