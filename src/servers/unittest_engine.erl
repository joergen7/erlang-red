-module(unittest_engine).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).
-export([stop/0]).
-export([start/0]).

%%
%% This engine is designed to run unit tests just as eunit does. Therefore
%% it also implements a listener for "should not happen" events. These are
%% passed to the error store service and then retrieved at the end of a test
%% to decided whether a test was successful or not.
%%

%%
%% These aren't export to be used external, these are "internal" exports.
-export([not_happen_loop/2]).
-export([run_test_on_another_planet/3]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.

handle_call(_Msg, _From, State) ->
    {reply, State, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, Store) ->
    {noreply, Store}.

stop_all_pids([], _) ->
    ok;
stop_all_pids([Pid | Pids], WsName) ->
    Pid ! {stop, WsName},
    stop_all_pids(Pids, WsName).

not_happen_loop(TestName, ErrorStorePid) ->
    receive
        stop ->
            ok;
        {it_happened, {NodeId, TabId}, Arg} ->
            Str = io_lib:format("~s in ~s\n", [Arg, TestName]),
            ErrorStorePid ! {store_msg, {NodeId, TabId, list_to_binary(Str)}},
            not_happen_loop(TestName, ErrorStorePid);
        _ ->
            not_happen_loop(TestName, ErrorStorePid)
    end.

ensure_error_store_is_started(TabErrColl, TestName) ->
    case error_store:start() of
        {error, {already_started, ErrorStorePid}} ->
            Pid = spawn(?MODULE, not_happen_loop, [TestName, ErrorStorePid]),
            register(TabErrColl, Pid),
            TabErrColl;
        {ok, ErrorStorePid} ->
            Pid = spawn(?MODULE, not_happen_loop, [TestName, ErrorStorePid]),
            register(TabErrColl, Pid),
            TabErrColl;
        _ ->
            error
    end.

start_this_should_not_happen_service(TabId, TestName) ->
    TabErrColl = ered_nodes:tabid_to_error_collector(TabId),

    case whereis(TabErrColl) of
        undefined ->
            error;
        _ ->
            TabErrColl ! stop,
            unregister(TabErrColl)
    end,
    ensure_error_store_is_started(TabErrColl, TestName).

send_off_test_result(WsName, FlowId, []) ->
    nodered:unittest_result(WsName, FlowId, success);
send_off_test_result(WsName, FlowId, Ary) ->
    nodered:unittest_result(WsName, FlowId, failed),
    dump_errors_onto_nodered(Ary, FlowId, WsName).

dump_errors_onto_nodered([], _FlowId, _WsName) ->
    ok;
dump_errors_onto_nodered([{NodeId, Msg} | T], FlowId, WsName) ->
    nodered:debug(WsName, nodered:debug_string(NodeId, FlowId, Msg), notice),
    dump_errors_onto_nodered(T, FlowId, WsName).

handle_info({start_test, FlowId, WsName}, State) ->
    spawn(?MODULE, run_test_on_another_planet, [FlowId, WsName, false]),
    {noreply, State};
handle_info({start_test, FlowId, WsName, IgnPendingFlag}, State) ->
    spawn(?MODULE, run_test_on_another_planet, [FlowId, WsName, IgnPendingFlag]),
    {noreply, State};
handle_info(stop, ErrorStore) ->
    gen_server:cast(?MODULE, stop),
    {noreply, ErrorStore};
handle_info(_Msg, ErrorStore) ->
    {noreply, ErrorStore}.

code_change(_OldVersion, ErrorStore, _Extra) ->
    {ok, ErrorStore}.

stop() ->
    gen_server:cast(?MODULE, stop).

terminate(normal, _State) ->
    ok.

%%
%% respect the pending flag on a flow test
run_test_on_another_planet(FlowId, WsName, false) ->
    error_store:reset_errors(FlowId),

    case flow_store_server:get_filename(FlowId) of
        error ->
            nodered:unittest_result(WsName, FlowId, unknown_testcase);
        FileName ->
            Ary = ered_flows:parse_flow_file(FileName),

            case ered_flows:is_test_case_pending(Ary) of
                true ->
                    nodered:unittest_result(WsName, FlowId, pending);
                false ->
                    run_the_test(FlowId, WsName, Ary)
            end
    end;
%%
%% ignore the pending flag of a flow tests
run_test_on_another_planet(FlowId, WsName, true) ->
    error_store:reset_errors(FlowId),

    case flow_store_server:get_filename(FlowId) of
        error ->
            nodered:unittest_result(WsName, FlowId, unknown_testcase);
        FileName ->
            Ary = ered_flows:parse_flow_file(FileName),

            run_the_test(FlowId, WsName, Ary)
    end.

%%
%% Run the test disregarding the pending flag of the test flow.
run_the_test(FlowId, WsName, Ary) ->
    case ered_flows:should_keep_flow_running(Ary) of
        true ->
            ered_nodes:create_pid_for_node(Ary, WsName);
        false ->
            ErrColl = start_this_should_not_happen_service(
                FlowId,
                <<"UnitTestEngine TestRun">>
            ),

            Pids = ered_nodes:create_pid_for_node(Ary, WsName),

            [
                ered_nodes:trigger_outgoing_messages(
                    maps:find(type, ND),
                    maps:find(id, ND),
                    WsName
                )
             || ND <- Ary
            ],

            %% give the messages time to propagate through the
            %% test flow
            timer:sleep(ered_flows:compute_timeout(Ary)),

            %% stop all nodes. Probably better would be to checking
            %% the message queues of the nodes and if all are empty
            %% it's probably safe to end the test case.
            stop_all_pids(Pids, WsName),

            %% some asserts work on the stop notification, give'em
            %% time to generate their results.
            timer:sleep(543),
            ErrColl ! stop,

            send_off_test_result(
                WsName,
                FlowId,
                error_store:get_errors(FlowId)
            )
    end.
