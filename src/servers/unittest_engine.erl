-module(unittest_engine).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).
-export([stop/0]).
-export([start/0]).

-export([not_happen_loop/2]).
-export([run_test_on_another_planet/1]).

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


%%
%%

stop_all_pids([]) ->
    ok;
stop_all_pids([Pid|Pids]) ->
    Pid ! stop,
    stop_all_pids(Pids).

not_happen_loop(TestName,ErrorStorePid) ->
    receive
        stop ->
            ok;

        {it_happened, {NodeId,TabId}, Arg} ->
            Str = io_lib:format("~s in ~s\n", [Arg,TestName]),
            ErrorStorePid ! {store_msg, {NodeId, TabId, list_to_binary(Str)}},
            not_happen_loop(TestName,ErrorStorePid);

        _ ->
            not_happen_loop(TestName,ErrorStorePid)
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

start_this_should_not_happen_service(TabId,TestName) ->
    TabErrColl = nodes:tabid_to_error_collector(TabId),

    case whereis(TabErrColl) of
        undefined ->
            error;
        _ ->
            TabErrColl ! stop,
            unregister(TabErrColl)
    end,
    ensure_error_store_is_started(TabErrColl,TestName).

send_injects_the_go({ok, <<"inject">>}, {ok, IdStr}) ->
    nodes:nodeid_to_pid(IdStr) ! {outgoing, #{'_msgid' => nodes:generate_id()}};

send_injects_the_go(_,_) ->
    ok.

send_off_test_result(FlowId, []) ->
    nodered:unittest_result(FlowId,success);

send_off_test_result(FlowId, Ary) ->
    nodered:unittest_result(FlowId,failed),
    dump_errors_onto_nodered(Ary,FlowId).

dump_errors_onto_nodered([],_FlowId) ->
    ok;
dump_errors_onto_nodered([{NodeId, Msg}|T],FlowId) ->
    io:format("~p~n",[NodeId]),
    nodered:debug(nodered:debug_string(NodeId,FlowId,Msg),notice),
    dump_errors_onto_nodered(T,FlowId).

handle_info({start_test,FlowId}, State) ->
    spawn(?MODULE, run_test_on_another_planet, [FlowId]),
    {noreply, State};


handle_info(stop, ErrorStore) ->
    gen_server:cast(?MODULE, stop),
    {noreply, ErrorStore };

handle_info(_Msg, ErrorStore) ->
    {noreply, ErrorStore }.

code_change(_OldVersion, ErrorStore, _Extra) ->
    {ok, ErrorStore}.

stop() ->
    gen_server:cast(?MODULE, stop).

terminate(normal, _State) ->
    ok.

%%
%% spawn it out function
run_test_on_another_planet(FlowId) ->
    error_store:reset_errors(FlowId),

    FileName = flow_store_server:get_filename(FlowId),
    Ary = flows:parse_flow_file(FileName),

    ErrColl = start_this_should_not_happen_service(FlowId,<<"Irrevelant">>),

    Pids = nodes:create_pid_for_node(Ary),

    [send_injects_the_go(maps:find(type,ND),maps:find(id,ND)) || ND <- Ary],

    %% give the messages time to propagate through the
    %% test flow
    timer:sleep(1234),

    %% stop all nodes. Probably better would be to checking
    %% the message queues of the nodes and if all are empty
    %% it's probably safe to end the test case.
    stop_all_pids(Pids),

    %% some asserts work on the stop notification, give'em
    %% time to generate their results.
    timer:sleep(543),
    ErrColl ! stop,

    send_off_test_result(FlowId, error_store:get_errors(FlowId)).
