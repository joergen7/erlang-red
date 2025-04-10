-module(flow_file_test).

-include_lib("eunit/include/eunit.hrl").

-export([not_happen_loop/2]).

stop_all_pids([]) ->
    ok;
stop_all_pids([Pid|Pids]) ->
    Pid ! stop,
    stop_all_pids(Pids).

%%
%% Node-RED frontend has a "create test case" that allows exporting of flows
%% to become cases. This test goes through and tests all off them.
%%
append_tab_name_to_filename(Ary,FileName) ->
    append_tab_name_to_filename(Ary, FileName, maps:find(z,lists:nth(2,Ary))).

append_tab_name_to_filename([],FileName,{ok,TabId}) ->
    {TabId,FileName};

append_tab_name_to_filename([NodeDef|MoreNodeDefs],FileName,{ok,TabId}) ->
    case maps:find(type,NodeDef) of
        {ok, <<"tab">>} ->
            {ok,Val} = maps:find(label,NodeDef),
            {ok,TabId2} = maps:find(id,NodeDef),

            {TabId2,
             binary_to_list(list_to_binary(io_lib:format("~s (~s)",
                                                         [Val, FileName])))};
        _ ->
            append_tab_name_to_filename(MoreNodeDefs, FileName,{ok,TabId})
    end.

send_injects_the_go({ok, <<"inject">>}, {ok, IdStr}) ->
    nodes:nodeid_to_pid(IdStr) ! {outgoing, #{'_msgid' => nodes:generate_id()}};

send_injects_the_go(_,_) ->
    ok.

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

%% Define this because the ErrorStorePid cannot be registered under a new
%% name. The point is that each error node in a flow sends its errors
%% to a specific error collector called "error_collector_<tabid>" (see
%% nodes:tabid_to_error_collector/1 for details).
%%
%% This allows tests to be run in parallel and errors are separated out
%% by their flow ids
%%
%% The error store is required because the spawn isn't a gen_server, i.e.,
%% I haven't found a way to get the data back from the error collector
%% service ...
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

not_happen_loop(TestName,ErrorStorePid) ->
    receive
        stop ->
            ok;

        {it_happened, {NodeId,TabId}, Arg} ->
            Str = io_lib:format("~s in ~s\n", [Arg,TestName]),
            %% io:format(list_to_binary(Str)),
            ErrorStorePid ! {store_msg, {NodeId, TabId, list_to_binary(Str)}},
            not_happen_loop(TestName,ErrorStorePid);

        _ ->
            not_happen_loop(TestName,ErrorStorePid)
    end.

create_test_for_flow_file([], Acc) ->
    Acc;

create_test_for_flow_file([FileName|MoreFileNames], Acc) ->
    Ary = flows:parse_flow_file(FileName),
    {TabId,TestName} = append_tab_name_to_filename(Ary,FileName),

    TestFunc = fun () ->
                       ErCo = start_this_should_not_happen_service(TabId,
                                                                   TestName),
                       error_store:reset_errors(TabId),

                       Pids = nodes:create_pid_for_node(Ary),
                       [send_injects_the_go(maps:find(type,ND),
                                            maps:find(id,ND)) || ND <- Ary],

                       %% give the messages time to propagate through the
                       %% test flow
                       timer:sleep(1234),

                       %% stop all nodes. Probably better would be to check
                       %% the message queues of the nodes and if all are empty
                       %% it's probably safe to end the test case.
                       stop_all_pids(Pids),

                       %% some asserts work on the stop notification, give'em
                       %% time to generate their results.
                       timer:sleep(543),
                       ErCo ! stop,

                       ?assertEqual([], error_store:get_errors(TabId))
               end,

    create_test_for_flow_file(MoreFileNames,
                              [{list_to_binary(color:yellow(TestName)),
                                fun() -> TestFunc() end}|Acc]).


foreach_testflow_test_() ->
    {_Cnt,FileNames} = filelib:fold_files("priv/testflows", "",
                                   false,
                                   fun (Fname,Acc) ->
                                           { element(1,Acc) + 1,
                                             [Fname|element(2,Acc)] } end,

                                         {0,[]}),

    TestList = create_test_for_flow_file(FileNames,[]),

    {foreach, fun () -> ok end, fun (_X) -> ok end, TestList}.
