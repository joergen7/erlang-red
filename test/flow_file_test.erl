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

append_tab_name_to_filename([],FileName) ->
    FileName;

append_tab_name_to_filename([NodeDef|MoreNodeDefs],FileName) ->
    case maps:find(type,NodeDef) of
        {ok, <<"tab">>} ->
            {ok,Val} = maps:find(label,NodeDef),
            binary_to_list(list_to_binary(io_lib:format("~s (~s)", [Val, FileName])));
        _ ->
            append_tab_name_to_filename(MoreNodeDefs,FileName)
    end.

send_injects_the_go(<<"inject">>,IdStr) ->
    io:format("==> sending creating message using inject [~p]\n",[IdStr]),
    nodes:nodeid_to_pid(IdStr) ! {outgoing, #{'_msgid' => nodes:generate_id()}};

send_injects_the_go(_,_) ->
    ok.

send_all_injects_the_go([]) ->
    ok;

send_all_injects_the_go([NodeDef|MoreNodes]) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),
    send_injects_the_go(TypeStr,IdStr),
    send_all_injects_the_go(MoreNodes).

stop_error_store(TestName) ->
    timer:sleep(100),

    case error_store:start() of
        {error, {already_started, _ErrorStorePid}} ->
            error_store:stop(),
            stop_error_store(TestName);

        {ok, ErrorStorePid} ->
            Pid = spawn(?MODULE, not_happen_loop,
                        [TestName, ErrorStorePid]),
            register(this_should_not_happen_service, Pid);

        _ ->
            ok
    end.

start_this_should_not_happen_service(TestName) ->
    case whereis(this_should_not_happen_service) of
        undefined -> ok;
        _ ->
            this_should_not_happen_service ! stop,
            unregister(this_should_not_happen_service)
    end,

    stop_error_store(TestName).

not_happen_loop(TestName,ErrorStorePid) ->
    receive
        stop ->
            ErrorStorePid ! stop,
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
    TestFunc = fun (Ary,TestName) ->
                       start_this_should_not_happen_service(TestName),

                       Pids = nodes:create_pid_for_node(Ary),
                       send_all_injects_the_go(Ary),

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

                       ?assertEqual([], error_store:get_store())
               end,

    Ary = flows:parse_flow_file(FileName),
    TestName = append_tab_name_to_filename(Ary,FileName),

    create_test_for_flow_file(MoreFileNames,
                              [{list_to_binary(color:yellow(TestName)),
                                fun() -> TestFunc(Ary, TestName) end}|Acc]).


foreach_testflow_test_() ->
    {_Cnt,FileNames} = filelib:fold_files("priv/testflows", "",
                                   false,
                                   fun (Fname,Acc) ->
                                           { element(1,Acc) + 1,
                                             [Fname|element(2,Acc)] } end,

                                         {0,[]}),

    TestList = create_test_for_flow_file(FileNames,[]),

    {foreach, fun () -> ok end, fun (_X) -> ok end, TestList}.
