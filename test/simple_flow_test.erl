-module(simple_flow_test).

-include_lib("eunit/include/eunit.hrl").

stop_all_pids([]) ->
    ok;
stop_all_pids([Pid|Pids]) ->
    Pid ! stop,
    stop_all_pids(Pids).

%%
%% Node-RED frontend has a "create test case" that allows exporting of flows
%% to become cases. This test goes through and tests all off them.
%%

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


read_and_test_file([],Cnt) ->
    io:format("\n\n====> Total flows tested: [~p]\n\n",[Cnt]),
    ?debugMsg(?capturedOutput),
    ok;

read_and_test_file([FileName|MoreFileNames],Cnt) ->
    io:format("\n====> Test flow: [~p]\n",[FileName]),

    Ary = flows:parse_flow_file(FileName),
    Pids = nodes:create_pid_for_node(Ary),
    send_all_injects_the_go(Ary),

    timer:sleep(1500),
    stop_all_pids(Pids),
    ?debugMsg(?capturedOutput),

    read_and_test_file(MoreFileNames,Cnt).

%%
%% Because each test has too many Pids, the test waits 1.5 seconds for the
%% messages to propogate through the flow.
%% But because we test all flow files, this will take longer so we need
%% to extend the timeout of the unit test. Give each flow 2 seconds.
%%
all_testflows_test_() ->
    {Cnt,FileNames} = filelib:fold_files("priv/testflows", "",
                                   false,
                                   fun (Fname,Acc) ->
                                           { element(1,Acc) + 1,
                                             [Fname|element(2,Acc)] } end,
                                         {0,[]}),

    {timeout, 2*Cnt, fun() -> read_and_test_file(FileNames,Cnt) end}.
