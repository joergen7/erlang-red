-module(simple_flow_test).

-include_lib("eunit/include/eunit.hrl").

stop_all_pids([]) ->
    ok;
stop_all_pids([Pid|Pids]) ->
    Pid ! stop,
    stop_all_pids(Pids).

first_test() ->
    Ary = flows:parse_flow_file('priv/flow.json'),

    Pids = nodes:create_pid_for_node(Ary),

    io:format("sending message - 1st inject\n"),
    node_pid_f9504da94c59e69f ! { outgoing, #{ '_msgid' => nodes:generate_id() }},


    io:format("sending message - 2nd inject\n"),
    node_pid_034562d7b8d76ac9 ! { outgoing, #{ '_msgid' => nodes:generate_id() }},

    receive
        helo ->
            ok
        after 3000 ->
            stop_all_pids(Pids),
            ?debugMsg(?capturedOutput)
    end,

    ?assert(true).


junction_nodes_test() ->
    Ary = flows:parse_flow_file('priv/flow.junctions.json'),

    Pids = nodes:create_pid_for_node(Ary),

    io:format("sending message\n"),
    node_pid_323c39235c019b84 ! { outgoing, #{ '_msgid' => nodes:generate_id() } },

    receive
        helo ->
            ok
        after 3000 ->
            stop_all_pids(Pids),
            ?debugMsg(?capturedOutput)
    end,

    ?assert(true).


disabled_node_test() ->
    Ary = flows:parse_flow_file('priv/flow.disabled.node.json'),

    Pids = nodes:create_pid_for_node(Ary),

    io:format("sending message\n"),
    node_pid_7858c408d5bd9924 ! { outgoing, #{ '_msgid' => nodes:generate_id() } },

    receive
        helo ->
            ok
        after 3000 ->
            stop_all_pids(Pids),
            ?debugMsg(?capturedOutput)
    end,

    ?assert(true).


attribute_setting_test() ->
    Ary = flows:parse_flow_file('priv/flow.attribute.setting.json'),

    Pids = nodes:create_pid_for_node(Ary),

    io:format("sending message\n"),
    node_pid_1460cac9a329205a ! { outgoing, #{ '_msgid' => nodes:generate_id() } },

    receive
        helo ->
            ok
        after 3000 ->
            stop_all_pids(Pids),
            ?debugMsg(?capturedOutput)
    end,
    ?assert(true).
