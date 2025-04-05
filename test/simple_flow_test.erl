-module(simple_flow_test).

-include_lib("eunit/include/eunit.hrl").

-compile(nodes).
-compile(flows).

wait(Sec) ->
    receive
    after (1000 * Sec) -> ok
    end.

first_test() ->
    Ary = flows:parse_flow_file('priv/flow.json'),

    nodes:create_pid_for_node(Ary),

    io:format("sending message\n"),

    %% This is a random inject node that actually generates a message,
    %% it does not receive messages. But for testing ...
    node_pid_f9504da94c59e69f ! { outgoing, #{} },

    io:format("sending message\n"),
    node_pid_f9504da94c59e69f ! { outgoing, #{} },
    io:format("sending message\n"),
    node_pid_f9504da94c59e69f ! { outgoing, #{} },
    io:format("sending message\n"),
    node_pid_f9504da94c59e69f ! { outgoing, #{} },
    io:format("sending message\n"),
    node_pid_f9504da94c59e69f ! { outgoing, #{} },
    io:format("sending message\n"),
    node_pid_f9504da94c59e69f ! { outgoing, #{} },

    ?assertEqual(5, 6).


junction_nodes_test() ->
    Ary = flows:parse_flow_file('priv/flow.junctions.json'),

    nodes:create_pid_for_node(Ary),

    io:format("sending message\n"),

    %% This is a random inject node that actually generates a message,
    %% it does not receive messages. But for testing ...
    node_pid_323c39235c019b84 ! { outgoing, #{} },

    io:format("sending message\n"),
    node_pid_323c39235c019b84 ! { outgoing, #{} },
    io:format("sending message\n"),
    node_pid_323c39235c019b84 ! { outgoing, #{} },
    io:format("sending message\n"),

    %%
    %% TODO: need a good way to discover when the message has propogated
    %% TODO: throughout the entire flow, that's when the test is complete.
    %%
    ?assertEqual(5, 5),
    ?assertEqual(5, 5),
    ?assertEqual(5, 5),
    ?assertEqual(5, 5),
    ?assertEqual(5, 5),
    ?assertEqual(5, 6).
