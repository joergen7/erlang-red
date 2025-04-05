-module(test).

-export([main/0]).


main() ->
    Ary = flows:parse_flow_file('priv/flow.json'),

    Pids = nodes:create_pid_for_node(Ary),

    io:format("sending message to ~p\n", [Pids]),

    %% This is a random inject node that actually generates a message,
    %% it does not receive messages. But for testing ...
    node_pid_f9504da94c59e69f ! { outgoing, #{} }.
