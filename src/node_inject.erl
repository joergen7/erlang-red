-module(node_inject).
-export([node_inject/1]).

%%
%% Inject node should have at least one outgoing wire
%%

receive_loop(NodeDef) ->
    receive
        %%
        %% outgoing messages are triggered by button presses on the UI
        %%
        {outgoing,Msg} ->
            nodes:send_msg_to_connected_nodes(NodeDef,Msg),
            io:format("inject triggered message\n"),
            receive_loop(NodeDef);

        {incoming,_} ->
            io:format("ERROR: inject node should not receive message\n"),
            receive_loop(NodeDef)
    end.

node_inject(NodeDef) ->
    io:format("inject node init\n"),
    receive_loop(NodeDef).
