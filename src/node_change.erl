-module(node_change).
-export([node_change/1]).

%%
%% Inject node should have at least one outgoing wire
%%

receive_loop(NodeDef) ->
    receive
        %%
        %% outgoing messages are triggered by button presses on the UI
        %%
        {incoming,_Msg} ->
            io:format("change node altered Msg\n"),
            receive_loop(NodeDef);

        stop ->
            ok
    end.

node_change(NodeDef) ->
    io:format("change node init\n"),
    receive_loop(NodeDef).
