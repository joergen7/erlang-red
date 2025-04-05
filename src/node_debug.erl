-module(node_debug).
-export([node_debug/1]).


%%
%% Debug nodes have no outgoing wires.
%%

receive_loop(NodeDef) ->
    receive
        {incoming, Msg} ->
            case maps:find(name,NodeDef) of
                {ok, Name} ->
                    NodeName = Name;
                _ ->
                    NodeName = "undefined"
            end,

            %%
            %% TODO this has to go down websocket connection in the long term
            %%
            io:format("DEBUG [~s]: ~p\n", [NodeName, Msg]),
            receive_loop(NodeDef);

        stop ->
            ok

    end.

node_debug(NodeDef) ->
    io:format("debug node init\n"),
    receive_loop(NodeDef).
