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
            io:format("DEBUG [~s]: ~p\n",[NodeName, Msg]),
            receive_loop(NodeDef)
    end.

node_debug(NodeDef) ->
    io:format("debug node init\n"),
    receive_loop(NodeDef).
