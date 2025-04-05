-module(node_switch).

-export([node_switch/1]).


%%
%% representation of a switch node, these can or cannot have
%% outgoing wires, although not having any would be very unusual.
%%

receive_loop(NodeDef) ->
    receive
        {incoming,_Msg} ->
            io:format("switch got something\n"),
            receive_loop(NodeDef);

        stop ->
            ok
    end.

node_switch(NodeDef) ->
    io:format("switch node init\n"),
    receive_loop(NodeDef).
