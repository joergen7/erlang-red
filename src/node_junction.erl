-module(node_junction).
-export([node_junction/1]).

%%
%% junctions are decorative elements that are "transparent" - they just
%% pass through the messages that they receive (having cloned the messages)
%%

receive_loop(NodeDef) ->
    receive
        {incoming,Msg} ->
            nodes:send_msg_to_connected_nodes(NodeDef,Msg),
            receive_loop(NodeDef)
    end.

node_junction(NodeDef) ->
    io:format("junction node init\n"),
    receive_loop(NodeDef).
