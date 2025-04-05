-module(nodes).

-export([create_pid_for_node/1]).
-export([send_msg_to_connected_nodes/2]).


%%
%% This is internal exports
%%
-compile(node_switch).
-compile(node_inject).

-export([node_noop/1, node_junction/1, node_debug/1]).


create_pid_for_node([]) ->
    ok;

create_pid_for_node([NodeDef|T]) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),

    %% TODO: here have to respect the 'd' (disabled)  attribute.
    %% TODO: if true, then the node does not need to have a Pid
    %% TODO: created for it.
    {Module, Fun} = node_type_to_fun(TypeStr),
    Pid = spawn(Module, Fun, [NodeDef]),

    NodeIdToPid = binary_to_atom(
                    list_to_binary(
                      lists:flatten(
                        io_lib:format("~s~s",["node_pid_",IdStr])))),

    register(NodeIdToPid, Pid),

    %% io:format("~p ~p\n",[Pid,NodeIdToPid]),
    create_pid_for_node(T).



%%
%% placeholder node for all types that aren't supported yet.
%%
node_noop(_) ->
  ok.

%%
%% junctions are decorative elements that are "transparent" - they just
%% pass through the messages that they receive (having cloned the messages)
%%
node_junction(NodeDef) ->
   receive
       {incoming, Msg} ->
           send_msg_to_connected_nodes(NodeDef,Msg),
           io:format("junction node passed on msg\n")
   end.


%%
%% Debug nodes have no outgoing wires.
%%
node_debug(_NodeDef) ->
   io:format("debug node init\n"),
   receive
       {incoming, Msg} ->
           io:format("debug got message ~p\n",[Msg])
   end.


send_msg_to_connected_nodes(NodeDef,Msg) ->
    %%
    %% The wires attribute is an array of arrays. The toplevel
    %% array has one entry per port that a node has. (Port being the connectors
    %% from which wires leave the node.)
    %% If a node only has one port, then wires will be an array containing
    %% exactly one array. This is the [Val] case and is the most common case.
    %%
    case maps:find(wires,NodeDef) of
        {ok,[Val]} ->
            sendMsg(Val,Msg);
        {ok,Val} ->
            sendMsg(Val,Msg)
    end.

%%
%% Helper for passing on messages once a node has completed with the message
%%
sendMsg([],_) ->
    ok;

sendMsg([WireId|Wires],Msg) ->
    NodeIdToPid = binary_to_atom(
                    list_to_binary(
                         lists:flatten(
                             io_lib:format("~s~s",["node_pid_",WireId])))),
    NodeIdToPid ! {incoming, Msg},
    sendMsg(Wires,Msg).

%%
%% Lookup table for node type to function.
%%
node_type_to_fun(<<"inject">>) -> {node_inject, node_inject};
node_type_to_fun(<<"switch">>) -> {node_switch, node_switch};
node_type_to_fun(<<"debug">>) -> {?MODULE, node_debug};

node_type_to_fun(Unknown) ->
  io:format("noop node initiated for unknown type: ~p\n", [Unknown]),
  {?MODULE, node_noop}.
