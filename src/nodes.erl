-module(nodes).

-export([create_pid_for_node/1]).


%%
%% This is internal exports
%%
-export([node_noop/1, node_switch/1, node_debug/1, node_inject/1]).


create_pid_for_node([]) ->
  ok;

create_pid_for_node([NodeDef|T]) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),

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
%% Inject node should have at least one outgoing wire
%%
node_inject(NodeDef) ->
   io:format("inject node init\n"),
   receive
       {message,Msg} ->
           send_msg_to_connected_nodes(NodeDef,Msg),
           io:format("inject got something\n")
   end.


%%
%% Debug nodes have no outgoing wires.
%%
node_debug(_NodeDef) ->
   io:format("debug node init\n"),
   receive
    {message, Msg} ->
        io:format("debug got message ~p\n",[Msg])
   end.

%%
%% representation of a switch node, these can or cannot have
%% outgoing wires, although not having any would be very unusual.
%%
node_switch(_NodeDef) ->
   io:format("switch node init\n"),
   receive
      {message,_Msg} ->
         io:format("switch got something\n")
   end.



send_msg_to_connected_nodes(NodeDef,Msg) ->
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
    NodeIdToPid ! {message, Msg},
    sendMsg(Wires,Msg).

%%
%% Lookup table for node type to function.
%%
node_type_to_fun(<<"inject">>) -> {?MODULE, node_inject};
node_type_to_fun(<<"switch">>) -> {?MODULE, node_switch};
node_type_to_fun(<<"debug">>) -> {?MODULE, node_debug};

node_type_to_fun(Unknown) ->
  io:format("noop node initiated for unknown type: ~p\n", [Unknown]),
  {?MODULE, node_noop}.
