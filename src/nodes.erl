-module(nodes).

%% -compile(export_all).
-export([node_inject/1]).
-export([node_debug/1]).
-export([node_switch/1]).
-export([node_noop/1]).


sendMsg([],_Msg) ->
  ok;

sendMsg([WireId|Wires],Msg) ->
  NodeIdToPid = binary_to_atom(
                    list_to_binary(
                         lists:flatten(
                             io_lib:format("~s~s",["node_pid_",WireId])))),
  NodeIdToPid ! {message, Msg},
  sendMsg(Wires,Msg).


%%
%% Inject node should have at least one outgoing wire
%%
node_inject([Wires]) ->
   io:format("inject node init ~p\n", [Wires]),
   receive
    {message,Msg} ->
        sendMsg(Wires,Msg),
        io:format("inject got something\n")
   end.


%%
%% Debug nodes have no outgoing wires.
%%
node_debug([]) ->
   io:format("debug node init\n"),
   receive
    {message, Msg} ->
        io:format("debug got message ~p\n",[Msg])
   end.

%%
%% representation of a switch node, these can or cannot have
%% outgoing wires, although not having any would be very unusual.
%%
node_switch([]) ->
   io:format("switch node with no wires\n"),
   receive
      {message,_Msg} ->
         io:format("switch got something\n")
   end;

node_switch(Wires) ->
   io:format("switch node init ~p\n", [Wires]),
   receive
      {message,_Msg} ->
         io:format("switch got something\n")
   end.


%%
%% placeholder node for all types that aren't supported yet.
%%
node_noop(_) ->
  ok.
