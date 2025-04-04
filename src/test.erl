-module(test).

-export([main/0]).

-compile(nodes).

main() ->
    {ok, Json} = file:read_file('priv/flow.json'),

    Push = fun(Key, Value, Acc) ->
       [{binary_to_atom(Key), Value} | Acc]
    end,

    {Ary,_,_} = json:decode(Json, ok, #{object_push => Push}),

    create_pid_for_node(Ary),

    io:format("sending message\n"),

    %% This is a random inject node that actually generates a message,
    %% it does not receive messages. But for testing ...
    node_pid_f9504da94c59e69f ! { message, "message" }.


type_to_atom(<<"inject">>) -> node_inject;
type_to_atom(<<"switch">>) -> node_switch;
type_to_atom(<<"debug">>) -> node_debug;
type_to_atom(Unknown) ->
  io:format("noop node initiated for unknown type: ~p\n", [Unknown]),
  node_noop.

create_pid_for_node([]) ->
  ok;

create_pid_for_node([H|T]) ->
  {ok, IdStr } = maps:find(id,H),
  {ok, TypeStr} = maps:find(type,H),

  Fun = type_to_atom(TypeStr),
  Module = nodes,

  case maps:find(wires,H) of
       {ok,Val} ->
         Pid = spawn(Module, Fun, [Val]);

       error ->
         Pid = spawn(Module, Fun, [[]])
  end,

  NodeIdToPid = binary_to_atom(list_to_binary(lists:flatten(io_lib:format("~s~s",["node_pid_",IdStr])))),
  register(NodeIdToPid, Pid),

  %% io:format("~p ~p\n",[Pid,NodeIdToPid]),
  create_pid_for_node(T).
