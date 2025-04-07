-module(nodes).

%%
%% External exports, should be used by others.
%%
-export([create_pid_for_node/1]).
-export([send_msg_to_connected_nodes/2]).
-export([get_prop_value_from_map/2]).
-export([get_prop_value_from_map/3]).
-export([send_msg_on/2]).
-export([generate_id/0]).

%%
%% This is an internal exports
%%
-export([node_noop/1]).

generate_id() ->
    string:lowercase(
      list_to_binary(
        [ io_lib:format("~2.16.0B",
                        [X]) || <<X>> <= crypto:strong_rand_bytes(8) ])).

create_pid_for_node(Ary) ->
    create_pid_for_node(Ary,[]).


create_pid_for_node([],Pids) ->
    Pids;

create_pid_for_node([NodeDef|T],Pids) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),

    %% TODO: here have to respect the 'd' (disabled)  attribute.
    %% TODO: if true, then the node does not need to have a Pid
    %% TODO: created for it.
    {Module, Fun} = node_type_to_fun(TypeStr, maps:find(d,NodeDef)),

    NodeIdToPid = binary_to_atom(
                    list_to_binary(
                      lists:flatten(
                        io_lib:format("~s~s",["node_pid_",IdStr])))),

    case whereis(NodeIdToPid) of
        undefined ->
            ok;
        _ ->
            NodeIdToPid ! stop,
            unregister(NodeIdToPid)
    end,

    Pid = spawn(Module, Fun, [NodeDef]),
    register(NodeIdToPid, Pid),

    %% io:format("~p ~p\n",[Pid,NodeIdToPid]),
    create_pid_for_node(T,[NodeIdToPid|Pids]).



%%
%% placeholder node for all types that aren't supported yet and for
%% nodes that have been disabled.
%%
node_noop(_F) ->
    receive
        stop -> ok;
        _ -> node_noop(_F)
    end.

%%
%% The wires attribute is an array of arrays. The toplevel
%% array has one entry per port that a node has. (Port being the connectors
%% from which wires leave the node.)
%% If a node only has one port, then wires will be an array containing
%% exactly one array. This is the [Val] case and is the most common case.
%%
send_msg_to_connected_nodes(NodeDef,Msg) ->
    case maps:find(wires,NodeDef) of
        {ok,[Val]} ->
            send_msg_on(Val,Msg);
        {ok,Val} ->
            send_msg_on(Val,Msg)
    end.

%%
%% Avoid having to create the same case all the time.
%%
get_prop_value_from_map(Prop,Map,Default) ->
    case maps:find(Prop,Map) of
        {ok, Val} ->
            Val;
        _ ->
            Default
    end.

get_prop_value_from_map(Prop,Map) ->
    get_prop_value_from_map(Prop,Map,"").

%%
%% Helper for passing on messages once a node has completed with the message
%%
send_msg_on([],_) ->
    ok;

send_msg_on([WireId|Wires],Msg) ->
    NodeIdToPid = binary_to_atom(
                    list_to_binary(
                         lists:flatten(
                             io_lib:format("~s~s",["node_pid_",WireId])))),
    NodeIdToPid ! {incoming, Msg},
    send_msg_on(Wires,Msg).

%%
%% Lookup table for mapping node type to function. Also here we respect the
%% disabled flag: if node is disabled, give it a noop node else continue
%% on checking by type.
%%
node_type_to_fun(_Type, {ok, true}) ->
    io:format("node disabled, ignoring\n"),
    {?MODULE, node_noop};

node_type_to_fun(Type,_) ->
    node_type_to_fun(Type).


node_type_to_fun(<<"inject">>)   -> {node_inject,   node_inject};
node_type_to_fun(<<"switch">>)   -> {node_switch,   node_switch};
node_type_to_fun(<<"debug">>)    -> {node_debug,    node_debug};
node_type_to_fun(<<"junction">>) -> {node_junction, node_junction};
node_type_to_fun(<<"change">>)   -> {node_change,   node_change};

node_type_to_fun(Unknown) ->
    io:format("noop node initiated for unknown type: ~p\n", [Unknown]),
    {?MODULE, node_noop}.
