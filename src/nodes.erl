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
-export([nodeid_to_pid/1]).
-export([node_init/1]).
-export([enter_receivership/2]).
-export([enter_receivership/3]).
-export([this_should_not_happen/2]).
-export([jstr/2]).
-export([jstr/1]).
-export([tabid_to_error_collector/1]).

%%
%% Common functionality
%%


jstr(Fmt,Args) ->
    list_to_binary(lists:flatten(io_lib:format(Fmt,Args))).

jstr(Str) when is_binary(Str) ->
    Str;
jstr(Str) ->
    list_to_binary(lists:flatten(Str)).

this_should_not_happen(NodeDef,Arg) ->
    {ok, TabId} = maps:find(z,NodeDef),
    ErrCollector = tabid_to_error_collector(TabId),

    case whereis(ErrCollector) of
        undefined ->
            io:format("TSNH: ~s\n",[Arg]);
        _ ->
            {ok, IdStr } = maps:find(id,NodeDef),
            {ok, ZStr } = maps:find(z,NodeDef),
            ErrCollector ! {it_happened, {IdStr,ZStr}, Arg}
    end.

increment_message_counter(NodeDef, CntName) ->
    {ok, V} = maps:find(CntName, NodeDef),
    maps:put(CntName, V + 1, NodeDef).


%% this is used  by the assert success node, since it does nothing with a
%% message (i.e. it has no output ports), it only needs the stop notification
%% so shortcut the callback stuff.
enter_receivership(Module,NodeDef,only_stop) ->
    receive
        stop ->
            erlang:apply(Module, handle_stop, [NodeDef]);

        {incoming,_Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_incoming'),
            enter_receivership(Module, NodeDef2, only_stop);

        {outgoing,_Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_outgoing'),
            enter_receivership(Module, NodeDef2, only_stop)
    end;

enter_receivership(Module,NodeDef,only_incoming) ->
    receive
        stop ->
            ok;

        {incoming,Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_incoming'),
            erlang:apply(Module, handle_incoming, [NodeDef2,Msg]),
            enter_receivership(Module, NodeDef2, only_incoming);

        {outgoing,_Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_outgoing'),
            enter_receivership(Module, NodeDef2, only_incoming)
    end.


enter_receivership(Module,NodeDef) ->
    receive
        stop ->
            %% {ok, IdStr} = maps:find(id,NodeDef),
            %% {ok, TypeStr} = maps:find(type,NodeDef),
            %% io:format("node STOPPED id: [~p] type: [~p]\n",[IdStr,TypeStr]),

            case erlang:function_exported(Module,handle_stop,1) of
                true -> erlang:apply(Module, handle_stop, [NodeDef]);
                _ -> ignore
            end,
            ok;

        {incoming,Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_incoming'),
            erlang:apply(Module, handle_incoming, [NodeDef2,Msg]),
            enter_receivership(Module, NodeDef2);

        %% Outgoing messages are message generators (e.g inject node) - send
        %% this to the process to get it to generate a message, Those messages
        %% become incoming messages
        {outgoing,Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_outgoing'),
            erlang:apply(Module, handle_outgoing, [NodeDef2,Msg]),
            enter_receivership(Module, NodeDef2)
    end.

node_init(_NodeDef) ->
    ok.
    %% {ok, IdStr} = maps:find(id,NodeDef),
    %% {ok, TypeStr} = maps:find(type,NodeDef),
    %% io:format("node STARTED id: [~p] type: [~p]\n",[IdStr,TypeStr]).

generate_id() ->
    string:lowercase(
      list_to_binary(
        [ io_lib:format("~2.16.0B",
                        [X]) || <<X>> <= crypto:strong_rand_bytes(8) ])).

nodeid_to_pid(IdStr) ->
    binary_to_atom(
      list_to_binary(
        lists:flatten(
          io_lib:format("~s~s", ["node_pid_",IdStr] )))).

tabid_to_error_collector(IdStr) ->
    binary_to_atom(
      list_to_binary(
        lists:flatten(
          io_lib:format("~s~s", ["error_collector_",IdStr] )))).

create_pid_for_node(Ary) ->
    create_pid_for_node(Ary,[]).


%% TODO: a tab node (i.e. the tab containing a flow) also has a disabled
%% TODO: flag but this is called 'disabled'. If it is set, then the entire
%% TODO: flow should be ignoreed --> this is not handled at the moment.
create_pid_for_node([],Pids) ->
    Pids;

create_pid_for_node([NodeDef|MoreNodeDefs],Pids) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),

    %% here have to respect the 'd' (disabled) attribute. if true, then
    %% the node does not need to have a Pid created for it.
    {Module, Fun} = node_type_to_fun(TypeStr, maps:find(d,NodeDef)),

    NodePid = nodeid_to_pid(IdStr),

    case whereis(NodePid) of
        undefined ->
            ok;
        _ ->
            NodePid ! stop,
            unregister(NodePid)
    end,

    %% internal message counters, get updated automagically in
    %% enter_receivership ==> 'mc' is message counter.
    NodeDef2 = maps:put('_mc_incoming', 0,
                       maps:put('_mc_outgoing', 0, NodeDef)),

    Pid = spawn(Module, Fun, [NodeDef2]),
    register(NodePid, Pid),

    %% io:format("~p ~p\n",[Pid,NodePid]),
    create_pid_for_node(MoreNodeDefs, [NodePid|Pids]).

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
            case Val of
                <<"">> ->   Default;
                "" ->       Default;
                _ ->        Val
            end;
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
    NodePid = nodeid_to_pid(WireId),
    case whereis(NodePid) of
        undefined ->
            ok;
        _ ->
            NodePid ! {incoming, Msg}
    end,
    send_msg_on(Wires,Msg).

%%
%% Lookup table for mapping node type to function. Also here we respect the
%% disabled flag: if node is disabled, give it a noop node else continue
%% on checking by type.
%%
node_type_to_fun(_Type, {ok, true}) ->
    io:format("node disabled, ignoring\n"),
    {node_disabled, node_disabled};

node_type_to_fun(Type,_) ->
    node_type_to_fun(Type).


node_type_to_fun(<<"inject">>)   -> {node_inject,   node_inject};
node_type_to_fun(<<"switch">>)   -> {node_switch,   node_switch};
node_type_to_fun(<<"debug">>)    -> {node_debug,    node_debug};
node_type_to_fun(<<"junction">>) -> {node_junction, node_junction};
node_type_to_fun(<<"change">>)   -> {node_change,   node_change};

%%
%% Assert nodes for testing functionality of the nodes
%%
node_type_to_fun(<<"ut-assert-values">>) ->
    {node_assert_values, node_assert_values};
node_type_to_fun(<<"ut-assert-failure">>) ->
    {node_assert_failure, node_assert_failure};
node_type_to_fun(<<"ut-assert-success">>) ->
    {node_assert_success, node_assert_success};

node_type_to_fun(Unknown) ->
    io:format("noop node initiated for unknown type: ~p\n", [Unknown]),
    {node_noop, node_noop}.
