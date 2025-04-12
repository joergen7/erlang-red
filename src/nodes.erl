-module(nodes).

%%
%% External exports, should be used by others.
%%
-export([create_pid_for_node/1]).
-export([get_prop_value_from_map/2]).
-export([get_prop_value_from_map/3]).
-export([generate_id/0]).
-export([generate_id/1]).
-export([nodeid_to_pid/1]).
-export([node_init/1]).
-export([enter_receivership/2]).
-export([enter_receivership/3]).
-export([this_should_not_happen/2]).
-export([jstr/2]).
-export([jstr/1]).
-export([tabid_to_error_collector/1]).
-export([trigger_outgoing_messages/3]).

%% send_msg_to_connnected_nodes assues an attribute 'wires' while
%% send send_msg_on is given an array of node ids and triggers the
%% 'incoming' message to be sent
-export([send_msg_to_connected_nodes/2]).
-export([send_msg_on/2]).

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
%%
%% TODO Not to self: once I get the hang of MACROS replace this receivership
%% TODO stuff with MACROS - this stuff needs to be fast since its called
%% TODO whenever messages are bounced around a flow.
enter_receivership(Module,NodeDef,only_stop) ->
    receive
        {stop,WsName} ->
            erlang:apply(Module, handle_stop, [NodeDef,WsName]);

        {incoming,_Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_incoming'),
            enter_receivership(Module, NodeDef2, only_stop);

        {outgoing,_Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_outgoing'),
            enter_receivership(Module, NodeDef2, only_stop)
    end;

enter_receivership(Module,NodeDef,incoming_and_outgoing) ->
    receive
        {stop,_WsName} ->
            ok;

        {incoming,Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_incoming'),
            NodeDef3 = erlang:apply(Module, handle_incoming, [NodeDef2,Msg]),
            enter_receivership(Module, NodeDef3, incoming_and_outgoing);

        {outgoing,Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_outgoing'),
            NodeDef3 = erlang:apply(Module, handle_outgoing, [NodeDef2,Msg]),
            enter_receivership(Module, NodeDef3, incoming_and_outgoing)
    end;

%%
%% link call nodes need a third type of message and that is the response
%% from a link out node that is in return mode. But a link call node does
%% not require an outgoing message type so ignore that.
enter_receivership(Module,NodeDef,link_call_node) ->
    receive
        {stop,_WsName} ->
            ok;

        {incoming,Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_incoming'),
            NodeDef3 = erlang:apply(Module, handle_incoming, [NodeDef2,Msg]),
            enter_receivership(Module, NodeDef3, link_call_node);

        {outgoing,_Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_outgoing'),
            enter_receivership(Module, NodeDef2, link_call_node);

        {link_return,Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_link_return'),
            NodeDef3 = erlang:apply(Module, handle_link_return, [NodeDef2,Msg]),
            enter_receivership(Module, NodeDef3, link_call_node)
    end;



enter_receivership(Module,NodeDef,only_incoming) ->
    receive
        {stop,_WsName} ->
            ok;

        {incoming,Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_incoming'),
            NodeDef3 = erlang:apply(Module, handle_incoming, [NodeDef2,Msg]),
            enter_receivership(Module, NodeDef3, only_incoming);

        {outgoing,_Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_outgoing'),
            enter_receivership(Module, NodeDef2, only_incoming)
    end;

enter_receivership(Module,NodeDef,Type) ->
    throw(io_lib:format("Umatched receivership type '~p' for ~p ~p~n",
                        [Type,Module,NodeDef])).

enter_receivership(Module,NodeDef) ->
    receive
        {stop,WsName} ->
            %% {ok, IdStr} = maps:find(id,NodeDef),
            %% {ok, TypeStr} = maps:find(type,NodeDef),
            %% io:format("node STOPPED id: [~p] type: [~p]\n",[IdStr,TypeStr]),

            case erlang:function_exported(Module,handle_stop,2) of
                true -> erlang:apply(Module, handle_stop, [NodeDef,WsName]);
                _ -> ignore
            end,
            ok;

        {incoming,Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_incoming'),
            NodeDef3 = erlang:apply(Module, handle_incoming, [NodeDef2,Msg]),
            enter_receivership(Module, NodeDef3);

        %% Outgoing messages are message generators (e.g inject node) - send
        %% this to the process to get it to generate a message, Those messages
        %% become incoming messages
        {outgoing,Msg} ->
            NodeDef2 = increment_message_counter(NodeDef,'_mc_outgoing'),
            NodeDef3 = erlang:apply(Module, handle_outgoing, [NodeDef2,Msg]),
            enter_receivership(Module, NodeDef3)
    end.

node_init(_NodeDef) ->
    ok.
    %% {ok, IdStr} = maps:find(id,NodeDef),
    %% {ok, TypeStr} = maps:find(type,NodeDef),
    %% io:format("node STARTED id: [~p] type: [~p]\n",[IdStr,TypeStr]).


generate_id(Length) ->
    IntLen = erlang:list_to_integer(erlang:float_to_list(Length/2,[{decimals,0}])),
    string:lowercase(
      list_to_binary(
        [ io_lib:format("~2.16.0B",
                        [X]) || <<X>> <= crypto:strong_rand_bytes(IntLen)])).

generate_id() ->
    generate_id(16).

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


%% TODO: a tab node (i.e. the tab containing a flow) also has a disabled
%% TODO: flag but this is called 'disabled'. If it is set, then the entire
%% TODO: flow should be ignoreed --> this is not handled at the moment.
%%
%% TODO2: Append the websocket name to the pid so that two browsers
%% TODO2: don't test with the same set of nodes. Each node is given the
%% TODO2: websocket name in the Msg object, so its not a problem for them
%% TODO2: to send their messages to the correct processes.
create_pid_for_node(Ary) ->
    create_pid_for_node(Ary,[]).


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
                        maps:put('_mc_link_return', 0,
                                 maps:put('_mc_outgoing', 0, NodeDef))),

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

%% Here 'on' is the same 'pass on' not as in 'on & off'. Standing on the
%% shoulders of great people is also not the same on as here. You turn me on
%% would also not be equivalent ... you have been warned.
send_msg_on([],_) ->
    ok;

send_msg_on([NodeId|Wires],Msg) ->
    NodePid = nodeid_to_pid(NodeId),
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


node_type_to_fun(<<"inject">>)    -> {node_inject,    node_inject};
node_type_to_fun(<<"switch">>)    -> {node_switch,    node_switch};
node_type_to_fun(<<"debug">>)     -> {node_debug,     node_debug};
node_type_to_fun(<<"junction">>)  -> {node_junction,  node_junction};
node_type_to_fun(<<"change">>)    -> {node_change,    node_change};
node_type_to_fun(<<"link out">>)  -> {node_link_out,  node_link_out};
node_type_to_fun(<<"link in">>)   -> {node_link_in,   node_link_in};
node_type_to_fun(<<"link call">>) -> {node_link_call, node_link_call};
node_type_to_fun(<<"delay">>)     -> {node_delay,     node_delay};

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


%% A list of all nodes that support outgoing messages, this was originally
%% only the inject node but then I realised that for testing purposes there
%% are in fact more.
trigger_outgoing_messages({ok, <<"http in">>}, {ok, IdStr}, WsName) ->
    nodes:nodeid_to_pid(IdStr) ! nodered:create_outgoing_msg(WsName);

trigger_outgoing_messages({ok, <<"inject">>}, {ok, IdStr}, WsName) ->
    nodes:nodeid_to_pid(IdStr) ! nodered:create_outgoing_msg(WsName);

trigger_outgoing_messages(_,_,_) ->
    ok.


%% TODO by the time I get down here, I always think that this functionality
%% TODO should be split out into a utilities/helper module but I never get
%% TODO around to it because I'm constantly adding to this module.
