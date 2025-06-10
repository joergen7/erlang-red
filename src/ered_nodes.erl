-module(ered_nodes).

-export([
    add_state/2,
    create_pid_for_node/2,
    get_node_name/2,
    get_prop_value_from_map/2,
    get_prop_value_from_map/3,
    generate_id/0,
    generate_id/1,
    jstr/2,
    jstr/1,
    nodeid_to_pid/2,
    post_exception_or_debug/3,

    %% send_msg_to_connnected_nodes assues an attribute 'wires' while
    %% send send_msg_on is given an array of node ids and triggers the
    %% 'incoming' message to be sent
    send_msg_on/2,
    send_msg_on_by_pids/2,
    send_msg_to_connected_nodes/2,

    %% This is used by the supervisor node to revive dead processes
    spin_up_and_link_node/2,
    is_supervisor/1,

    tabid_to_error_collector/1,
    this_should_not_happen/2,
    trigger_outgoing_messages/3,
    unpriv/1
]).

%%
%% Module provides help for the nodes doing the work. Just a collection
%% of helper functionality that may or may not be used.
%%
%% The main rasion d'etre for this module is the create_pid_for_node/2
%% functionality that spins up the processes for the nodes defined in
%% a flow. This is used by unittest code and the compute engine. In
%% contains all sorts of logic for the various types of nodes and their
%% specific behaviours.
%%

-import(ered_nodered_comm, [
    node_status/5,
    send_out_debug_msg/4,
    send_out_debug_error/2,
    ws_from/1
]).
-import(ered_msg_handling, [
    create_outgoing_msg/1
]).
-import(ered_message_exchange, [
    clear_pg_group/1,
    post_exception/3
]).

%%
%% Since exceptions are either handled by a catch node or posted in the
%% debug panel if they don't get caught.
post_exception_or_debug(NodeDef, Msg, ErrMsg) ->
    case post_exception(NodeDef, Msg, jstr(ErrMsg)) of
        dealt_with ->
            ok;
        _ ->
            send_out_debug_error(NodeDef, maps:put(error_msg, ErrMsg, Msg))
    end.

%%
%% Map priv directory in file names
unpriv(FileName) when is_binary(FileName) ->
    unpriv(binary_to_list(FileName));
unpriv(FileName) when is_list(FileName) ->
    string:replace(FileName, "${priv}", code:priv_dir(erlang_red), all);
unpriv(FileName) ->
    %% Let it fail.
    FileName.

%%
%% Common functionality
%%
jstr(Fmt, Args) ->
    list_to_binary(lists:flatten(io_lib:format(Fmt, Args))).

jstr(Str) when is_binary(Str) ->
    Str;
jstr(Str) when is_atom(Str) ->
    atom_to_binary(Str);
jstr(Str) ->
    list_to_binary(lists:flatten(Str)).

%%
%%
this_should_not_happen(NodeDef, Arg) ->
    {ok, TabId} = maps:find(z, NodeDef),
    ErrCollector = tabid_to_error_collector(TabId),

    case whereis(ErrCollector) of
        undefined ->
            io:format("TSNH: ~s\n", [Arg]);
        _ ->
            {ok, IdStr} = maps:find(id, NodeDef),
            {ok, ZStr} = maps:find(z, NodeDef),
            ErrCollector ! {it_happened, {IdStr, ZStr}, Arg}
    end.

%%
%%
generate_id(Length) ->
    IntLen = erlang:list_to_integer(
        erlang:float_to_list(Length / 2, [{decimals, 0}])
    ),
    string:lowercase(
        list_to_binary(
            [
                io_lib:format(
                    "~2.16.0B",
                    [X]
                )
             || <<X>> <= crypto:strong_rand_bytes(IntLen)
            ]
        )
    ).

%% generate an id in a form that is conform with NodeRED ids: 16 hexadecimal.
generate_id() ->
    generate_id(16).

%% processes for nodes are scoped by the websocket name so that each
%% each websocket connection gets its own collection of processes for nodes.
get_node_name(WsName, IdStr) ->
    list_to_binary(
        lists:flatten(
            io_lib:format("~s~s~s~s", [
                "node_pid_", jstr(WsName), "_", IdStr
            ])
        )
    ).

nodeid_to_pid(WsName, IdStr) ->
    Name = get_node_name(WsName, IdStr),
    case pg:get_members(Name) of
        [] ->
            {error, undefined};
        [Pid | []] ->
            {ok, Pid};
        [_H | _T] ->
            io:format("Multiple PIDs for node name ~p~n", [Name]),
            {error, too_many}
    end.

tabid_to_error_collector(IdStr) ->
    binary_to_atom(
        list_to_binary(
            lists:flatten(
                io_lib:format("~s~s", ["error_collector_", IdStr])
            )
        )
    ).

%% TODO: a tab node (i.e. the tab containing a flow) also has a disabled
%% TODO: flag but this is called 'disabled'. If it is set, then the entire
%% TODO: flow should be ignored --> this is not handled at the moment.
create_pid_for_node(AryAll, WsName) ->
    store_config_nodes(AryAll, WsName),

    % all this does is ensure that the code contained in a module node
    % is installed in this instance of the BEAM.
    % TODO: because we do this before initialising any catch nodes, the
    % TODO: exception raised here always ends up in the debug panel.
    install_module_node_code(AryAll, WsName),

    % as a side affect, this will drop any disabled nodes from the list.
    {Ary, Supervisors} = extract_supervisors(AryAll),

    % limit is set sufficiently large (`+ 100`) so that complex supervisor trees
    % are supported. If the limit is too small, that can lead to distractingly
    % difficult bugs to debug. The Limit is a hard upper limit on the recursion
    % required for constructing supervisors trees. If there are no more
    % supervisors to configure, then the recursion exits before the limit
    % is reached.
    ReducedAry = supervisor_filter_nodes(
        Supervisors,
        Ary,
        [],
        WsName,
        length(Supervisors) + 100
    ),

    create_pid_for_node(ReducedAry, [], WsName).

%% erlfmt:ignore equals and arrows should line up here.
create_pid_for_node([], Pids, _WsName) ->
    Pids;
create_pid_for_node([NodeDef | MoreNodeDefs], Pids, WsName) ->
    {ok, Pid} = spin_up_node(NodeDef, WsName),
    create_pid_for_node(MoreNodeDefs, [Pid | Pids], WsName).

%%
%%
install_module_node_code([], _WsName) ->
    ok;
install_module_node_code([NodeDef | ModeNodeDefs], WsName) ->
    case {is_module_node(NodeDef), disabled(NodeDef)} of
        {true, false} ->
            ered_node_erlmodule:install(NodeDef, WsName);
        _ ->
            ignore
    end,
    install_module_node_code(ModeNodeDefs, WsName).

%%
%% Add the state to the NodeDef to prepare it for starting the flow.
%%
%% internal message counters, get updated automagically in
%% enter_receivership ==> 'mc' is message counter.
add_state(NodeDef, NodePid) ->
    NodeDef1 = maps:put('_node_pid_', NodePid, NodeDef),
    NodeDef2 = maps:put('_mc_incoming', 0, NodeDef1),
    NodeDef3 = maps:put('_mc_link_return', 0, NodeDef2),
    NodeDef4 = maps:put('_mc_websocket', 0, NodeDef3),
    NodeDef5 = maps:put('_mc_outgoing', 0, NodeDef4),
    maps:put('_mc_exception', 0, NodeDef5).

%%
%%
extract_supervisors(NodeDefs) ->
    extract_supervisors(NodeDefs, [], []).

extract_supervisors([], Supervisors, Nodes) ->
    {Nodes, Supervisors};
extract_supervisors([NodeDef | MoreNodeDefs], Supervisors, Nodes) ->
    case {is_supervisor(maps:get(type, NodeDef)), disabled(NodeDef)} of
        {_, true} ->
            %% drop any disabled nodes from the list of nodes
            extract_supervisors(MoreNodeDefs, Supervisors, Nodes);
        {true, false} ->
            extract_supervisors(MoreNodeDefs, [NodeDef | Supervisors], Nodes);
        {false, false} ->
            extract_supervisors(MoreNodeDefs, Supervisors, [NodeDef | Nodes])
    end.

%%
%% supervisor_filter_nodes iterates through the node defintions until all
%% supervisors have their nodes. This iteration is needed for implementing
%% the supervisor-of-supervisor pattern.
%%
%% What each supervisor definition does is add the nodes to its definition
%% and remove those nodes from the list of node definitions. here order is
%% important since a supervisor should only be defined once the other supervisor
%% has defined itself. It has done this when it reappears in the list of
%% node definitions.
%%
%% A supervisor node definition will be removed from the list of node
%% definitions to be started but the supervisor will add itself back to the
%% list of node definitions once it has found all its children. Hence another
%% supervisor that is supervising that supervisor can then pick it up from
%% there.
supervisor_filter_nodes(
    Supervisors,
    NodeDefs,
    RedoSupers,
    WsName,
    0
) when length(Supervisors) > 0; length(RedoSupers) > 0 ->
    FailedSuper = fun(NodeDef, WebsocketName) ->
        send_out_debug_msg(
            NodeDef,
            #{'_ws' => WebsocketName},
            <<"Not all supervisors configured, check configurations">>,
            error
        ),
        node_status(WsName, NodeDef, "configuration failure", "red", "ring")
    end,
    [FailedSuper(ND, WsName) || ND <- (RedoSupers ++ Supervisors)],
    NodeDefs;
supervisor_filter_nodes(_Supervisors, NodeDefs, _RedoSupervisors, _WsName, 0) ->
    NodeDefs;
supervisor_filter_nodes([], NodeDefs, [], _WsName, _Limit) ->
    NodeDefs;
supervisor_filter_nodes([], NodeDefs, RedoSupervisors, _WsName, Limit) ->
    supervisor_filter_nodes(RedoSupervisors, NodeDefs, [], _WsName, Limit - 1);
supervisor_filter_nodes(
    [SupNodeDef | MoreSupNodeDefs],
    NodeDefs,
    RedoSupervisors,
    WsName,
    Limit
) ->
    Module = node_type_to_module(SupNodeDef),

    case Module:extract_nodes(SupNodeDef, NodeDefs, WsName) of
        {ok, ReducedNodeDefs} ->
            supervisor_filter_nodes(
                MoreSupNodeDefs,
                ReducedNodeDefs,
                RedoSupervisors,
                WsName,
                Limit - 1
            );
        {error, _} ->
            supervisor_filter_nodes(
                MoreSupNodeDefs,
                NodeDefs,
                [SupNodeDef | RedoSupervisors],
                WsName,
                Limit - 1
            )
    end.

%%
%% This can be used by a supervisor to revive a dead process.
spin_up_node(NodeDef, WsName) ->
    {ok, IdStr} = maps:find(id, NodeDef),
    {ok, TypeStr} = maps:find(type, NodeDef),

    %% here have to respect the 'd' (disabled) attribute. if true, then
    %% the node does not need to have a Pid created for it.
    Module = node_type_to_module(TypeStr, disabled(NodeDef)),

    GrpName = get_node_name(WsName, IdStr),

    clear_pg_group(GrpName),

    %% The Module:start calls the ered_node:start(..) function which registers
    %% the PID with the corresponding pg group
    {ok, Pid} = Module:start(add_state(NodeDef, GrpName), WsName),

    %% Perform any Post spawn activities. This is mostly registering with
    %% collectors of events as subscribers, see catch and complete nodes
    %% for examples. This is done after the spawn because to register
    %% with the various servers, a Pid is needed and that is not known
    %% when a node is initialised via the Module:start.
    gen_server:call(Pid, {registered, WsName, Pid}),

    {ok, Pid}.

%% Used by the supervisor node to restart/start nodes.
spin_up_and_link_node(NodeDef, WsName) ->
    {ok, Pid} = spin_up_node(NodeDef, WsName),
    erlang:link(Pid),
    % this flag indicates that a supervisor node should fall down if
    % its supervisor process dies, that's because the supervisor node
    % is being started by another supervisor. that supervisor is responsible
    % for restarting this node when it goes down. Also this function is
    % exclusively used by supervisor nodes, so its safe to set the
    % flag here. The top-level supervisor is started by the spin_up_nodes
    % function and hence does not have this flag.
    gen_server:call(Pid, {being_supervised, WsName}),

    {ok, Pid}.

%%
%% Squirrel away all config nodes to the config store for later retrieval
%% by nodes that use the config nodes.
store_config_nodes([], _WsName) ->
    ok;
store_config_nodes([NodeDef | OtherNodeDefs], WsName) ->
    case is_config_node(maps:get(type, NodeDef)) of
        true ->
            ered_config_store:store_config_node(
                maps:get(id, NodeDef),
                NodeDef
            );
        _ ->
            ignore
    end,
    store_config_nodes(OtherNodeDefs, WsName).

%%
%% tab node usses 'disabled', everything else 'd'.
disabled(NodeDef) ->
    case maps:find(d, NodeDef) of
        {ok, V} ->
            V;
        _ ->
            case maps:find(disabled, NodeDef) of
                {ok, V} ->
                    V;
                _ ->
                    false
            end
    end.

%%
%% The wires attribute is an array of arrays. The toplevel
%% array has one entry per port that a node has. (Port being the connectors
%% from which wires leave the node.)
%% If a node only has one port, then wires will be an array containing
%% exactly one array. This is the [Val] case and is the most common case.
%%
send_msg_to_connected_nodes(NodeDef, Msg) ->
    case maps:find(wires, NodeDef) of
        {ok, [Val]} ->
            send_msg_on(Val, Msg);
        {ok, Val} ->
            send_msg_on(Val, Msg)
    end.

%%
%% Avoid having to create the same case all the time.
%%
get_prop_value_from_map(Prop, Map, Default) when is_binary(Prop) ->
    get_prop_value_from_map(binary_to_atom(Prop), Map, Default);
get_prop_value_from_map(Prop, Map, Default) ->
    case maps:find(Prop, Map) of
        {ok, Val} ->
            case Val of
                <<"">> -> Default;
                "" -> Default;
                _ -> Val
            end;
        _ ->
            Default
    end.

get_prop_value_from_map(Prop, Map) ->
    get_prop_value_from_map(Prop, Map, "").

%%
%% Helper for passing on messages once a node has completed with the message
%%

%% Here 'on' is the same 'pass on' not as in 'on & off'. Standing on the
%% shoulders of great people is also not the same 'on' as here.
send_msg_on([], _) ->
    ok;
send_msg_on([NodeId | Wires], Msg) ->
    NodePid = nodeid_to_pid(ws_from(Msg), NodeId),

    case NodePid of
        {error, _} ->
            ignore;
        {ok, Pid} ->
            gen_server:cast(Pid, {incoming, Msg})
    end,
    send_msg_on(Wires, Msg).

%%
%%
send_msg_on_by_pids([], _) ->
    ok;
send_msg_on_by_pids([Pid | Wires], Msg) ->
    gen_server:cast(Pid, {incoming, Msg}),
    send_msg_on_by_pids(Wires, Msg).

%%
%% Lookup table for mapping node type to function. Also here we respect the
%% disabled flag: if node is disabled, give it a noop node else continue
%% on checking by type.
%%

node_type_to_module(_Type, true) ->
    ered_node_disabled;
node_type_to_module(Type, _) ->
    node_type_to_module(Type).

%%
%% Mapping more Node-RED Node Type to Erlang node module. Required because
%% some node types map to the same module, i.e. ered_node_ignore.
%%
%% erlfmt:ignore alignment.
node_type_to_module(NodeDef) when is_map(NodeDef) ->
    node_type_to_module(maps:get(type,NodeDef));
node_type_to_module(<<"inject">>)            -> ered_node_inject;
node_type_to_module(<<"switch">>)            -> ered_node_switch;
node_type_to_module(<<"debug">>)             -> ered_node_debug;
node_type_to_module(<<"junction">>)          -> ered_node_junction;
node_type_to_module(<<"change">>)            -> ered_node_change;
node_type_to_module(<<"link out">>)          -> ered_node_link_out;
node_type_to_module(<<"link in">>)           -> ered_node_link_in;
node_type_to_module(<<"link call">>)         -> ered_node_link_call;
node_type_to_module(<<"delay">>)             -> ered_node_delay;
node_type_to_module(<<"file in">>)           -> ered_node_file_in;
node_type_to_module(<<"json">>)              -> ered_node_json;
node_type_to_module(<<"template">>)          -> ered_node_template;
node_type_to_module(<<"join">>)              -> ered_node_join;
node_type_to_module(<<"split">>)             -> ered_node_split;
node_type_to_module(<<"catch">>)             -> ered_node_catch;
node_type_to_module(<<"comment">>)           -> ered_node_ignore;
node_type_to_module(<<"tab">>)               -> ered_node_ignore;
node_type_to_module(<<"complete">>)          -> ered_node_complete;
node_type_to_module(<<"group">>)             -> ered_node_ignore;
node_type_to_module(<<"status">>)            -> ered_node_status;
node_type_to_module(<<"trigger">>)           -> ered_node_trigger;
node_type_to_module(<<"http in">>)           -> ered_node_http_in;
node_type_to_module(<<"http response">>)     -> ered_node_http_response;
node_type_to_module(<<"http request">>)      -> ered_node_http_request;
node_type_to_module(<<"mqtt in">>)           -> ered_node_mqtt_in;
node_type_to_module(<<"mqtt out">>)          -> ered_node_mqtt_out;
node_type_to_module(<<"exec">>)              -> ered_node_exec;
node_type_to_module(<<"function">>)          -> ered_node_function;
node_type_to_module(<<"markdown">>)          -> ered_node_markdown;
node_type_to_module(<<"csv">>)               -> ered_node_csv;
node_type_to_module(<<"FlowHubPull">>)       -> ered_node_flowhub_pull;
node_type_to_module(<<"erlsupervisor">>)     -> ered_node_erlsupervisor;
node_type_to_module(<<"Sink">>)              -> ered_node_ignore;
node_type_to_module(<<"Seeker">>)            -> ered_node_ignore;
node_type_to_module(<<"erlmodule">>)         -> ered_node_erlmodule;
node_type_to_module(<<"erlstatemachine">>)   -> ered_node_erlstatemachine;
node_type_to_module(<<"erleventhandler">>)   -> ered_node_erleventhandler;
node_type_to_module(<<"mermaid-flowchart">>) -> ered_node_ignore;
%%
%% Assert nodes for testing functionality of the nodes. These are the first
%% Node-RED and Erlang-RED nodes - they have implmentations for both because
%% they confirm the compatiability between Node-RED and ErlangRED.
%%
node_type_to_module(<<"ut-assert-values">>)  -> ered_node_assert_values;
node_type_to_module(<<"ut-assert-failure">>) -> ered_node_assert_failure;
node_type_to_module(<<"ut-assert-success">>) -> ered_node_assert_success;
node_type_to_module(<<"ut-assert-status">>)  -> ered_node_assert_status;
node_type_to_module(<<"ut-assert-debug">>)   -> ered_node_assert_debug;
node_type_to_module(Unknown) ->
    case is_config_node(Unknown) of
        false ->
            io:format("noop node initiated for unknown type: ~p\n", [Unknown]),
            ered_node_noop;
        _ ->
            ered_node_ignore
    end.

%% A list of all nodes that support outgoing messages, this was originally
%% only the inject node but then I realised that for testing purposes there
%% are in fact more.
trigger_outgoing_messages({ok, <<"inject">>}, {ok, IdStr}, WsName) ->
    case nodeid_to_pid(WsName, IdStr) of
        {ok, Pid} ->
            gen_server:cast(Pid, create_outgoing_msg(WsName));
        {error, _} ->
            ignore
    end;
trigger_outgoing_messages(_, _, _) ->
    ok.

%%
%% List of all known config nodes that need to be stored away before
%% a flow is executed
%% erlfmt:ignore alignment
is_config_node(<<"mqtt-broker">>)        -> true;
is_config_node(<<"FlowCompareCfg">>)     -> true;
is_config_node(<<"Flow2MermaidCfg">>)    -> true;
is_config_node(<<"FlowHubCfg">>)         -> true;
is_config_node(<<"websocket-listener">>) -> true;
is_config_node(_)                        -> false.

%%
%% this is also used by the supervisor node, hence the extra support.
is_supervisor(NodeDef) when is_map(NodeDef) ->
    is_supervisor(maps:get(type, NodeDef));
is_supervisor(<<"erlsupervisor">>) ->
    true;
is_supervisor(_) ->
    false.

%%
%% module nodes are started before everything else because they define
%% code modules needed for the statemachine node (for example).
is_module_node(NodeDef) when is_map(NodeDef) ->
    is_module_node(maps:get(type, NodeDef));
is_module_node(<<"erlmodule">>) ->
    true;
is_module_node(_) ->
    false.
