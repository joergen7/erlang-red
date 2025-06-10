-module(ered_node_erlsupervisor).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

-export([init/1]).
-export([extract_nodes/3]).

%%
%% Supervisor for restarting processes that die unexpectedly.
%%
%% "type": "erlsupervisor",
%% "scope": [         <<--- this can also be "group" or "flow"
%%     "874cb18b3842747d",
%%     "84927e2b99bfc27b"
%% ],
%% "supervisor_type": "static", <<--- or "dynamic"
%% "strategy": "one_for_all", <<--+- as desribed in the OTP docu
%% "auto_shutdown": "never", <<--/
%% "intensity": "5",     <<-----/
%% "period": "30",     <<------/
%% "child_type": "worker",
%% "child_restart": "permanent",
%% "child_shutdown": "brutal_kill",  <<--- if this timeout then
%% "child_shutdown_timeout": "",  <<<---- this value is relevant
%%
%% This node is a "manager" of a manager. The architecture of this node
%% isn't that simple. First off there is a process for the node but only
%% if it can be initialised, i.e., it has a valid configuration and its
%% nodes (i.e. children) are available.
%%
%% Once the node is ready, it's spun up using the start/2 call. Once that
%% has happened it will receive a "registered" event and spins up its children
%% using the ered_supervisor_manager - which is the supervisor responsible
%% for starting and restarting the children. It implements the configuration
%% of the node.
%%
%% The ered_supervisor_manager dies when the intensity is reached. This death
%% would take this node with it (supervisors always being linked) but since
%% that is not desirable, there is another supervisr between this node and
%% the ered_supervisor_manager. It buffers the death of the manager and
%% not much more. It does not perform a restart of the manager because its
%% restart strategy is temporary.
%%
%% This buffer supervisor is created in the create_children/3 call. This call
%% is also used when an incoming message requests the restart of the supervisor.
%% create_children/3 will stop any exising "buffer" supervisor and also
%% restart the ered_supervisor_manager.
%%
%% This works fine if this supervisor node is not being supervised by another
%% supervisor, i.e., supervisor-of-supervisor pattern. The problem there is
%% the death of the ered_supervisor_manager has to be propagated up the
%% supervisor chain. But this node does everything to prevent itself from
%% dying when the supervisor dies. In a supervisor-of-supervisor pattern,
%% this node needs to die since the supervisor is supervising the node
%% process NOT the process of the supervisor that dies when the children die.
%%
%% Luckily what happens is that this node receives a message when the supervisor
%% goes down - the "{'DOWN', ...}" message. It receievs this message to alter
%% its status - from started to dead. What we do is set a flag on the NodeDef
%% of this node to tell it to go down if and only if its supervisor goes down.
%% We only set this flag if the supervisor node is being supervised.
%%
%% When the node goes down, the supervisor supervising it, restarts the entire
%% node. Simple really.
%%
-import(ered_nodes, [
    is_supervisor/1,
    jstr/1,
    send_msg_to_connected_nodes/2
]).

-import(ered_nodered_comm, [
    node_status/5,
    unsupported/3,
    ws_from/1
]).

-import(ered_msg_handling, [
    convert_to_num/1,
    create_outgoing_msg/1
]).

-define(SEND_STATUS(Status),
    send_msg_to_connected_nodes(
        NodeDef,
        maps:put(status, Status, element(2, create_outgoing_msg(WsName)))
    )
).

%%
%%
start(NodeDef, WsName) ->
    node_status(WsName, NodeDef, "starting", "green", "ring"),
    ered_node:start(maps:put('_ws', WsName, NodeDef), ?MODULE).

%% erlfmt:ignore alignment
init(Children) ->
    {ok, {
          % these settings are irrelevant because the single child of the
          % supervisor goes down once and stays down. The fight is rigged.
          % (because the restart strategy is temporary).
          #{
            strategy      => one_for_one,
            intensity     => 1,
            period        => 5,
            auto_shutdown => never
      }, Children}}.

%%
%%
handle_event({registered, WsName, _MyPid}, NodeDef) ->
    case maps:find('_my_node_defs', NodeDef) of
        {ok, Children} ->
            create_children(Children, NodeDef, WsName);
        _ ->
            NodeDef
    end;
handle_event({being_supervised, _WsName}, NodeDef) ->
    process_flag(trap_exit, true),
    NodeDef;
handle_event({stop, WsName}, NodeDef) ->
    node_status(WsName, NodeDef, "stopped", "red", "dot"),
    ?SEND_STATUS(<<"stopped">>),

    % _super_ref is set in the monitor_this_process event and that Pid
    % is the supervisor process that is actually doing the work.
    case maps:find('_super_ref', NodeDef) of
        {ok, SupRef} ->
            is_process_alive(SupRef) andalso exit(SupRef, normal),

            maps:remove('_super_ref', NodeDef);
        _ ->
            NodeDef
    end;
handle_event({'EXIT', _From, normal}, NodeDef) ->
    % Hey, what? We got taken down - most probably from another supervisor
    WsName = ws_from(NodeDef),
    node_status(WsName, NodeDef, "dead", "blue", "ring"),
    NodeDef;
handle_event({'EXIT', _From, shutdown}, NodeDef) ->
    % Hey, what? We got taken down - most probably from another supervisor
    WsName = ws_from(NodeDef),
    node_status(WsName, NodeDef, "stopped", "red", "dot"),
    NodeDef;
handle_event({'DOWN', _, process, _Pid, shutdown}, NodeDef) ->
    % Our supervisor died, i.e., the children misbehaved and the supervisor
    % packed up. This is literally the supervisor hitting the intensity wall.
    WsName = ws_from(NodeDef),

    node_status(WsName, NodeDef, "dead", "blue", "ring"),
    ?SEND_STATUS(<<"dead">>),

    case maps:find('_sup_pid_', NodeDef) of
        {ok, SupPid} ->
            is_process_alive(SupPid) andalso exit(SupPid, normal);
        _ ->
            ignore
    end,

    case maps:find('_being_supervised', NodeDef) of
        {ok, true} ->
            self() ! {stop, WsName};
        _ ->
            ignore
    end,
    NodeDef;
handle_event({supervisor_started, SupRef}, NodeDef) ->
    % This event is generated by the the ered_supervisor_manager module
    % once it has spun up the supervisor that actually supervises the nodes.
    WsName = ws_from(NodeDef),
    node_status(WsName, NodeDef, "started", "green", "dot"),
    ?SEND_STATUS(<<"started">>),

    %% SupRef is the Pid of the actual supervisor doing the supervision of
    %% the children - monitor it and if it goes down, we need to know.
    erlang:monitor(process, SupRef),
    maps:put('_super_ref', SupRef, NodeDef);
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    WsName = ws_from(Msg),

    case maps:get(action, Msg) of
        <<"restart">> ->
            case maps:find('_my_node_defs', NodeDef) of
                {ok, MyNodeDefs} ->
                    NewNodeDef = create_children(MyNodeDefs, NodeDef, WsName),
                    ?SEND_STATUS(<<"restarted">>),
                    {handled, NewNodeDef, Msg};
                _ ->
                    ErrMsg = "restart action",
                    unsupported(
                        NodeDef, {websocket, WsName}, ErrMsg
                    ),
                    self() ! {stop, WsName},
                    {handled, NodeDef, Msg}
            end;
        _ ->
            {handled, NodeDef, Msg}
    end;
handle_msg(Msg, NodeDef) ->
    io:format("UNHANDLED MSG: Supervisor: {{{ ~p }}}~n", [Msg]),
    {unhandled, NodeDef}.

%%
%% ------- Helpers
%%
create_children(MyNodeDefs, SupNodeDef, WsName) ->
    SupNodeId = maps:get('_node_pid_', SupNodeDef),

    StartChild = fun(NodeDef) ->
        ChildId = binary_to_atom(
            list_to_binary(
                io_lib:format(
                    "child_~s_~s",
                    [SupNodeId, maps:get(id, NodeDef)]
                )
            )
        ),

        #{
            id => ChildId,
            start => {
                ered_nodes, spin_up_and_link_node, [NodeDef, WsName]
            },
            restart => cf_child_restart(
                maps:get(child_restart, SupNodeDef)
            ),
            shutdown => cf_child_shutdown(
                maps:get(child_shutdown, SupNodeDef),
                maps:get(child_shutdown_timeout, SupNodeDef),
                NodeDef
            ),
            type => cf_child_type(
                maps:get(child_type, SupNodeDef),
                NodeDef
            )
        }
    end,

    %% this supervisor ensures that this node does not go down when the
    %% supervisor supervising the nodes goes down.
    %% The configuraton for this supervisor is the init/1 function of this
    %% module.
    %%
    %% The single child is a supervisor so the shutdown policy is infinity
    %% to avoid an error message from the gen_server about being killed since
    %% the supervisor behaviour is a descendant of the gen_server behaviour.
    {ok, SuperPid} = supervisor:start_link(?MODULE, [
        #{
            id => binary_to_atom(
                list_to_binary(
                    io_lib:format("supervisor_manager_~s", [SupNodeId])
                )
            ),
            start => {
                ered_supervisor_manager,
                start_link,
                [
                    self(),
                    SupNodeDef,
                    [StartChild(NodeDef) || NodeDef <- MyNodeDefs]
                ]
            },
            restart => temporary,
            shutdown => infinity,
            type => supervisor
        }
    ]),

    maps:put(
        '_sup_pid_',
        SuperPid,
        maps:put('_my_node_defs', MyNodeDefs, SupNodeDef)
    ).

%%
%% ------------- pre spin up configuration setup
%%
%% The functionality beyond here is related to configuring supervisor nodes
%% before these are spun up. This functionality checks the configuration of the
%% supervisor node and extracts the nodes from a list of nodes. This is called
%% from the ered_nodes:create_pid_for_node/2 function.
%%
%%
check_config(NodeDef) ->
    check_config(
        maps:get(strategy, NodeDef),
        maps:get(auto_shutdown, NodeDef),
        maps:get(supervisor_type, NodeDef)
    ).

check_config(_Strategy, <<"any_significant">>, _SupervisorType) ->
    {no, "auto shutdown"};
check_config(_Strategy, <<"all_significant">>, _SupervisorType) ->
    {no, "auto shutdown"};
check_config(_Strategy, _AutoShutdown, <<"dynamic">>) ->
    {no, "dynamic supervisor type"};
check_config(<<"simple_one_for_one">>, _AutoShutdown, _SupervisorType) ->
    {no, "simple one-to-one"};
check_config(_Strategy, _AutoShutdown, _SupervisorType) ->
    ok.

%%
%%
filter_nodedefs(<<"flow">>, _NodeDefs) ->
    {error, "scope flow"};
filter_nodedefs(<<"group">>, _NodeDefs) ->
    {error, "scope group"};
filter_nodedefs(Scope, NodeDefs) when is_list(Scope) ->
    filter_nodedefs_by_ids(Scope, NodeDefs);
filter_nodedefs(_, _) ->
    {error, "unknown"}.

%%
%% Filter the NodeDefs by Id given a list of NodeIds for which this
%% node will act as supervisor.
filter_nodedefs_by_ids(LstOfNodeIds, NodeDefs) ->
    filter_nodedefs_by_ids(LstOfNodeIds, NodeDefs, [], []).
filter_nodedefs_by_ids(LstOfNodeIds, [], RestNodes, MyNodes) ->
    % order the nodes for this supervisor in the order of the IDs defined
    % in the scope list. This defines the start-up and shutdown order and
    % is for rest-for-one restart policy important.
    case {length(MyNodes), length(LstOfNodeIds)} of
        {Same, Same} ->
            Lookup = lists:map(fun(E) -> {maps:get(id, E), E} end, MyNodes),
            OrderMyNodes = lists:map(
                fun(E) ->
                    element(2, lists:keyfind(E, 1, Lookup))
                end,
                LstOfNodeIds
            ),
            {ok, {RestNodes, OrderMyNodes}};
        {_But, _Different} ->
            {not_all_nodes_found, "not all nodes found"}
    end;
filter_nodedefs_by_ids(
    LstOfNodeIds,
    [NodeDef | OtherNodeDefs],
    RestNodes,
    MyNodes
) ->
    case lists:member(maps:get(id, NodeDef), LstOfNodeIds) of
        true ->
            filter_nodedefs_by_ids(
                LstOfNodeIds,
                OtherNodeDefs,
                RestNodes,
                [NodeDef | MyNodes]
            );
        _ ->
            filter_nodedefs_by_ids(
                LstOfNodeIds,
                OtherNodeDefs,
                [NodeDef | RestNodes],
                MyNodes
            )
    end.

%%
%% From the list of Node definitions, remove all those nodes that are managed
%% by the Supervisor (defined by the SupNodeDef) and return the reduced list of
%% nodes including this supervisor definition.
%%
%% If something goes wrong, return the original list of node definitions.
extract_nodes(SupNodeDef, NodeDefs, WsName) ->
    case check_config(SupNodeDef) of
        {no, ErrMsg} ->
            unsupported(SupNodeDef, {websocket, WsName}, ErrMsg),
            node_status(
                WsName,
                SupNodeDef,
                "unsupported configuration",
                "yellow",
                "dot"
            ),
            {error, NodeDefs};
        _ ->
            case filter_nodedefs(maps:get(scope, SupNodeDef), NodeDefs) of
                {ok, {RestNodeDefs, MyNodeDefs}} ->
                    SupNodeDefWithNodes =
                        maps:put(
                            '_my_node_defs',
                            MyNodeDefs,
                            SupNodeDef
                        ),
                    {ok, [SupNodeDefWithNodes | RestNodeDefs]};
                {not_all_nodes_found, _ErrMsg} ->
                    %% Ignore this because we iterate through the list until
                    %% all nodes are found, supervisor of supervisor of
                    %% supervisor patterns uses this logic.
                    {error, NodeDefs};
                {error, ErrMsg} ->
                    % TODO: group and flow are both not supported, although
                    % TODO: flow would be easy since it would imply all the
                    % TODO: nodedefs while group are all the nodes with the
                    % TODO: same 'g' value as the supervisor
                    unsupported(SupNodeDef, {websocket, WsName}, ErrMsg),
                    node_status(
                        WsName,
                        SupNodeDef,
                        "unsupported configuration",
                        "yellow",
                        "dot"
                    ),
                    {error, NodeDefs}
            end
    end.

%%
%%
cf_child_restart(<<"temporary">>) -> temporary;
cf_child_restart(<<"transient">>) -> transient;
cf_child_restart(_) -> permanent.

%% Any child that is a supervisor and has no timeout set, it is assigned a
%% infinite timeout instead. Note: other child nodes, i.e. workers, aren't
%% affected by this.
cf_child_shutdown(ShutdownCfg, ShutdownTimeout, ChildNodeDef) ->
    case {ShutdownCfg, is_supervisor(ChildNodeDef)} of
        {<<"brutal_kill">>, true} ->
            infinity;
        _ ->
            cf_child_shutdown(ShutdownCfg, ShutdownTimeout)
    end.
cf_child_shutdown(<<"infinite">>, _) -> infinity;
cf_child_shutdown(<<"timeout">>, Timeout) -> convert_to_num(Timeout);
cf_child_shutdown(_, _Timeout) -> brutal_kill.

%% Any child that is a supervisor is set as supervisor regardless of the
%% configuration of the children configuraion found in supervisor node.
cf_child_type(SuperNodeConfigChildType, ChildNodeDef) ->
    case is_supervisor(ChildNodeDef) of
        true ->
            supervisor;
        false ->
            cf_child_type(SuperNodeConfigChildType)
    end.
cf_child_type(<<"supervisor">>) -> supervisor;
cf_child_type(_) -> worker.
