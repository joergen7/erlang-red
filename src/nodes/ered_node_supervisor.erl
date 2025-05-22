-module(ered_node_supervisor).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

-export([init/1]).
-export([extract_nodes/2]).

%%
%% Supervisor for restarting processes that die unexpectedly.
%%
%% For the time being, this supervisor node implements a dynamic supervisor.

-import(ered_nodes, [
    is_supervisor/1,
    jstr/1
]).

-import(ered_nodered_comm, [
    node_status/5,
    unsupported/3,
    ws_from/1
]).

%%
%%
start(NodeDef, WsName) ->
    node_status(WsName, NodeDef, "starting", "green", "ring"),
    ered_node:start(maps:put('_ws', WsName, NodeDef), ?MODULE).

%% erlfmt:ignore alignment
init(NodeDef) ->
    {ok, {
          #{
            strategy      => binary_to_atom(maps:get(strategy,NodeDef)),
            intensity     => binary_to_integer(maps:get(intensity,NodeDef)),
            period        => binary_to_integer(maps:get(period,NodeDef)),
            auto_shutdown => binary_to_atom(maps:get(auto_shutdown,NodeDef))
      }, []}}.

%%
%% Extract nodes will do a number of things:
%%  - remove all nodes from the list of NodeDefs that are managed by this
%%    supervisor.
%%  - having removed the nodes, the supervisor then spins them up using
%%    ered_nodes:spin_up_node/2 and manage them. The nodes are still
%%    registered so that they can communicate with other processes
%%  - return a list of NodeDefs which no longer contain those nodes that
%%    this supervisor manages.
-spec extract_nodes(
    Supervisor :: supervisor:sup_ref(),
    NodeDefs :: [map()]
) -> [map()].
extract_nodes(Supervisor, NodeDefs) ->
    % this gen_server:call goes via the ered_node:handle_call/3 function
    % before reaching `handle_event({filter_nodes,...` below.
    gen_server:call(Supervisor, {filter_nodes, NodeDefs}).

%%
%%
check_config(NodeDef) ->
    check_config(
        maps:get(strategy, NodeDef),
        maps:get(auto_shutdown, NodeDef),
        maps:get(supervisor_type, NodeDef)
    ).
%%
check_config(_A, <<"any_significant">>, _B) -> {no, "auto shutdown"};
check_config(_A, <<"all_significant">>, _B) -> {no, "auto shutdown"};
check_config(_A, _B, <<"static">>) -> {no, "static supervisor type"};
check_config(<<"simple_one_for_one">>, _A, _B) -> {no, "simple one-to-one"};
check_config(_Strategy, _AutoShutdown, _SupervisorType) -> ok.

%%
%%
handle_event({filter_nodes, NodeDefs}, SupNodeDef) ->
    WsName = ws_from(SupNodeDef),

    case check_config(SupNodeDef) of
        {no, ErrMsg} ->
            unsupported(SupNodeDef, {websocket, WsName}, ErrMsg),
            self() ! {stop, WsName},
            {NodeDefs, SupNodeDef};
        _ ->
            case filter_nodedefs(maps:get(scope, SupNodeDef), NodeDefs) of
                {ok, {RestNodeDefs, MyNodeDefs}} ->
                    io:format("DD {{ ~p }} ~p~n", [
                        lists:any(fun ered_nodes:is_supervisor/1, MyNodeDefs),
                        MyNodeDefs
                    ]),
                    case
                        lists:any(fun ered_nodes:is_supervisor/1, MyNodeDefs)
                    of
                        true ->
                            ErrMsg = "supervisor of supervisor not supported",
                            unsupported(
                                SupNodeDef, {websocket, WsName}, ErrMsg
                            ),
                            self() ! {stop, WsName},
                            {NodeDefs, SupNodeDef};
                        _ ->
                            SupNodeDef2 = create_children(
                                MyNodeDefs,
                                SupNodeDef,
                                WsName
                            ),
                            node_status(
                                WsName,
                                SupNodeDef,
                                "started",
                                "green",
                                "dot"
                            ),
                            {RestNodeDefs, SupNodeDef2}
                    end;
                {error, ErrMsg} ->
                    % TODO: group and flow are both not supported, although
                    % TODO: flow would be easy since it would imply all the
                    % TODO: nodedefs while group are all the nodes with the
                    % TODO: same 'g' value as the supervisor
                    unsupported(SupNodeDef, {websocket, WsName}, ErrMsg),
                    self() ! {stop, WsName},
                    {NodeDefs, SupNodeDef}
            end
    end;
handle_event({registered, _WsName, _Pid}, NodeDef) ->
    % Remove those nodes that this supervisor is managing.
    io:format("Supervisor told to register~n", []),
    NodeDef;
handle_event({stop, WsName}, NodeDef) ->
    node_status(WsName, NodeDef, "stopped", "red", "dot"),
    case maps:find('_super_ref', NodeDef) of
        {ok, SupRef} ->
            gen_server:stop(SupRef),
            maps:remove('_super_ref', NodeDef);
        _ ->
            NodeDef
    end;
handle_event(Event, NodeDef) ->
    io:format("Did get event ~p~n", [Event]),
    NodeDef.
%% handle_event(_, NodeDef) ->
%%     NodeDef.

%%
%%
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%
filter_nodedefs(<<"flow">>, _NodeDefs) ->
    {error, "scope flow"};
filter_nodedefs(<<"group">>, _NodeDefs) ->
    {error, "scope group"};
filter_nodedefs(Scope, NodeDefs) when is_list(Scope) ->
    {ok, filter_nodedefs_by_ids(Scope, NodeDefs)};
filter_nodedefs(_, _) ->
    {error, "unknown"}.

%%
%% Filter the NodeDefs by Id given a list of NodeIds for which this
%% node will act as supervisor.
filter_nodedefs_by_ids(LstOfNodeIds, NodeDefs) ->
    filter_nodedefs_by_ids(LstOfNodeIds, NodeDefs, [], []).
filter_nodedefs_by_ids(_, [], RestNodes, MyNodes) ->
    {RestNodes, MyNodes};
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
%%
start_supervisor(NodeDef) ->
    case supervisor:start_link(?MODULE, NodeDef) of
        {ok, Pidd} ->
            Pidd;
        {error, {already_started, Piddd}} ->
            gen_server:stop(Piddd),
            start_supervisor(NodeDef)
    end.

%%
%%
create_children(MyNodeDefs, SupNodeDef, WsName) ->
    SupRef = start_supervisor(SupNodeDef),
    SupNodeId = maps:get('_node_pid_', SupNodeDef),

    io:format(
        "Supervisor told to filter nodes PID: ~p ==> ~p~n",
        [self(), SupRef]
    ),

    StartChild = fun(NodeDef) ->
        ChildId = binary_to_atom(
            list_to_binary(
                io_lib:format(
                    "child_~s_~s",
                    [SupNodeId, maps:get(id, NodeDef)]
                )
            )
        ),

        R = supervisor:start_child(SupRef, #{
            id => ChildId,
            start => {
                ered_nodes, super_spin_up_node, [NodeDef, WsName]
            },
            restart => permanent,
            shutdown => brutal_kill,
            type => worker
        }),
        io:format("STAET CRT: ~p ==> ~p~n", [R, NodeDef])
    end,

    [StartChild(NodeDef) || NodeDef <- MyNodeDefs],

    maps:put('_super_ref', SupRef, SupNodeDef).
