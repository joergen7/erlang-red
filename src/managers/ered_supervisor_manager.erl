-module(ered_supervisor_manager).

-behaviour(supervisor).

-export([
    init/1,
    start_link/3
]).

%%
%% Supervisor manager that manages the supervisor that is created for a
%% supervisor node. This manager actually supervises the nodes that the
%% supervisor node is configured for. Then there is another supervisior
%% that supervisors this manager.
%%

start_link(NodePid, NodeDef, Children) ->
    MgrId = binary_to_atom(
        list_to_binary(
            io_lib:format(
                "supervisor_for_~s",
                [maps:get(id, NodeDef)]
            )
        )
    ),

    whereis(MgrId) =/= undefined andalso
        is_process_alive(whereis(MgrId)) andalso
        gen_server:stop(MgrId),

    {ok, Pid} = supervisor:start_link(
        {local, MgrId},
        ?MODULE,
        [NodePid, NodeDef, Children]
    ),

    NodePid ! {supervisor_node, {supervisor_started, Pid}},
    NodePid ! {supervisor_node, {monitor_this_process, Pid}},
    {ok, Pid}.

init([NodePid, NodeDef, Children]) ->
    SupOpts = #{
        strategy => binary_to_atom(maps:get(strategy, NodeDef)),
        intensity => binary_to_integer(maps:get(intensity, NodeDef)),
        period => binary_to_integer(maps:get(period, NodeDef)),
        auto_shutdown => binary_to_atom(maps:get(auto_shutdown, NodeDef))
    },
    {ok, {SupOpts, Children}}.
