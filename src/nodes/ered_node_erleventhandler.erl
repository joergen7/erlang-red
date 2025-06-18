-module(ered_node_erleventhandler).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Event handler node for implementing the gen_event behaviour
%%
%% A very basic implementation that does not support passing arguments to
%% the add_handler/3 call nor id value in the name of the handler. That
%% is just so.
%%

-import(ered_nodered_comm, [
    node_status/5,
    node_status_clear/2,
    post_exception_or_debug/3,
    ws_from/1
]).

-import(ered_nodes, [
    jstr/1,
    send_msg_to_connected_nodes/2
]).

%%
%%
start(NodeDef, WsName) ->
    ered_node:start(NodeDef#{'_ws' => WsName}, ?MODULE).

%%
%%
handle_event({registered, WsName, _MyPid}, NodeDef) ->
    {ok, {Pid, _Ref}} = gen_event:start_monitor(),
    case add_handlers(Pid, maps:find(<<"handlers">>, NodeDef)) of
        ok ->
            node_status(WsName, NodeDef, "started", "green", "dot");
        {error, ErrorList} ->
            [
                post_exception_or_debug(
                    NodeDef, #{'_ws' => WsName}, jstr(ErrMsg)
                )
             || ErrMsg <- ErrorList
            ],
            node_status(WsName, NodeDef, "invalid", "blue", "ring")
    end,
    NodeDef#{'_eventh_pid' => Pid};
handle_event({'DOWN', _Ref, process, _Pid, shutdown}, NodeDef) ->
    node_status(ws_from(NodeDef), NodeDef, "stopped", "red", "dot"),
    maps:remove('_eventh_pid', NodeDef);
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    EventHandlerPid = maps:get('_eventh_pid', NodeDef),
    case maps:find(<<"action">>, Msg) of
        {ok, <<"add_handler">>} ->
            R = gen_event:add_handler(
                EventHandlerPid,
                binary_to_atom(maps:get(<<"payload">>, Msg)),
                []
            ),
            Msg2 = Msg#{<<"payload">> => R},
            send_msg_to_connected_nodes(NodeDef, Msg2);
        {ok, <<"delete_handler">>} ->
            R = gen_event:delete_handler(
                EventHandlerPid,
                binary_to_atom(maps:get(<<"payload">>, Msg)),
                []
            ),
            Msg2 = Msg#{<<"payload">> => R},
            send_msg_to_connected_nodes(NodeDef, Msg2);
        _ ->
            %% debatable - use 'payload' as event name or use a separate
            %% attribute is being done now. I guess my preference is clear.
            case maps:find(<<"event">>, Msg) of
                {ok, EventName} ->
                    gen_event:notify(
                        EventHandlerPid, {EventName, Msg, NodeDef}
                    );
                _ ->
                    ignore
            end
    end,
    {handled, NodeDef, dont_send_complete_msg};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%
add_handlers(_EventHandlerPid, error) ->
    % missing list, no static configuration
    ok;
add_handlers(EventHandlerPid, {ok, Handlers}) ->
    add_handlers(EventHandlerPid, {ok, Handlers}, []).

%%
%% Collect together all errors - nothing worse than fixing one error and then
%% being confronted with the next even though the system knew that already.
add_handlers(_EventHandlerPid, {ok, []}, []) ->
    % empty handler list and empty error list, all done
    ok;
add_handlers(_EventHandlerPid, {ok, []}, ErrorList) ->
    % empty handler list and empty error list, all done
    {error, ErrorList};
add_handlers(EventHandlerPid, {ok, [Hndlr | MoreHndlrs]}, ErrorList) ->
    NodeId = maps:get(<<"nodeid">>, Hndlr),

    case ered_erlmodule_exchange:find_module(NodeId) of
        not_found ->
            Error = io_lib:format(
                "Module for NodeId ~p not found",
                [NodeId]
            ),
            add_handlers(EventHandlerPid, {ok, MoreHndlrs}, [Error | ErrorList]);
        {ok, ModuleName} ->
            case code:is_loaded(ModuleName) of
                false ->
                    Error = io_lib:format(
                        "Module ~p (~p) is not loaded",
                        [NodeId, ModuleName]
                    ),
                    add_handlers(
                        EventHandlerPid,
                        {ok, MoreHndlrs},
                        [Error | ErrorList]
                    );
                _ ->
                    gen_event:add_handler(
                        EventHandlerPid,
                        ModuleName,
                        []
                    ),
                    add_handlers(EventHandlerPid, {ok, MoreHndlrs}, ErrorList)
            end
    end.
