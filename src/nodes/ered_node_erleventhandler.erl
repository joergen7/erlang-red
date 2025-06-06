-module(ered_node_erleventhandler).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Event handler node for implementing the gen_event behaviour
%%

-import(ered_nodered_comm, [
    node_status/5,
    node_status_clear/2,
    ws_from/1
]).

-import(ered_nodes, [
    send_msg_to_connected_nodes/2
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event({registered, WsName, _MyPid}, NodeDef) ->
    {ok, {Pid, _Ref}} = gen_event:start_monitor(),
    node_status(WsName, NodeDef, "started", "green", "dot"),
    maps:put('_eventh_pid', Pid, NodeDef);
handle_event({'DOWN', _Ref, process, _Pid, shutdown}, NodeDef) ->
    maps:remove('_eventh_pid', NodeDef);
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    EventHandlerPid = maps:get('_eventh_pid', NodeDef),
    case maps:find(action, Msg) of
        {ok, <<"add_handler">>} ->
            R = gen_event:add_handler(
                EventHandlerPid,
                binary_to_atom(maps:get(payload, Msg)),
                []
            ),
            io:format( "DDD : {{ ~p }}~n", [R]),
            Msg2 = Msg#{payload => R},
            send_msg_to_connected_nodes(NodeDef, Msg2);
        {ok, <<"delete_handler">>} ->
            R = gen_event:delete_handler(
                EventHandlerPid,
                binary_to_atom(maps:get(payload, Msg)),
                []
            ),
            io:format( "DDD : {{ ~p }}~n", [R]),
            Msg2 = Msg#{payload => R},
            send_msg_to_connected_nodes(NodeDef, Msg2);
        _ ->
            case maps:find(event, Msg) of
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
