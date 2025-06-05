-module(ered_node_erlstatemachine).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Implement the gen_statem behaviour.
%%

-import(ered_nodes, [
    send_msg_to_connected_nodes/2
]).

-import(ered_nodered_comm, [
    node_status/5,
    node_status_clear/2
]).

%%
%%
start(NodeDef, WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event({registered, WsName, _MyPid}, NodeDef) ->
    ModuleName = binary_to_atom(maps:get(module_name, NodeDef)),

    {ok, {Pid, Ref}} = gen_statem:start_monitor(ModuleName, [], []),
    io:format("DDD: {{{ ~p }} {{ ~p }}~n",[Pid, Ref]),
    node_status(WsName, NodeDef, "started", "green", "dot"),

    maps:put('_statem_pid', Pid, NodeDef);

handle_event({'DOWN', _Ref, process, _Pid, shutdown}, NodeDef) ->
    maps:remove('_statem_pid', NodeDef);

handle_event(Evnt, NodeDef) ->
    io:format( "Event: ~p~n",[Evnt]),
    NodeDef;

handle_event(_, NodeDef) ->
    NodeDef.


%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    State = binary_to_atom(maps:get(payload, Msg)),
    Pid = maps:get('_statem_pid', NodeDef),

    R = gen_statem:call(Pid, State),
    io:format("DD : {{ ~p }}~n", [R]),

    Msg2 = maps:put(payload, R, Msg),
    send_msg_to_connected_nodes(NodeDef, Msg2),

    {handled, NodeDef, Msg2};

handle_msg(UnknownMsg, NodeDef) ->
    io:format( "DDEvent: ~p~n",[UnknownMsg]),
    {unhandled, NodeDef}.
