-module(ered_msgtracer_helpers).

-export([
    do_msgtrace_for_node/3,
    send_off_debug/2
]).

%%
%% Implement the functionality of the tracer functionality.
%%
%% Message tracing can be activated without having to redeploy the flow, so
%% it great for debugging live issues. Also useful for debugging flows in
%% design phase but I usually use this in live systems.
%%

-import(ered_nodered_comm, [
    node_status/5,
    node_status_clear/2,
    send_on_if_ws/2,
    send_to_debug_sidebar/2,
    ws_from/1
]).

-import(ered_nodes,[
   jstr/2
]).

%%
%%
send_off_debug(NodeDef, Msg) ->
    send_off_debug(maps:get(<<"type">>,NodeDef), NodeDef, Msg).

%%
%%
send_off_debug(<<"junction">>,_N,_M) ->
    % junctions cause errors when sent to the debug panel, also they
    % cannot tbe highlighted in the flow editor.
    ignore;
send_off_debug(_Type, NodeDef, Msg) ->
    send_to_debug_sidebar(NodeDef, Msg).

%%
%%
do_msgtrace_for_node(NodeDef, Pid, State) ->
    WsName = binary_to_atom(ws_from(State)),

    send_on_if_ws(WsName, {msgtracing, maps:get(<<"id">>, NodeDef)}),
    node_status(WsName, NodeDef, jstr("~p msg received",[Pid]), "green", "ring"),

    % no need to keep the entire NodeDef when clearing the status - only
    % need the id of the node
    NodeId = #{ <<"id">> => maps:get(<<"id">>, NodeDef) },
    spawn(fun() -> clear_status_after_one_sec(WsName, NodeId) end).

%%
%%
clear_status_after_one_sec(WsName, NodeId) ->
    timer:sleep(1000),
    node_status_clear(WsName, NodeId).
