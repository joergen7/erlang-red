-module(ered_node_debug).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Debug nodes has no outgoing wires.
%% Debug node dumps messages to the debug panel in the flow editor.
%% It's data is transmitted via a websocket to the browser.
%%

-import(ered_nodered_comm, [
    debug/3,
    node_status/5,
    send_to_debug_sidebar/2,
    unsupported/3,
    ws_from/1
]).
-import(ered_nodes, [
    get_prop_value_from_map/2,
    get_prop_value_from_map/3,
    jstr/2
]).
-import(ered_msg_handling, [
    retrieve_prop_value/2
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_status_setting({ok, true}, {ok, <<"msg">>}, NodeDef, Msg) ->
    {ok, PropName} = maps:find(statusVal, NodeDef),
    Val = retrieve_prop_value(PropName, Msg),
    node_status(ws_from(Msg), NodeDef, Val, "grey", "dot");
handle_status_setting({ok, true}, {ok, <<"counter">>}, NodeDef, Msg) ->
    Cnt = get_prop_value_from_map('_mc_incoming', NodeDef),
    node_status(ws_from(Msg), NodeDef, Cnt, "blue", "ring");
handle_status_setting({ok, false}, _, _, _) ->
    ok;
handle_status_setting({ok, true}, {ok, StatusType}, NodeDef, Msg) ->
    unsupported(NodeDef, Msg, jstr("StatusType: ~p", [StatusType])).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_incoming(NodeDef, Msg) ->
    case maps:find(console, NodeDef) of
        {ok, true} ->
            NodeName = get_prop_value_from_map(
                name,
                NodeDef,
                "undefined"
            ),
            io:format("DEBUG [~s]: ~p\n", [NodeName, Msg]);
        _ ->
            ignore
    end,

    case maps:find(tosidebar, NodeDef) of
        {ok, true} ->
            case maps:find(active, NodeDef) of
                {ok, true} ->
                    send_to_debug_sidebar(NodeDef, Msg);
                _ ->
                    not_active_no_output
            end;
        _ ->
            not_to_sidebar
    end,

    handle_status_setting(
        maps:find(tostatus, NodeDef),
        maps:find(statusType, NodeDef),
        NodeDef,
        Msg
    ),

    {NodeDef, Msg}.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    {NodeDef2, Msg2} = handle_incoming(NodeDef, Msg),
    {handled, NodeDef2, Msg2};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
