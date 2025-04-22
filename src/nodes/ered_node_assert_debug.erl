-module(ered_node_assert_debug).

-export([node_assert_debug/2]).
-export([handle_event/2]).
-export([handle_ws_event/2]).

%%
%% Assert node for checking whether another node generated a debug
%% message for the debug panel.
%%

-import(ered_node_receivership, [enter_receivership/3]).

-import(ered_nodered_comm, [
    assert_failure/3,
    node_status/5
]).
-import(ered_nodes, [
    jstr/2
]).
-import(ered_ws_event_exchange, [
    subscribe/5
]).

is_same(A, A) -> true;
is_same(_, _) -> false.

%%
%%
handle_event({registered, WsName, _Pid}, NodeDef) ->
    {ok, TgtNodeId} = maps:find(nodeid, NodeDef),
    {ok, MsgType} = maps:find(msgtype, NodeDef),
    {ok, NodePid} = maps:find('_node_pid_', NodeDef),
    %%
    %% inverse means that there should be **no** debug message, this
    %% means that this node needs to subscribe too all types of debug
    %% messages.
    case maps:find(inverse, NodeDef) of
        {ok, true} ->
            ered_ws_event_exchange:subscribe(
                WsName,
                TgtNodeId,
                debug,
                any,
                NodePid
            );
        _ ->
            ered_ws_event_exchange:subscribe(
                WsName,
                TgtNodeId,
                debug,
                MsgType,
                NodePid
            )
    end,
    NodeDef;
handle_event({stop, WsName}, NodeDef) ->
    case maps:find('_mc_websocket', NodeDef) of
        {ok, 0} ->
            case maps:find(inverse, NodeDef) of
                {ok, false} ->
                    {ok, NodeId} = maps:find(nodeid, NodeDef),
                    ErrMsg = jstr("Expected debug from ~p\n", [NodeId]),
                    assert_failure(NodeDef, WsName, ErrMsg);
                _ ->
                    node_status(
                        WsName, NodeDef, "assert succeed", "green", "ring"
                    )
            end;
        _ ->
            node_status(WsName, NodeDef, "assert succeed", "green", "ring")
    end,
    {ok, NodePid} = maps:find('_node_pid_', NodeDef),
    ered_ws_event_exchange:unsubscribe(WsName, NodePid),
    NodeDef;
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_ws_event({debug, WsName, NodeId, Type, _Data}, NodeDef) ->
    case maps:find(inverse, NodeDef) of
        {ok, true} ->
            ErrMsg = jstr("No debug expected from ~p\n", [NodeId]),
            assert_failure(NodeDef, WsName, ErrMsg);
        _ ->
            {ok, ExpType} = maps:find(msgtype, NodeDef),
            io:format("Exp ~p v. ~p~n", [ExpType, Type]),

            case is_same(binary_to_atom(ExpType), Type) of
                true ->
                    success;
                _ ->
                    ErrMsg = jstr(
                        "debug type mismatch ~s != ~p", [ExpType, Type]
                    ),
                    assert_failure(NodeDef, WsName, ErrMsg)
            end
    end,
    NodeDef;
handle_ws_event(_, NodeDef) ->
    NodeDef.

%%
%%
node_assert_debug(NodeDef, _WsName) ->
    enter_receivership(?MODULE, NodeDef, websocket_events_and_stop).
