-module(node_assert_debug).

%%
%% Assert node for checking whether another node generated a debug
%% message for the debug panel.
%%
-export([node_assert_debug/1]).
-export([handle_ws_event/2]).
-export([handle_stop/2]).

-import(node_receivership, [enter_receivership/3]).
-import(node_assert_status, [post_failure/3]).

is_same(A, A) -> true;
is_same(_, _) -> false.

handle_stop(NodeDef, WsName) ->
    case maps:find('_mc_websocket', NodeDef) of
        {ok, 0} ->
            case maps:find(inverse, NodeDef) of
                {ok, false} ->
                    {ok, NodeId} = maps:find(nodeid, NodeDef),
                    ErrMsg = nodes:jstr("Expected debug from ~p\n", [NodeId]),
                    post_failure(NodeDef, WsName, ErrMsg);
                _ ->
                    success
            end;
        _ ->
            success
    end,
    NodeDef.

handle_ws_event(NodeDef, {debug, WsName, NodeId, Type, _Data}) ->
    case maps:find(inverse, NodeDef) of
        {ok, true} ->
            ErrMsg = nodes:jstr("No debug expected from ~p\n", [NodeId]),
            post_failure(NodeDef, WsName, ErrMsg);
        _ ->
            {ok, ExpType} = maps:find(msgtype, NodeDef),
            io:format("Exp ~p v. ~p~n", [ExpType, Type]),

            case is_same(binary_to_atom(ExpType), Type) of
                true ->
                    success;
                _ ->
                    ErrMsg = nodes:jstr(
                        "debug type mismatch ~s != ~p", [ExpType, Type]
                    ),
                    post_failure(NodeDef, WsName, ErrMsg)
            end
    end,
    NodeDef;
handle_ws_event(NodeDef, _) ->
    NodeDef.

%%
%%
node_assert_debug(NodeDef) ->
    nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, websocket_events_and_stop).
