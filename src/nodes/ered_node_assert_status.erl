-module(ered_node_assert_status).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Assert node for checking whether another node generated a status
%% update for itself. Or not.
%%

-import(ered_nodes, [
    jstr/2
]).
-import(ered_nodered_comm, [
    assert_failure/3,
    node_status/5
]).
-import(ered_ws_event_exchange, [
    subscribe/4
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

is_same({A, A}) -> true;
is_same({_, _}) -> false.

check_attributes({ExpTxt, Txt}) ->
    case is_same({ExpTxt, Txt}) of
        false ->
            case ExpTxt of
                %% ignore this check if expected text is empty.
                <<>> -> [];
                _ -> jstr("Content mismatch ~p\n", [{ExpTxt, Txt}])
            end;
        true ->
            []
    end.

check_attributes(Shp, Txt) ->
    case is_same(Shp) of
        false ->
            [
                jstr("Shape mismatch ~p\n", [Shp]) | [check_attributes(Txt)]
            ];
        true ->
            [check_attributes(Txt)]
    end.

check_attributes(Clr, Shp, Txt) ->
    case is_same(Clr) of
        false ->
            [
                jstr("Colour mismatch ~p\n", [Clr])
                | [check_attributes(Shp, Txt)]
            ];
        true ->
            [check_attributes(Shp, Txt)]
    end.

%%
%%
handle_event({registered, WsName, _Pid}, NodeDef) ->
    case maps:find(<<"nodeid">>, NodeDef) of
        {ok, TgtNodeId} ->
            {ok, NodePid} = maps:find('_node_pid_', NodeDef),
            ered_ws_event_exchange:subscribe(
                WsName, TgtNodeId, status, NodePid
            );
        _ ->
            ignore_missing_value
    end,
    NodeDef;
handle_event({stop, WsName}, NodeDef) ->
    case maps:find('_mc_websocket', NodeDef) of
        {ok, 0} ->
            case maps:find(<<"inverse">>, NodeDef) of
                {ok, false} ->
                    {ok, NodeId} = maps:find(<<"nodeid">>, NodeDef),
                    ErrMsg = jstr("Expected status from ~p\n", [NodeId]),
                    assert_failure(NodeDef, WsName, ErrMsg);
                _ ->
                    node_status(
                        WsName,
                        NodeDef,
                        "assert succeed",
                        "green",
                        "ring"
                    )
            end;
        _ ->
            node_status(
                WsName,
                NodeDef,
                "assert succeed",
                "green",
                "ring"
            )
    end,
    {ok, NodePid} = maps:find('_node_pid_', NodeDef),
    ered_ws_event_exchange:unsubscribe(WsName, NodePid),
    NodeDef;
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_websocket({status, WsName, NodeId, Txt, Clr, Shp}, NodeDef) ->
    case maps:find(<<"inverse">>, NodeDef) of
        {ok, true} ->
            ErrMsg = jstr("No status expected from ~p\n", [NodeId]),
            assert_failure(NodeDef, WsName, ErrMsg);
        _ ->
            {ok, ExpClr} = maps:find(<<"colour">>, NodeDef),
            {ok, ExpShp} = maps:find(<<"shape">>, NodeDef),
            {ok, ExpTxt} = maps:find(<<"content">>, NodeDef),
            Errors = check_attributes(
                {ExpClr, list_to_binary(Clr)},
                {ExpShp, list_to_binary(Shp)},
                {ExpTxt, list_to_binary(Txt)}
            ),
            case lists:flatten(Errors) of
                [] ->
                    success;
                _ ->
                    ErrMsg = list_to_binary(Errors),
                    assert_failure(NodeDef, WsName, ErrMsg)
            end
    end,
    NodeDef;
handle_websocket(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg({ws_event, Details}, NodeDef) ->
    {handled, handle_websocket(Details, NodeDef), empty};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
