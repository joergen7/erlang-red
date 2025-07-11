-module(ered_node_assert_status).

-include("ered_nodes.hrl").

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

-define(TestSuccess, '_failed_' := false).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef#{'_failed_' => false}, ?MODULE).

%%
%%
handle_event(
    {registered, WsName, _Pid},
    #{?NodePid, <<"nodeid">> := TgtNodeId} = NodeDef
) ->
    ered_ws_event_exchange:subscribe(WsName, TgtNodeId, status, NodePid),
    NodeDef;
handle_event({registered, _WsName, _Pid}, NodeDef) ->
    NodeDef;
%%
handle_event(
    {stop, WsName},
    #{
        '_mc_websocket' := 0,
        <<"inverse">> := false,
        <<"nodeid">> := NodeId,
        ?TestSuccess,
        ?NodePid
    } = NodeDef
) ->
    ErrMsg = jstr("Expected status from ~p\n", [NodeId]),
    assert_failure(NodeDef, WsName, ErrMsg),
    ered_ws_event_exchange:unsubscribe(WsName, NodePid),
    NodeDef;
handle_event(
    {stop, WsName},
    #{?TestSuccess, ?NodePid} = NodeDef
) ->
    node_status(WsName, NodeDef, "assert succeed", "green", "ring"),
    ered_ws_event_exchange:unsubscribe(WsName, NodePid),
    NodeDef;
handle_event(
    {stop, WsName},
    #{?NodePid} = NodeDef
) ->
    ered_ws_event_exchange:unsubscribe(WsName, NodePid),
    NodeDef;
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_websocket(
    {status, WsName, NodeId, _Txt, _Clr, _Shp},
    #{
        <<"inverse">> := true
    } = NodeDef
) ->
    ErrMsg = jstr("No status expected from ~p\n", [NodeId]),
    assert_failure(NodeDef, WsName, ErrMsg),
    NodeDef#{'_failed_' => true};
handle_websocket(
    {status, WsName, _NodeId, Txt, Clr, Shp},
    #{
        <<"colour">> := ExpClr,
        <<"shape">> := ExpShp,
        <<"content">> := ExpTxt,
        <<"inverse">> := false
    } = NodeDef
) ->
    Errors = check_attributes(
        {ExpClr, list_to_binary(Clr)},
        {ExpShp, list_to_binary(Shp)},
        {ExpTxt, Txt}
    ),
    case lists:flatten(Errors) of
        [] ->
            NodeDef;
        _ ->
            ErrMsg = list_to_binary(Errors),
            assert_failure(NodeDef, WsName, ErrMsg),
            NodeDef#{'_failed_' => true}
    end;
handle_websocket(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg({ws_event, Details}, NodeDef) ->
    {handled, handle_websocket(Details, NodeDef), empty};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%  ----------------------- helpers
%%
is_same({A, A}) -> true;
is_same({_, _}) -> false.

check_attributes({ExpTxt, Txt}) when is_list(Txt) ->
    check_attributes({ExpTxt, list_to_binary(Txt)});
check_attributes({ExpTxt, Txt}) when is_binary(Txt) ->
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
