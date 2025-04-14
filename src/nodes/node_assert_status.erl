-module(node_assert_status).

%%
%% Assert node for checking whether another node generated a status
%% update for itself. Or not.
%%
-export([node_assert_status/1]).
-export([handle_ws_event/2]).
-export([handle_stop/2]).

-import(node_receivership, [enter_receivership/3]).

-export([post_failure/3]).

is_same({A, A}) -> true;
is_same({_, _}) -> false.

check_attributes({ExpTxt, Txt}) ->
    case is_same({ExpTxt, Txt}) of
        false ->
            case ExpTxt of
                %% ignore this check if expected text is empty.
                <<>> -> [];
                _ -> nodes:jstr("Content mismatch ~p\n", [{ExpTxt, Txt}])
            end;
        true ->
            []
    end.

check_attributes(Shp, Txt) ->
    case is_same(Shp) of
        false ->
            [
                nodes:jstr("Shape mismatch ~p\n", [Shp])
                | [check_attributes(Txt)]
            ];
        true ->
            [check_attributes(Txt)]
    end.

check_attributes(Clr, Shp, Txt) ->
    case is_same(Clr) of
        false ->
            [
                nodes:jstr("Colour mismatch ~p\n", [Clr])
                | [check_attributes(Shp, Txt)]
            ];
        true ->
            [check_attributes(Shp, Txt)]
    end.

%% erlfmt:ignore equals and arrows should line up here.
post_failure(NodeDef,WsName,ErrMsg) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),

    nodes:this_should_not_happen(NodeDef,ErrMsg),

    IdStr       = nodes:get_prop_value_from_map(id,NodeDef),
    ZStr        = nodes:get_prop_value_from_map(z,NodeDef),
    NameStr     = nodes:get_prop_value_from_map(name,NodeDef,TypeStr),

    Data = #{
             id       => IdStr,
             z        => ZStr,
             '_alias' => IdStr,
             path     => ZStr,
             name     => NameStr,
             msg      => nodes:jstr(ErrMsg),
             format   => <<"string">>
            },

    nodered:debug(WsName,Data,error),
    nodered:node_status(WsName,NodeDef, "assert failed", "red", "dot").

%%
%%
handle_stop(NodeDef, WsName) ->
    case maps:find('_mc_websocket', NodeDef) of
        {ok, 0} ->
            case maps:find(inverse, NodeDef) of
                {ok, false} ->
                    {ok, NodeId} = maps:find(nodeid, NodeDef),
                    ErrMsg = nodes:jstr("Expected status from ~p\n", [NodeId]),
                    post_failure(NodeDef, WsName, ErrMsg);
                _ ->
                    success
            end;
        _ ->
            success
    end,
    NodeDef.

%%
%%
%% erlfmt:ignore equals and arrows should line up here.
handle_ws_event(NodeDef, {status, WsName, NodeId, Txt, Clr, Shp}) ->
    case maps:find(inverse, NodeDef) of
        {ok, true} ->
            ErrMsg = nodes:jstr("No status expected from ~p\n",[NodeId]),
            post_failure(NodeDef,WsName,ErrMsg);

        _ ->
            {ok, ExpClr} = maps:find(colour,  NodeDef),
            {ok, ExpShp} = maps:find(shape,   NodeDef),
            {ok, ExpTxt} = maps:find(content, NodeDef),
            Errors = check_attributes({ExpClr,list_to_binary(Clr)},
                                      {ExpShp,list_to_binary(Shp)},
                                      {ExpTxt,list_to_binary(Txt)}),
            case lists:flatten(Errors) of
                [] ->  success;
                _ ->
                    ErrMsg = list_to_binary(Errors),
                    post_failure(NodeDef,WsName,ErrMsg)
            end
    end,
    NodeDef;

handle_ws_event(NodeDef,_) ->
    NodeDef.

%%
%%
node_assert_status(NodeDef) ->
    nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, websocket_events_and_stop).
