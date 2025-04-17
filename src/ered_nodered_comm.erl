-module(ered_nodered_comm).

%%
%% Module for sending various websocket messages to Node-RED frontend
%%
%% Its the module that represents the encapsulates the communication beteen
%% nodes in Erlang and their representation in the flow editor.
%%
-export([node_status/5]).
-export([debug/3]).
-export([unittest_result/3]).
-export([get_websocket_name/0]).
-export([websocket_name_from_request/1]).

-export([debug_string/3]).
-export([debug_string/2]).
-export([ws_from/1]).

-export([unsupported/3]).
-export([assert_failure/3]).

-import(ered_nodes, [
    generate_id/0,
    generate_id/1,
    get_prop_value_from_map/2,
    get_prop_value_from_map/3,
    jstr/1,
    this_should_not_happen/2
]).

send_on_if_ws(none, Msg) ->
    io:format("WARNING[nodered](none): not sending ~p~n", [Msg]),
    ok;
send_on_if_ws(WsName, Msg) ->
    case whereis(WsName) of
        undefined ->
            io:format("WARNING[nodered](Wsname): not sending ~p ~p~n", [
                WsName, Msg
            ]),
            ok;
        _ ->
            WsName ! Msg
    end.

node_status(WsName, NodeDef, Txt, Clr, Shp) when is_integer(Txt) ->
    TxtStr = io_lib:format("~p", [Txt]),
    node_status(WsName, NodeDef, TxtStr, Clr, Shp);
node_status(WsName, NodeDef, Txt, Clr, Shp) ->
    {ok, NodeId} = maps:find(id, NodeDef),
    send_on_if_ws(WsName, {status, NodeId, Txt, Clr, Shp}).

debug(WsName, Data, error) ->
    send_on_if_ws(WsName, {error_debug, Data});
debug(WsName, Data, warning) ->
    send_on_if_ws(WsName, {warning_debug, Data});
debug(WsName, Data, notice) ->
    send_on_if_ws(WsName, {notice_debug, Data});
debug(WsName, Data, normal) ->
    send_on_if_ws(WsName, {debug, Data}).

unittest_result(WsName, FlowId, failed) ->
    send_on_if_ws(WsName, {unittest_results, FlowId, <<"failed">>});
unittest_result(WsName, FlowId, pending) ->
    send_on_if_ws(WsName, {unittest_results, FlowId, <<"pending">>});
unittest_result(WsName, FlowId, unknown_testcase) ->
    send_on_if_ws(WsName, {unittest_results, FlowId, <<"unknown_testcase">>});
unittest_result(WsName, FlowId, success) ->
    send_on_if_ws(WsName, {unittest_results, FlowId, <<"success">>}).

%%
%% Unsupported features are highlighted in the flow editor frontend. It makes
%% no sense to fail a unit test if this is hit. It's a gentle reminder that
%% Erlang-RED isn't feature complete.
%%
%% erlfmt:ignore lined up and to attention
unsupported(NodeDef, {websocket, WsName}, ErrMsg) ->
    unsupported(NodeDef, #{ '_ws' => WsName}, ErrMsg);
unsupported(NodeDef, Msg, ErrMsg) ->
    Type     = get_prop_value_from_map(type, NodeDef),
    IdStr    = get_prop_value_from_map(id,   NodeDef),
    ZStr     = get_prop_value_from_map(z,    NodeDef),
    NameStr  = get_prop_value_from_map(name, NodeDef, Type),

    Data = #{
      id     => IdStr,
      z      => ZStr,
      path   => ZStr,
      name   => NameStr,
      msg    => list_to_binary(
                  io_lib:format("Unsupported Feature: ~s",
                                [ErrMsg])
                 ),
      format => <<"string">>
     },

    debug(ws_from(Msg), Data, notice).

%%
%%
get_websocket_name() ->
    binary_to_atom(
        list_to_binary(io_lib:format("ws~s", [generate_id(6)]))
    ).

%%
%% Obtain the WsName from the HTTP Req or empty atom
websocket_name_from_request(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),

    case lists:keyfind(<<"wsname">>, 1, Cookies) of
        {<<"wsname">>, WsName} ->
            binary_to_atom(WsName);
        _ ->
            none
    end.

%%
%% Retrieve the websocket name from the Msg map.
ws_from(Msg) ->
    case maps:find('_ws', Msg) of
        {ok, Val} ->
            Val;
        _ ->
            none
    end.


%%
%% helpers
%% erlfmt:ignore the stars are lined up
debug_string(NodeDef,Msg) ->
    IdStr = get_prop_value_from_map(id,NodeDef),
    ZStr  = get_prop_value_from_map(z,NodeDef),
    debug_string(IdStr,ZStr,Msg).

%% erlfmt:ignore the stars are lined up
debug_string(NodeId,TabId,Msg) ->
    #{
      id     => NodeId,
      z      => TabId,
      path   => TabId,
      name   => <<"Unit Test Notice">>,
      topic  => <<"">>,
      msg    => Msg,
      format => <<"string">>
    }.

%% erlfmt:ignore equals and arrows should line up here.
assert_failure(NodeDef,WsName,ErrMsg) ->
    {ok, IdStr}   = maps:find(id,   NodeDef),
    {ok, TypeStr} = maps:find(type, NodeDef),

    this_should_not_happen(NodeDef, ErrMsg),

    IdStr   = get_prop_value_from_map(id,   NodeDef),
    ZStr    = get_prop_value_from_map(z,    NodeDef),
    NameStr = get_prop_value_from_map(name, NodeDef, TypeStr),

    Data = #{
             id       => IdStr,
             z        => ZStr,
             '_alias' => IdStr,
             path     => ZStr,
             name     => NameStr,
             msg      => jstr(ErrMsg),
             format   => <<"string">>
            },

    debug(WsName, Data, error),
    node_status(WsName, NodeDef, "assert failed", "red", "dot").
