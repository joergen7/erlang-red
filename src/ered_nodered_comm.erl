-module(ered_nodered_comm).

%%
%% Module for sending various websocket messages to Node-RED frontend
%%
%% Its the module that represents the encapsulates the communication between
%% nodes in Erlang and their representation in the flow editor.
%%
-export([
    assert_failure/3,
    debug/3,
    debug_string/2,
    debug_string/3,
    get_websocket_name/0,
    node_status_clear/2,
    node_status/5,
    send_on_if_ws/2,
    send_out_debug_msg/4,
    send_out_debug_error/2,
    send_to_debug_sidebar/2,
    unittest_result/3,
    unsupported/3,
    websocket_name_from_request/1,
    ws_from/1
]).

-import(ered_nodes, [
    generate_id/0,
    generate_id/1,
    get_prop_value_from_map/2,
    get_prop_value_from_map/3,
    jstr/1,
    this_should_not_happen/2
]).

send_on_if_ws(none, Msg) ->
    io:format("WARNING[nodered](none): not sending ~p~n", [Msg]);
send_on_if_ws(WsName, Msg) ->
    case whereis(WsName) of
        undefined ->
            io:format("WARNING[nodered](Wsname): not sending ~p ~p~n", [
                WsName, Msg
            ]);
        _ ->
            WsName ! Msg
    end.

%%
%% clear a previous node status value.
node_status_clear(WsName, NodeDef) ->
    {ok, NodeId} = maps:find(id, NodeDef),
    send_on_if_ws(WsName, {status, NodeId, clear}).

node_status(WsName, NodeDef, Txt, Clr, Shp) when is_integer(Txt) ->
    TxtStr = io_lib:format("~p", [Txt]),
    node_status(WsName, NodeDef, TxtStr, Clr, Shp);
node_status(WsName, NodeDef, Txt, Clr, Shp) ->
    {ok, NodeId} = maps:find(id, NodeDef),
    send_on_if_ws(WsName, {status, NodeId, Txt, Clr, Shp}).

debug(WsName, Data, error) ->
    send_on_if_ws(WsName, {debug, Data, error});
debug(WsName, Data, warning) ->
    send_on_if_ws(WsName, {debug, Data, warning});
debug(WsName, Data, notice) ->
    send_on_if_ws(WsName, {debug, Data, notice});
debug(WsName, Data, normal) ->
    send_on_if_ws(WsName, {debug, Data, normal}).

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
                  io_lib:format(
                    "Unsupported Feature: ~s NodeDef: ~p Msg: ~p",
                    [ErrMsg, NodeDef, Msg])
                 ),
      format => <<"string">>
     },

    debug(ws_from(Msg), Data, notice).

%% erlfmt:ignore lined up and to attention
send_out_debug_msg(NodeDef, Msg, ErrMsg, DebugType) ->
    TypeStr  = get_prop_value_from_map(type,  NodeDef),
    IdStr    = get_prop_value_from_map(id,    NodeDef),
    ZStr     = get_prop_value_from_map(z,     NodeDef),
    NameStr  = get_prop_value_from_map(name,  NodeDef, TypeStr),

    Data = #{
      id     => IdStr,
      z      => ZStr,
      path   => ZStr,
      name   => NameStr,
      msg    => ErrMsg,
      format => <<"string">>
     },

    debug(ws_from(Msg), Data, DebugType).

%% erlfmt:ignore lined up and to attention
send_out_debug_error(NodeDef, Msg) ->
    TypeStr  = get_prop_value_from_map(type,  NodeDef),
    IdStr    = get_prop_value_from_map(id,    NodeDef),
    ZStr     = get_prop_value_from_map(z,     NodeDef),
    NameStr  = get_prop_value_from_map(name,  NodeDef, TypeStr),

    Data = #{
      id     => IdStr,
      z      => ZStr,
      path   => ZStr,
      name   => NameStr,
      msg    => Msg,
      format => <<"object">>
     },

    debug(ws_from(Msg), Data, error).

%%
%%
%% erlfmt:ignore equals and arrows should line up here.
send_to_debug_sidebar(NodeDef,Msg) ->
    Type     = get_prop_value_from_map(type,  NodeDef),
    IdStr    = get_prop_value_from_map(id,    NodeDef),
    ZStr     = get_prop_value_from_map(z,     NodeDef),
    NameStr  = get_prop_value_from_map(name,  NodeDef, Type),
    TopicStr = get_prop_value_from_map(topic, Msg,     ""),

    %% format is important here.
    %% Triggery for large files and I don't know what. Using format
    %% of "object" as opposed to "Object" (capital-o) causes less
    %% breakage. Definitely something to investigate.
    %% See info for test id: c4690c0a085d6ef5 for more details.
    Data = #{
      id     => IdStr,
      z      => ZStr,
      path   => ZStr,
      name   => NameStr,
      topic  => to_binary_if_not_binary(TopicStr),
      msg    => Msg,
      format => <<"object">>
     },

    debug(ws_from(Msg), Data, normal).

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

%%
%%
to_binary_if_not_binary(Obj) when is_binary(Obj) ->
    Obj;
to_binary_if_not_binary(Obj) when is_list(Obj) ->
    list_to_binary(Obj);
to_binary_if_not_binary(Obj) ->
    Obj.
