-module(ered_node_http_request).

-include("ered_nodes.hrl").

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Node is responsible for making external http requests and feeding the
%% results into the flow.
%%
-import(ered_nodes, [
    get_prop_value_from_map/2,
    get_prop_value_from_map/3,
    jstr/1,
    jstr/2,
    send_msg_to_connected_nodes/2
]).

-import(ered_nodered_comm, [
    send_out_debug_msg/4,
    post_exception_or_debug/3,
    unsupported/3
]).

-import(ered_nodered_comm, [
    ws_from/1
]).

%%
%%
start(#{<<"headers">> := []} = NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE);
start(#{<<"headers">> := _Headers} = NodeDef, WsName) ->
    unsupported(NodeDef, {websocket, WsName}, "header specs not supported"),
    ered_node:start(NodeDef, ered_node_ignore).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg({incoming, Msg}, #{<<"method">> := NodeMeth} = NodeDef) ->
    unsupported_headers_warnings(NodeDef, Msg),

    Url = get_prop_from_nodedef_or_msg(<<"url">>, NodeDef, Msg),
    Method =
        case NodeMeth of
            <<"use">> ->
                get_prop_value_from_map(<<"method">>, Msg);
            _ ->
                get_prop_from_nodedef_or_msg(<<"method">>, NodeDef, Msg)
        end,

    case {Method, Url} of
        {[], []} ->
            post_exception_or_debug(NodeDef, Msg, <<"url and method not set">>),
            {handled, NodeDef, Msg};
        {[], _} ->
            post_exception_or_debug(NodeDef, Msg, <<"method not set">>),
            {handled, NodeDef, Msg};
        {_, []} ->
            post_exception_or_debug(NodeDef, Msg, <<"url not set">>),
            {handled, NodeDef, Msg};
        _ ->
            case perform_request(Method, Url, NodeDef, Msg) of
                {ok, {{_, StatusCode, _ReasonPhrase}, _, Body}} ->
                    Msg2 = Msg#{
                        <<"statusCode">> => StatusCode,
                        ?AddPayload(jstr(Body))
                    },
                    send_msg_to_connected_nodes(NodeDef, Msg2),
                    {handled, NodeDef, Msg2};
                {error, Reason} ->
                    ErrMsg = jstr("Http request failed: ~p", [Reason]),
                    post_exception_or_debug(NodeDef, Msg, ErrMsg),
                    {handled, NodeDef, Msg}
            end
    end;
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%% -------------- helpers
%%
unsupported_headers_warnings(NodeDef, Msg) ->
    case
        {
            maps:find(<<"headers">>, NodeDef),
            maps:find(<<"headers">>, Msg)
        }
    of
        {{ok, []}, {ok, []}} ->
            ok;
        {{ok, []}, error} ->
            ok;
        {{ok, []}, {ok, _T}} ->
            unsupported(NodeDef, Msg, "message: headers unsupported");
        {{ok, _S}, {ok, []}} ->
            unsupported(NodeDef, Msg, "node definition: headers unsupported");
        {{ok, _S}, error} ->
            unsupported(NodeDef, Msg, "node definition: headers unsupported");
        {{ok, _S}, {ok, _T}} ->
            unsupported(NodeDef, Msg, "node definition: headers unsupported"),
            unsupported(NodeDef, Msg, "message: headers unsupported")
    end.

mth_to_atom(Method) when is_binary(Method) ->
    binary_to_atom(string:lowercase(Method)).

%%
%% get prop from NodeDef or Msg
get_prop_from_nodedef_or_msg(PropName, NodeDef, Msg) ->
    case maps:find(PropName, Msg) of
        {ok, MsgUrl} ->
            get_prop_value_from_map(PropName, NodeDef, MsgUrl);
        _ ->
            get_prop_value_from_map(PropName, NodeDef)
    end.

perform_request(Method = <<"POST">>, Url, _NodeDef, #{?GetWsName} = Msg) ->
    case maps:find(<<"payload">>, Msg) of
        {ok, Payload} ->
            httpc:request(
                mth_to_atom(Method),
                {Url,
                    [
                        {"Cookie", io_lib:format("wsname=~s", [WsName])}
                    ],
                    "application/json", Payload},
                [],
                []
            );
        _ ->
            httpc:request(
                mth_to_atom(Method),
                {Url,
                    [
                        {"Cookie", io_lib:format("wsname=~s", [WsName])}
                    ],
                    "application/json", ""},
                [],
                []
            )
    end;
perform_request(Method = <<"GET">>, Url, NodeDef, Msg) ->
    case maps:find(<<"paytoqs">>, NodeDef) of
        {ok, <<"ignore">>} ->
            httpc:request(
                mth_to_atom(Method),
                {Url, [
                    {"Cookie", io_lib:format("wsname=~s", [ws_from(Msg)])}
                ]},
                [],
                []
            );
        _ ->
            ErrMsg = jstr("unsuported config for payload handling", []),
            post_exception_or_debug(NodeDef, Msg, ErrMsg)
    end;
perform_request(_Method, _Url, NodeDef, Msg) ->
    unsupported(NodeDef, Msg, "unsuported method"),
    {error, unsupported_method}.
