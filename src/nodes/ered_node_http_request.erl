-module(ered_node_http_request).

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
    unsupported/3
]).

-import(ered_message_exchange, [
    post_exception/3
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    Url = get_prop_from_nodedef_or_msg(url, NodeDef, Msg),
    Method =
        case maps:get(method, NodeDef) of
            <<"use">> ->
                get_prop_value_from_map(method, Msg);
            _ ->
                get_prop_from_nodedef_or_msg(method, NodeDef, Msg)
        end,

    case {Method, Url} of
        {[], []} ->
            unsupported(NodeDef, Msg, "url and method not set"),
            {handled, NodeDef, Msg};
        {[], _} ->
            unsupported(NodeDef, Msg, "method not set"),
            {handled, NodeDef, Msg};
        {_, []} ->
            unsupported(NodeDef, Msg, "url not set"),
            {handled, NodeDef, Msg};
        _ ->
            case httpc:request(mth_to_atom(Method), {Url, []}, [], []) of
                {ok, {{_, StatusCode, _ReasonPhrase}, _, Body}} ->
                    Msg2 = maps:put(statusCode, StatusCode, Msg),
                    Msg3 = maps:put(payload, jstr(Body), Msg2),
                    send_msg_to_connected_nodes(NodeDef, Msg3),
                    {handled, NodeDef, Msg3};
                {error, Reason} ->
                    ErrMsg = jstr("Http request failed: ~p", [Reason]),
                    case post_exception(NodeDef, Msg, ErrMsg) of
                        dealt_with ->
                            ok;
                        _ ->
                            send_out_debug_msg(NodeDef, Msg, ErrMsg, error)
                    end,
                    {handled, NodeDef, Msg}
            end
    end;
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%
mth_to_atom(Method) when is_binary(Method) ->
    binary_to_atom(string:lowercase(Method));
mth_to_atom(Method) when is_list(Method) ->
    list_to_atom(string:lowercase(Method));
mth_to_atom(Method) ->
    Method.

%%
%% get prop from NodeDef or Msg
get_prop_from_nodedef_or_msg(PropName, NodeDef, Msg) ->
    case maps:find(PropName, Msg) of
        {ok, MsgUrl} ->
            get_prop_value_from_map(PropName, NodeDef, MsgUrl);
        _ ->
            get_prop_value_from_map(PropName, NodeDef)
    end.
