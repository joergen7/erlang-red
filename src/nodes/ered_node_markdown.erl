-module(ered_node_markdown).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Markdown node for converting Markdown content to HTML content.
%%
%% The node uses earmark for that via an Elixir bridge - cross over to the
%% other side via the bridge to nowhere, on the road to nowhere.
%%

-import(ered_nodes, [
    jstr/2,
    send_msg_to_connected_nodes/2
]).

-import(ered_nodered_comm, [
    post_exception_or_debug/3
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
    Bcontent = ensure_binary(maps:get(<<"payload">>, Msg)),

    try
        case 'Elixir.ErlangRedHelpers':markdown_to_html(Bcontent) of
            {ok, HtmlC, _} ->
                Msg2 = maps:put(<<"payload">>, HtmlC, Msg),
                send_msg_to_connected_nodes(NodeDef, Msg2),
                {handled, NodeDef, Msg2};
            Error ->
                ErrMsg = io_lib:format(
                    "Content: {{{ ~p }}} Error: ~p",
                    [Bcontent, Error]
                ),

                post_exception_or_debug(NodeDef, Msg, ErrMsg),
                {handled, NodeDef, Msg}
        end
    catch
        E:F ->
            ErrMsg2 = io_lib:format(
                "Stanza: {{{ ~p }}} Error: ~p:~p",
                [Bcontent, E, F]
            ),
            post_exception_or_debug(NodeDef, Msg, ErrMsg2),
            {handled, NodeDef, Msg}
    end;
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%
ensure_binary(V) when is_binary(V) ->
    V;
ensure_binary(V) when is_atom(V) ->
    erlang:atom_to_binary(V);
ensure_binary(V) when is_list(V) ->
    erlang:list_to_binary(V);
ensure_binary(V) ->
    V.
