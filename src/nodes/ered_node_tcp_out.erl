-module(ered_node_tcp_out).

-behaviour(ered_node).

-include("ered_nodes.hrl").

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Tcp Out node
%%
%% {
%%     "id": "25d34eb00072bf33",
%%     "type": "tcp out",
%%     "z": "2a95a7b40a798878",
%%     "name": "",
%%     "host": "",
%%     "port": "",
%%     "beserver": "reply", <<---- send responses to tcp-in events
%%     "base64": false, <<---- decode base64 content
%%     "end": false,  <<---- close a connection as soon as a message has been sent
%%     "tls": "",
%%     "x": 1342,
%%     "y": 440.5,
%%     "wires": []
%% }

-import(ered_nodes, [
    check_config/4,
    jstr/2
]).

-import(ered_nodered_comm, [
    send_out_debug_msg/4,
    unsupported/3,
    ws_from/1
]).

%%
%%
% erlfmt:ignore - alignment
start(NodeDef, WsName) ->
    %% TODO: this is the todo list for this node - support the various
    %% TODO: configuration possibilities.
    case {
          check_config(<<"beserver">>, <<"reply">>, NodeDef, WsName),
          check_config(<<"tls">>,      <<"">>,      NodeDef, WsName),
          check_config(<<"host">>,     <<"">>,      NodeDef, WsName),
          check_config(<<"port">>,     <<"">>,      NodeDef, WsName),
          check_config(<<"base64">>,   false,       NodeDef, WsName),
          check_config(<<"end">>,      false,       NodeDef, WsName)
         } of
        {ok, ok, ok, ok, ok, ok} ->
            ered_node:start(?PUT_WS(NodeDef), ?MODULE);
        _ ->
            %% seemlessly return an ignore node if the configuration doesn't
            %% match. This allows other flows and nodes to continue to work
            %% just this instance of the tcp-out node is ignored.
            ered_node:start(NodeDef, ered_node_ignore)
    end.

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

handle_msg({incoming, Msg}, NodeDef) ->
    case maps:get(<<"beserver">>, NodeDef) of
        <<"reply">> ->
            handle_reply_mode(Msg, NodeDef);
        Unsupported ->
            ErrMsg = jstr("unsupported beserver mode ~p", [Unsupported]),
            unsupported(NodeDef, Msg, ErrMsg)
    end,
    {handled, NodeDef, dont_send_complete_msg};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%% ---------------- Helpers

%%
%%
handle_reply_mode(Msg, NodeDef) ->
    case maps:find(<<"_session">>, Msg) of
        {ok, TcpSession} ->
            case check_tcp_session(TcpSession) of
                {ok, SessionId} ->
                    ered_tcp_manager:send(
                        SessionId,
                        maps:get(<<"payload">>, Msg)
                    ),
                    handle_reset(maps:find(<<"reset">>, Msg), SessionId);
                {error, ErrMsg} ->
                    send_out_debug_msg(NodeDef, Msg, ErrMsg, error)
            end;
        _ ->
            ErrMsg = jstr("Message contains no Tcp Session", []),
            send_out_debug_msg(NodeDef, Msg, ErrMsg, error)
    end.

%%
%%
check_tcp_session(#{<<"status">> := <<"closed">>}) ->
    {error, <<"cannot reply, tcp session closed">>};
check_tcp_session(#{<<"id">> := SessionId}) ->
    {ok, SessionId}.

%%
%%
handle_reset({ok, true}, SessionId) ->
    ered_tcp_manager:close(SessionId);
handle_reset({ok, <<"true">>}, SessionId) ->
    ered_tcp_manager:close(SessionId);
handle_reset({ok, "true"}, SessionId) ->
    ered_tcp_manager:close(SessionId);
handle_reset(_, _) ->
    ignore.
