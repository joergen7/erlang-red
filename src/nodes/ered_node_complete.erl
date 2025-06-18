-module(ered_node_complete).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% The complete node is a complex character. It raises the question "when is
%% a node complete?" - think here join node or switch node, for example.
%%
%% A join node isn't complete, it's handling of a subset of messages might
%% be completed but itself is never complete. Hence a join is configured to
%% collect four messages into a single message, will be completed when four
%% messages have arrived and once it sends out the message containing
%% those four messages, it has completed with the individual four messages.
%%
%% A swich node - does it do anything to a message? A node that does not
%% alter the message, does it have a completion?
%%
%% A switch raises no complete messages and a join node raises complete messages
%% once it is ready to send out a message. The wonderful world of the complete
%% node.
%%
%% Not forgetting the dangers of endless loops by listening to nodes that
%% are (directly or indirectly) connected to the complete node doing the
%% sending.
%%
%% Complete nodes can also listen to each other and cause endless loops.
%%
-import(ered_nodes, [
    send_msg_to_connected_nodes/2
]).
-import(ered_message_exchange, [
    subscribe_to_completed/3
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%% To avoid infinite loops by listening to nodes that are connected to this
%% complete node (nodes connected to those nodes, i.e. second or third degree
%% nodes), we mark the messages we handle and ignore those that come
%% back to us - like a bad boomarang ready to hit us in the head.
mark_msg(NodeDef, Msg) ->
    {ok, ThisId} = maps:find(<<"id">>, NodeDef),

    case maps:find('_complete_list', Msg) of
        {ok, Ary} ->
            case lists:member(ThisId, Ary) of
                true ->
                    already_handled_msg;
                false ->
                    {ok, maps:put('_complete_list', [ThisId | Ary], Msg)}
            end;
        _ ->
            {ok, maps:put('_complete_list', [ThisId], Msg)}
    end.

%%
%%
handle_event({registered, WsName, Pid}, NodeDef) ->
    subscribe_to_completed(NodeDef, WsName, Pid),
    NodeDef;
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_completed_msg(NodeDef, FromDef, Msg) ->
    {ok, FromId} = maps:find(<<"id">>, FromDef),
    {ok, Scope} = maps:find(<<"scope">>, NodeDef),

    case lists:member(FromId, Scope) of
        true ->
            case mark_msg(NodeDef, Msg) of
                {ok, MarkedMsg} ->
                    send_msg_to_connected_nodes(NodeDef, MarkedMsg),
                    {NodeDef, MarkedMsg};
                _ ->
                    {NodeDef, dont_send_complete_msg}
            end;
        _ ->
            {NodeDef, dont_send_complete_msg}
    end.

%%
%%
handle_msg({completed_msg, FromNodeDef, Msg}, NodeDef) ->
    {NodeDef2, Msg2} = handle_completed_msg(NodeDef, FromNodeDef, Msg),
    {handled, NodeDef2, Msg2};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
