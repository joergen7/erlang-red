-module(ered_node_join).

-include("ered_nodes.hrl").

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% join node is the companion of the split node that generates many messages
%% from a single message. The join node collects these together again and
%% sends them out as a single message.
%%
%% Possible attributes:
%%
%%       "mode": "custom",
%%       "build": "array",       <<---- send out as array, aka list
%%       "property": "",
%%       "propertyType": "full", <<---- collect the entire msg object
%%       "key": "topic",
%%       "joiner": "\\n",
%%       "joinerType": "str",
%%       "useparts": false,      <<---- parts is set by the split node
%%       "accumulate": false,
%%       "timeout": "",          <<---- wait this long after the first message before sending
%%       "count": "24",          <<---- wait for 24 messages before sending
%%       "reduceRight": false,
%%       "reduceExp": "",
%%       "reduceInit": "",
%%       "reduceInitType": "",
%%       "reduceFixup": "",
%%

-import(ered_nodes, [
    jstr/2,
    send_msg_to_connected_nodes/2
]).
-import(ered_nodered_comm, [
    unsupported/3
]).
-import(ered_message_exchange, [
    post_completed/2
]).
-import(ered_messages, [
    retrieve_prop_value/2
]).

%%
%%
start(
    #{
        <<"mode">> := Mode,
        <<"build">> := Build,
        <<"count">> := Count,
        <<"propertyType">> := PropType
    } = NodeDef,
    WsName
) ->
    case use_manual(Mode, Build, PropType, Count, NodeDef) of
        {ok, V} ->
            ered_node:start(V, ?MODULE);
        {false, V} ->
            ErrMsg = jstr("Node Config ~p", [NodeDef]),
            unsupported(NodeDef, {websocket, WsName}, ErrMsg),
            ered_node:start(V, ered_node_ignore)
    end;
start(NodeDef, WsName) ->
    ErrMsg = jstr("Node Config ~p", [NodeDef]),
    unsupported(NodeDef, {websocket, WsName}, ErrMsg),
    ered_node:start(NodeDef, ered_node_ignore).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
%% Handle manually collecting a specific property name.
%% Just received the final message, ready to generated a message
handle_msg(
    {incoming, Msg},
    #{
        '_is_manually_collecting' := {1, {prop, PropName}, Lst},
        <<"count">> := Count
    } = NodeDef
) ->
    %% because these values are sent off to the complete node,
    %% need to keep a copy of the original message.
    Lst2 = [{retrieve_prop_value(PropName, Msg), Msg} | Lst],

    %% now that we are ready to send out our message, we are completed
    %% with the message that make up that message (!!) so those
    %% messages should be sent to a complete node - if there is one
    %% See this post for details:
    %%   https://discourse.nodered.org/t/complete-node-msg-before-or-after-computation/96648/5
    [post_completed(NodeDef, M) || {_, M} <- lists:reverse(Lst2)],

    %% Need to reverse the order of the returned array - because
    %% we've been pushing onto the head and not the tail.
    Msg2 = Msg#{?AddPayload([V || {V, _} <- lists:reverse(Lst2)])},
    send_msg_to_connected_nodes(NodeDef, Msg2),

    {ok, Cnt} = convert_to_int(Count),
    %% reset the counter, ready to receive more messages
    NodeDef2 =
        NodeDef#{'_is_manually_collecting' => {Cnt, {prop, PropName}, []}},

    {handled, NodeDef2, dont_send_complete_msg};
%% Not yet collected all messages, add message to list
handle_msg(
    {incoming, Msg},
    #{
        '_is_manually_collecting' := {Cnt, {prop, PropName}, Lst}
    } = NodeDef
) ->
    NodeDef2 = NodeDef#{
        '_is_manually_collecting' =>
            {Cnt - 1, {prop, PropName}, [
                {retrieve_prop_value(PropName, Msg), Msg} | Lst
            ]}
    },
    {handled, NodeDef2, dont_send_complete_msg};
%%
%% Collecting the entire Message object
%%
%% Recieved final message
handle_msg(
    {incoming, Msg},
    #{
        '_is_manually_collecting' := {1, {entire_msg}, Lst},
        <<"count">> := Count
    } = NodeDef
) ->
    Lst2 = [Msg | Lst],

    %% now that we are ready to send out our message, we are completed
    %% with the message that make up that message (!!) so those
    %% messages should be sent to a complete node - if there is one
    %% See this post for details:
    %%   https://discourse.nodered.org/t/complete-node-msg-before-or-after-computation/96648/5
    [post_completed(NodeDef, M) || M <- lists:reverse(Lst2)],

    %% Need to reverse the order of the returned array - because
    %% we've been pushing onto the head and not the tail.
    Msg2 = Msg#{?AddPayload(lists:reverse(Lst2))},
    send_msg_to_connected_nodes(NodeDef, Msg2),

    {ok, Cnt} = convert_to_int(Count),

    NodeDef2 = NodeDef#{'_is_manually_collecting' => {Cnt, {entire_msg}, []}},
    {handled, NodeDef2, dont_send_complete_msg};
handle_msg(
    {incoming, Msg},
    #{
        '_is_manually_collecting' := {Cnt, {entire_msg}, Lst}
    } = NodeDef
) ->
    NodeDef2 = NodeDef#{
        '_is_manually_collecting' =>
            {Cnt - 1, {entire_msg}, [Msg | Lst]}
    },
    {handled, NodeDef2, dont_send_complete_msg};
%%
%%
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%
%% --------------- helpers
%%
%%
convert_to_int(Val) when is_integer(Val) ->
    {ok, Val};
convert_to_int(Val) when is_float(Val) ->
    {ok, erlang:element(1, string:to_integer(io_lib:format("~p", [Val])))};
convert_to_int(Val) ->
    case string:to_float(Val) of
        {error, _} ->
            case string:to_integer(Val) of
                {error, _} ->
                    {error, "no conversion possible"};
                {V, _} ->
                    {ok, V}
            end;
        {V, _} ->
            %% V is now a float and the guard 'is_float' will catch it now
            {ok, convert_to_int(V)}
    end.

%%
%%
use_manual(
    <<"custom">>,
    <<"array">>,
    <<"msg">>,
    Count,
    #{<<"property">> := PropName} = NodeDef
) ->
    %% This means take a specific property of the msg, e.g. payload
    %%    "mode": "custom",
    %%    "build": "array",
    %%    "property": "payload",
    %%    "propertyType": "msg",
    case convert_to_int(Count) of
        {ok, 0} ->
            {false, NodeDef};
        {ok, Cnt} ->
            {ok, NodeDef#{
                '_is_manually_collecting' => {Cnt, {prop, PropName}, []}
            }};
        _ ->
            {false, NodeDef}
    end;
use_manual(<<"custom">>, <<"array">>, <<"full">>, Count, NodeDef) ->
    case convert_to_int(Count) of
        {ok, 0} ->
            {false, NodeDef};
        {ok, Cnt} ->
            {ok, NodeDef#{'_is_manually_collecting' => {Cnt, {entire_msg}, []}}};
        _ ->
            {false, NodeDef}
    end;
use_manual(_, _, _, _, NodeDef) ->
    {false, NodeDef}.
