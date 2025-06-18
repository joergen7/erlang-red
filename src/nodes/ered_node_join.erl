-module(ered_node_join).

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
start(NodeDef, WsName) ->
    Mode = maps:get(<<"mode">>, NodeDef),
    Build = maps:get(<<"build">>, NodeDef),
    Count = maps:get(<<"count">>, NodeDef),
    PropType = maps:get(<<"propertyType">>, NodeDef),

    NodeDef2 =
        case use_manual(Mode, Build, PropType, Count, NodeDef) of
            {ok, V} ->
                V;
            {false, V} ->
                ErrMsg = jstr("Node Config ~p", [NodeDef]),
                unsupported(NodeDef, {websocket, WsName}, ErrMsg),
                V
        end,
    ered_node:start(NodeDef2, ?MODULE).

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
use_manual(<<"custom">>, <<"array">>, <<"msg">>, Count, NodeDef) ->
    %% This means take a specific property of the msg, e.g. payload
    %%    "mode": "custom",
    %%    "build": "array",
    %%    "property": "payload",
    %%    "propertyType": "msg",
    case convert_to_int(Count) of
        {ok, 0} ->
            {false, NodeDef};
        {ok, Cnt} ->
            {ok,
                maps:put(
                    '_is_manually_collecting',
                    {ok, Cnt, maps:find(<<"property">>, NodeDef), []},
                    NodeDef
                )};
        _ ->
            {false, NodeDef}
    end;
use_manual(<<"custom">>, <<"array">>, <<"full">>, Count, NodeDef) ->
    case convert_to_int(Count) of
        {ok, 0} ->
            {false, NodeDef};
        {ok, Cnt} ->
            {ok,
                maps:put(
                    '_is_manually_collecting',
                    {ok, Cnt, {entire_msg}, []},
                    NodeDef
                )};
        _ ->
            {false, NodeDef}
    end;
use_manual(_, _, _, _, NodeDef) ->
    {false, NodeDef}.

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

handle_incoming(NodeDef, Msg) ->
    case maps:find('_is_manually_collecting', NodeDef) of
        %%
        %% Handle a specific property
        %%
        {ok, {ok, 1, {ok, PropName}, Lst}} ->
            %% because these values are sent off to the complete node,
            %% need to keep a copy of the original message.
            Lst2 = [{retrieve_prop_value(PropName, Msg), Msg} | Lst],
            %% Need to reverse the order of the returned array - because
            %% we've been pushing onto the head and not the tail.
            Msg2 = maps:put(
                <<"payload">>,
                [V || {V, _} <- lists:reverse(Lst2)],
                Msg
            ),

            %% now that we are ready to send out our message, we are completed
            %% with the message that make up that message (!!) so those
            %% messages should be sent to a complete node - if there is one
            %% See this post for details:
            %%   https://discourse.nodered.org/t/complete-node-msg-before-or-after-computation/96648/5
            [post_completed(NodeDef, M) || {_, M} <- lists:reverse(Lst2)],

            send_msg_to_connected_nodes(NodeDef, Msg2),

            {ok, Count} = maps:find(<<"count">>, NodeDef),
            {ok, Cnt} = convert_to_int(Count),
            NodeDef2 = maps:put(
                '_is_manually_collecting',
                {ok, Cnt, {ok, PropName}, []},
                NodeDef
            ),
            {NodeDef2, dont_send_complete_msg};
        {ok, {ok, Cnt, {ok, PropName}, Lst}} ->
            NodeDef2 = maps:put(
                '_is_manually_collecting',
                {ok, Cnt - 1, {ok, PropName}, [
                    {retrieve_prop_value(PropName, Msg), Msg} | Lst
                ]},
                NodeDef
            ),

            %% dont_send is an message to the post_completed callback
            %% to ignore this msg. This is becuase the join node didn't
            %% send anything - yet.
            {NodeDef2, dont_send_complete_msg};
        %%
        %% Handle the entire message object
        %%
        {ok, {ok, 1, {entire_msg}, Lst}} ->
            Lst2 = [Msg | Lst],

            %% Need to reverse the order of the returned array - because
            %% we've been pushing onto the head and not the tail.
            Msg2 = maps:put(<<"payload">>, lists:reverse(Lst2), Msg),

            %% now that we are ready to send out our message, we are completed
            %% with the message that make up that message (!!) so those
            %% messages should be sent to a complete node - if there is one
            %% See this post for details:
            %%   https://discourse.nodered.org/t/complete-node-msg-before-or-after-computation/96648/5
            [post_completed(NodeDef, M) || M <- lists:reverse(Lst2)],

            send_msg_to_connected_nodes(NodeDef, Msg2),

            {ok, Count} = maps:find(<<"count">>, NodeDef),
            {ok, Cnt} = convert_to_int(Count),

            NodeDef2 = maps:put(
                '_is_manually_collecting',
                {ok, Cnt, {entire_msg}, []},
                NodeDef
            ),
            {NodeDef2, dont_send_complete_msg};
        {ok, {ok, Cnt, {entire_msg}, Lst}} ->
            NodeDef2 = maps:put(
                '_is_manually_collecting',
                {ok, Cnt - 1, {entire_msg}, [Msg | Lst]},
                NodeDef
            ),

            %% dont_send is an message to the post_completed callback
            %% to ignore this msg. This is becuase the join node didn't
            %% send anything - yet.
            {NodeDef2, dont_send_complete_msg};
        _ ->
            {NodeDef, dont_send_complete_msg}
    end.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    {NodeDef2, Msg2} = handle_incoming(NodeDef, Msg),
    {handled, NodeDef2, Msg2};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
