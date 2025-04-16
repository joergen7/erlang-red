-module(ered_node_join).

-export([node_join/1]).
-export([handle_incoming/2]).

-import(ered_node_receivership, [enter_receivership/3]).

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

handle_incoming(NodeDef, Msg) ->
    case maps:find('_is_manually_collecting', NodeDef) of
        {ok, {ok, 1, Lst}} ->
            Lst2 = [Msg | Lst],
            Msg2 = maps:put(payload, Lst2, Msg),
            ered_nodes:send_msg_to_connected_nodes(NodeDef, Msg2),

            {ok, Count} = maps:find(count, NodeDef),
            {ok, Cnt} = convert_to_int(Count),
            maps:put('_is_manually_collecting', {ok, Cnt, []}, NodeDef);
        {ok, {ok, Cnt, Lst}} ->
            maps:put(
                '_is_manually_collecting',
                {ok, Cnt - 1, [Msg | Lst]},
                NodeDef
            );
        _ ->
            NodeDef
    end.

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
set_manual(<<"custom">>, <<"array">>, <<"full">>, Count, NodeDef) ->
    case convert_to_int(Count) of
        {ok, 0} ->
            NodeDef;
        {ok, Cnt} ->
            maps:put('_is_manually_collecting', {ok, Cnt, []}, NodeDef);
        _ ->
            NodeDef
    end;
set_manual(_, _, _, _, NodeDef) ->
    NodeDef.

%%
%%
node_join(NodeDef) ->
    ered_nodes:node_init(NodeDef),

    {ok, Mode} = maps:find(mode, NodeDef),
    {ok, Build} = maps:find(build, NodeDef),
    {ok, Count} = maps:find(count, NodeDef),
    {ok, PropType} = maps:find(propertyType, NodeDef),

    NodeDef2 = set_manual(Mode, Build, PropType, Count, NodeDef),

    enter_receivership(?MODULE, NodeDef2, only_incoming).
