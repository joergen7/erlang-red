-module(ered_node_csv).

-include("ered_nodes.hrl").
-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Basic CSV node based on nimble_csv. This should support streaming instead
%% splitting arrays. There is still much to be done here.
%%
%% NOTE: this node *isn't* implemented for performance, this node is implemented
%% NOTE: to showcase the ability of Erlang-Red to bring developers together.
%% NOTE: things like handling the parsing of strings to numbers can be
%% NOTE: done better ... also using atoms as map keys is not a good idea but
%% NOTE: I do this because thats how JSON content is decoded so comparison
%% NOTE: is possible
%%

%%  "spec": "rfc",
%%  "sep": ",",
%%  "hdrin": "", <<--- first line defines column names: "", true, false
%%  "hdrout": "none",
%%  "multi": "one", <<---- one means one msg per line, "mult" is all at once
%%  "ret": "\\r\\n", <<--- line separator double escaped
%%  "temp": "",
%%  "skip": "0", <<-- skip this many rows from the front
%%  "strings": true, <<-- true -> parse strings to numbers, false -> don't parse
%%  "include_empty_strings": "",
%%  "include_null_values": "",

-import(ered_nodered_comm, [
    post_exception_or_debug/3,
    unsupported/3
]).

-import(ered_nodes, [
    jstr/2,
    send_msg_to_connected_nodes/2
]).

-import(ered_message_exchange, [
    post_completed/2
]).

-import(ered_messages, [
    convert_to_num/1,
    to_bool/1
]).

-define(AddColumns(V), <<"columns">> => V).

%%
%%
start(
    #{<<"sep">> := <<>>} = NodeDef,
    WsName
) ->
    %% empty column separator not supported
    unsupported(NodeDef, {websocket, WsName}, "configuration: empty sep"),
    ered_node:start(NodeDef, ered_node_ignore);
%%
start(
    #{
        <<"include_empty_strings">> := <<>>,
        <<"include_null_values">> := <<>>,
        <<"spec">> := <<"rfc">>
    } = NodeDef,
    _WsName
) ->
    ered_node:start(NodeDef, ?MODULE);
%%
start(#{<<"spec">> := <<"rfc">>} = NodeDef, WsName) ->
    %% either null values or empty strings option defined, not supported
    unsupported(NodeDef, {websocket, WsName}, "empty strings | null values"),
    ered_node:start(NodeDef, ered_node_ignore);
%%
start(NodeDef, WsName) ->
    %% format isn't rfc -- not supported
    unsupported(NodeDef, {websocket, WsName}, "csv format"),
    ered_node:start(NodeDef, ered_node_ignore).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg(
    {incoming, #{?GetPayload} = Msg},
    #{
        <<"sep">> := _ColumnSeparator,
        <<"ret">> := _LineSeparator
    } = NodeDef
) when Payload =:= <<>> ->
    %% empty payload
    {handled, NodeDef, Msg};
%%
handle_msg(
    {incoming, #{?GetPayload} = Msg},
    #{
        <<"sep">> := ColumnSeparator,
        <<"ret">> := _LineSeparator
    } = NodeDef
) when is_binary(Payload) ->
    %% payload is binary data --> decode the data
    handle_decode(ColumnSeparator, Payload, NodeDef, Msg);
%%
handle_msg(
    {incoming, #{?GetPayload} = Msg},
    #{
        <<"sep">> := ColumnSeparator,
        <<"ret">> := LineSeparator
    } = NodeDef
) when is_list(Payload) ->
    %% payload is a list of stuff --> encode the data
    handle_encode(ColumnSeparator, LineSeparator, Payload, NodeDef, Msg);
%%
handle_msg(
    {incoming, Msg},
    NodeDef
) ->
    %% no payload defined on the message, ignore and pretend all is good.
    {handled, NodeDef, Msg};
%%
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%
handle_encode(Sep, <<"\\n">>, Payload, NodeDef, Msg) ->
    handle_encode(Sep, <<"\n">>, Payload, NodeDef, Msg);
handle_encode(Sep, <<"\\r">>, Payload, NodeDef, Msg) ->
    handle_encode(Sep, <<"\r">>, Payload, NodeDef, Msg);
handle_encode(Sep, <<"\\r\\n">>, Payload, NodeDef, Msg) ->
    handle_encode(Sep, <<"\r\n">>, Payload, NodeDef, Msg);
% Payload is a list of lists - good!
handle_encode(Sep, LineSep, Payload = [H | _T], NodeDef, Msg) when is_list(H) ->
    Msg2 = Msg#{
        ?AddPayload(
            list_to_binary(
                ered_csv_parser_store:encode_something(
                    Payload,
                    Sep,
                    <<"\"">>,
                    LineSep
                )
            )
        )
    },
    send_msg_to_connected_nodes(NodeDef, Msg2),
    {handled, NodeDef, Msg2};
% Payload is single array of values - this is also handled by the Node-RED
% CSV node even though - IMHO - it's incorrect formatting. Data that should
% be converted to CSV should always be a list-of-lists or a list of maps.
handle_encode(Sep, LineSep, Payload, NodeDef, Msg) ->
    handle_encode(Sep, LineSep, [Payload], NodeDef, Msg).

%%
%%
handle_decode(Sep, Payload, NodeDef, Msg) ->
    handle_rfc(NodeDef, Msg, Payload, Sep).

%%
%%
handle_rfc(NodeDef, Msg, _Payload, <<>>) ->
    {handled, NodeDef, Msg};
handle_rfc(NodeDef, Msg, Payload, <<"\\t">>) ->
    handle_rfc(NodeDef, Msg, Payload, <<"\t">>);
handle_rfc(
  #{
    <<"multi">> := MultiOrOne,
    <<"skip">> := SkipRowCount,
    <<"hdrin">> := FirstRowIsHeader,
    <<"strings">> := ParseStringsToNumbers
   } = NodeDef,
  Msg,
  Payload,
  Sep
) ->
    DataOrig = ered_csv_parser_store:parse_string(
        Sep,
        <<"\"">>,
        Payload,
        % we deal with the headers
        [{skip_headers, false}]
    ),

    % skip line can be done here since the header hasn't yet been defined
    % just as Node-RED does it: skip first, then define header. Which means
    % that a line of content can also be used as a header line.
    DataStrings = skip_rows(DataOrig, convert_to_num(SkipRowCount)),

    % remove the first line if necessary
    {ColNames, HeadlessData} = obtain_columns(
        DataStrings, FirstRowIsHeader
    ),

    % convert things to numeric values if necessary
    NumericData = parse_nums(HeadlessData, ParseStringsToNumbers),

    case MultiOrOne of
        <<"one">> ->
            %% send_one_msg_per_row generates a {handle, ...} tuple
            send_one_msg_per_row(
                NodeDef,
                Msg#{
                     ?AddColumns(create_columns_values(ColNames))
                },
                NumericData,
                ColNames
            );
        <<"mult">> ->
            Msg2 = Msg#{
                ?AddPayload(create_maps(NumericData, ColNames)),
                ?AddColumns(create_columns_values(ColNames))
            },
            send_msg_to_connected_nodes(NodeDef, Msg2),
            {handled, NodeDef, Msg2}
    end.

%%
%%
create_maps(Data, ColNames) ->
    [maps:from_list(lists:zip(ColNames, Row)) || Row <- Data].
%%
%%
generate_parts(Cnt, TotalCnt, MsgId) ->
    #{<<"id">> => MsgId, <<"count">> => TotalCnt, <<"index">> => Cnt}.

%%
%%
send_one_msg_per_row(NodeDef, Msg, Data, ColNames) ->
    send_one_msg_per_row(NodeDef, Msg, Data, ColNames, 0, length(Data)).

send_one_msg_per_row(NodeDef, Msg, [], _A, _B, _C) ->
    {handled, NodeDef, Msg};
send_one_msg_per_row(
  NodeDef, #{'_msgid' := MsgId} = Msg, [Row | MoreRows], ColNames, Idx, Len
) ->
    Msg2 = Msg#{
        ?AddParts(generate_parts(Idx, Len, MsgId)),
        ?AddPayload(maps:from_list(lists:zip(ColNames, Row)))
    },
    send_msg_to_connected_nodes(NodeDef, Msg2),
    post_completed(NodeDef, Msg2),
    send_one_msg_per_row(NodeDef, Msg, MoreRows, ColNames, Idx + 1, Len).

%%
%%
create_columns_values(Row) ->
    list_to_binary(lists:join(",", Row)).

%%
%%
-spec obtain_columns(
    AllRows :: list(),
    FirstRowIsHeaderRow :: boolean() | <<>>
) -> Result :: {list(), [list()]}.
obtain_columns([FirstRow | HeadlessData], true) ->
    {FirstRow, HeadlessData};
obtain_columns([FirstRow | _RestData] = AllRows, _FirstRowIsHeaderRow) ->
    {
        [
            list_to_binary(io_lib:format("col~p", [Idx]))
         || Idx <- lists:seq(1, length(FirstRow))
        ],
        AllRows
    }.
%%
%%
skip_rows(Data, Cnt) when Cnt < 0 ->
    skip_rows(Data, 0);
skip_rows(Data, 0) ->
    Data;
skip_rows([_ | MoreRows], Cnt) ->
    skip_rows(MoreRows, Cnt - 1).

%%
%% Yes, if the strings attribute is true, then parse the strings to become
%% numbers --> "Content is strings but we want numbers" --> True!
parse_nums(Data, <<"true">>) ->
    parse_nums(Data, true);
parse_nums(Data, "true") ->
    parse_nums(Data, true);
parse_nums(Data, true) ->
    %% Data is a list of lists ...
    [convert_list_to_nums(Row) || Row <- Data];
parse_nums(Data, _) ->
    Data.

%%
%%
convert_list_to_nums([], Acc) ->
    lists:reverse(Acc);
convert_list_to_nums([H | T], Acc) ->
    convert_list_to_nums(T, [convert_string_to_num(H) | Acc]).

convert_list_to_nums(Row) ->
    convert_list_to_nums(Row, []).

%%
%%
convert_string_to_num(Str) ->
    case convert_to_num(Str) of
        {error, _} ->
            Str;
        V ->
            V
    end.
