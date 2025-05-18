-module(ered_node_csv).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Basic CSV node based on nimble_csv. This should support streaming instead
%% splitting arrays. There is still much to be done here.
%%
%% NOTE: this node *isn't* implemented for performance, this node is implemented
%% NOTE: to showcase the ability of Erlang-RED to bring developers together.
%% NOTE: things like handling the parsing of strings to numbers can be
%% NOTE: done better ... also using atoms as maps key is not a good idea but
%% NOTE: I do this because thats how JSON content is decoded so comparison
%% NOTE: is possible
%%

%%     "type": "csv",
%%     ...
%%     "spec": "rfc",
%%     "sep": ",",
%%     "hdrin": "", <<--- first line defines column names: "", true, false
%%     "hdrout": "none",
%%     "multi": "one", <<---- one means one msg per line, "mult" is all at once
%%     "ret": "\\r\\n",
%%     "temp": "",
%%     "skip": "0", <<-- skip this many rows from the front
%%     "strings": true, <<-- true -> parse strings to numbers, false -> don't parse
%%     "include_empty_strings": "",
%%     "include_null_values": "",

-import(ered_nodered_comm, [
    unsupported/3
]).

-import(ered_nodes, [
    jstr/2,
    post_exception_or_debug/3,
    send_msg_to_connected_nodes/2
]).

-import(ered_message_exchange, [
    post_completed/2
]).

-import(ered_msg_handling, [
    convert_to_num/1,
    to_bool/1
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
    case {maps:get(sep, NodeDef), maps:get(spec, NodeDef)} of
        {<<",">>, <<"rfc">>} ->
            handle_rfc_comma(NodeDef, Msg);
        {_, _} ->
            unsupported(NodeDef, Msg, "supported configuration"),
            {handled, NodeDef, dont_send_complete_msg}
    end;
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%
send_one_msg_per_row(NodeDef, Msg, Data, ColNames) ->
    send_one_msg_per_row(NodeDef, Msg, Data, ColNames, 0, length(Data)).

send_one_msg_per_row(NodeDef, _, [], _, _, _) ->
    {handled, NodeDef, dont_send_complete_msg};
send_one_msg_per_row(NodeDef, Msg, [Row | MoreRows], ColNames, Idx, Len) ->
    Msg2 = maps:put(
        parts,
        generate_parts(Idx, Len, maps:get('_msgid', Msg)),
        Msg
    ),
    Msg3 = maps:put(payload, maps:from_list(lists:zip(ColNames, Row)), Msg2),
    send_msg_to_connected_nodes(NodeDef, Msg3),
    post_completed(NodeDef, Msg3),
    send_one_msg_per_row(NodeDef, Msg, MoreRows, ColNames, Idx + 1, Len).

%%
%%
handle_rfc_comma(NodeDef, Msg) ->
    case maps:get(multi, NodeDef) of
        <<"one">> ->
            DataOrig = 'Elixir.ErlangRedHelpers':csv_decode_as_rfc_4180(
                maps:get(payload, Msg),
                [{skip_headers, false}]
            ),

            DataStrings = skip_rows(
                DataOrig,
                convert_to_num(maps:get(skip, NodeDef))
            ),
            Data = parse_nums(DataStrings, maps:get(strings, NodeDef)),

            ColNames = obtain_columns(Data, maps:get(hdrin, NodeDef)),

            send_one_msg_per_row(
                NodeDef,
                maps:put(
                    columns,
                    create_columns_values(ColNames),
                    Msg
                ),
                Data,
                ColNames
            );
        <<"mult">> ->
            %% TODO I suspect this format is wrong and that the node-red
            %% TODO node returns an array of objects and not an array of
            %% TODO arrays but I'll create a test first.
            DataOrig = 'Elixir.ErlangRedHelpers':csv_decode_as_rfc_4180(
                maps:get(payload, Msg),
                [{skip_headers, to_bool(maps:get(hdrin, NodeDef))}]
            ),
            DataStrings = skip_rows(
                DataOrig,
                convert_to_num(maps:get(skip, NodeDef))
            ),

            Content = parse_nums(DataStrings, maps:get(strings, NodeDef)),

            Msg2 = maps:put(payload, Content, Msg),
            send_msg_to_connected_nodes(NodeDef, Msg2),
            {handled, NodeDef, Msg2}
    end.

%%
%%
generate_parts(Cnt, TotalCnt, MsgId) ->
    #{
        id => MsgId,
        count => TotalCnt,
        index => Cnt
    }.

%%
%%
create_columns_values(Row) ->
    list_to_binary(lists:join(",", [atom_to_binary(R) || R <- Row])).

obtain_columns([FirstRow | _], true) ->
    FirstRow;
obtain_columns([FirstRow | _], _) ->
    [
        binary_to_atom(list_to_binary(io_lib:format("col~p", [Idx])))
     || Idx <- lists:seq(1, length(FirstRow))
    ].
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
parse_nums(Data, true) ->
    %% Data is a list of lists ...
    [convert_list_to_nums(Row) || Row <- Data];
parse_nums(Data, <<"true">>) ->
    parse_nums(Data, true);
parse_nums(Data, "true") ->
    parse_nums(Data, true);
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
