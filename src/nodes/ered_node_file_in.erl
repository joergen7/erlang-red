-module(ered_node_file_in).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Read file contents from disk and stream the data into the flow. Well
%% not stream but dump into a message.
%%

-import(ered_nodes, [
    jstr/1,
    jstr/2,
    send_msg_to_connected_nodes/2,
    this_should_not_happen/2,
    unpriv/1
]).
-import(ered_nodered_comm, [
    debug/3,
    send_out_debug_msg/4,
    post_exception_or_debug/3,
    ws_from/1
]).

-import(ered_messages, [
    get_prop/2
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

debug_msg(NodeDef, Msg, {filename_type_not_supported, FileNameType}) ->
    ErrMsg = jstr(
        "File name type not supported: ~p",
        [FileNameType]
    ),
    send_out_debug_msg(NodeDef, Msg, ErrMsg, error);
debug_msg(NodeDef, Msg, {no_file_specified}) ->
    ErrMsg = jstr("No file specified"),
    send_out_debug_msg(NodeDef, Msg, ErrMsg, warning).

%% Attributes of interest:
%%   "filename": "priv/testdata/helloworld.txt",
%%   "filenameType": "str",
%%   "format": "utf8",
%%   "chunk": false,
%%   "sendError": false,
%%   "encoding": "none",
%%   "allProps": false,
%%
get_filename(<<"str">>, NodeDef, _Msg) ->
    maps:find(<<"filename">>, NodeDef);
get_filename(<<"msg">>, NodeDef, Msg) ->
    case get_prop(maps:find(<<"filename">>, NodeDef), Msg) of
        {ok, V, _} ->
            %% simulate the return value of maps:find/2
            {ok, V};
        E ->
            E
    end;
get_filename(FileNameType, NodeDef, Msg) ->
    debug_msg(NodeDef, Msg, {filename_type_not_supported, FileNameType}),
    failed.

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

handle_incoming(NodeDef, Msg) ->
    {ok, FileNameType} = maps:find(<<"filenameType">>, NodeDef),

    case get_filename(FileNameType, NodeDef, Msg) of
        {ok, <<>>} ->
            debug_msg(NodeDef, Msg, {no_file_specified}),
            {NodeDef, Msg};
        {ok, FileName} ->
            case file:read_file(unpriv(FileName)) of
                {ok, FileData} ->
                    Msg2 = maps:put(<<"payload">>, FileData, Msg),
                    send_msg_to_connected_nodes(NodeDef, Msg2),
                    {NodeDef, Msg2};
                _ ->
                    ErrMsg = jstr("File Not Found: ~p", [FileName]),
                    post_exception_or_debug(NodeDef, Msg, ErrMsg),
                    {NodeDef, Msg}
            end;
        failed ->
            {NodeDef, Msg};
        Error ->
            this_should_not_happen(
                NodeDef,
                io_lib:format(
                    "file in error in obtaining filename [~p] (~p) --> {{ ~p }}",
                    [NodeDef, Msg, Error]
                )
            ),
            {NodeDef, Msg}
    end.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    {NodeDef2, Msg2} = handle_incoming(NodeDef, Msg),
    {handled, NodeDef2, Msg2};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
