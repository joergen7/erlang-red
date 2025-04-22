-module(ered_node_file_in).

-export([node_file_in/2]).
-export([handle_event/2]).
-export([handle_incoming/2]).

%%
%% No Operation node that is used for all unknown types. It represents
%% a deadend for a message, it stops here.

-import(ered_node_receivership, [enter_receivership/3]).
-import(ered_nodes, [
    get_prop_value_from_map/2,
    get_prop_value_from_map/3,
    jstr/1,
    jstr/2,
    send_msg_to_connected_nodes/2,
    this_should_not_happen/2,
    unpriv/1
]).
-import(ered_nodered_comm, [
    debug/3,
    send_out_debug_msg/4,
    ws_from/1
]).

-import(ered_message_exchange, [
    post_exception/3
]).

debug_msg(NodeDef, Msg, {filename_type_not_supported, FileNameType}) ->
    ErrMsg = jstr(
        "File name type not supported: ~p",
        [FileNameType]
    ),
    send_out_debug_msg(NodeDef, Msg, ErrMsg, error);
debug_msg(NodeDef, Msg, {file_not_found, FileName}) ->
    ErrMsg = jstr("File Not Found: ~p", [FileName]),
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
    maps:find(filename, NodeDef);
get_filename(<<"msg">>, NodeDef, Msg) ->
    {ok, PropName} = maps:find(filename, NodeDef),
    maps:find(binary_to_atom(PropName), Msg);
get_filename(FileNameType, NodeDef, Msg) ->
    debug_msg(NodeDef, Msg, {filename_type_not_supported, FileNameType}),
    failed.

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

handle_incoming(NodeDef, Msg) ->
    {ok, FileNameType} = maps:find(filenameType, NodeDef),

    case get_filename(FileNameType, NodeDef, Msg) of
        {ok, <<>>} ->
            debug_msg(NodeDef, Msg, {no_file_specified}),
            {NodeDef, Msg};
        {ok, FileName} ->
            case file:read_file(unpriv(FileName)) of
                {ok, FileData} ->
                    Msg2 = maps:put(payload, FileData, Msg),
                    send_msg_to_connected_nodes(NodeDef, Msg2),
                    {NodeDef, Msg2};
                _ ->
                    ErrMsg = jstr("File Not Found: ~p", [FileName]),
                    case post_exception(NodeDef, Msg, ErrMsg) of
                        dealt_with ->
                            ok;
                        _ ->
                            debug_msg(NodeDef, Msg, {file_not_found, FileName})
                    end,
                    {NodeDef, Msg}
            end;
        failed ->
            {NodeDef, Msg};
        _ ->
            this_should_not_happen(
                NodeDef,
                io_lib:format(
                    "file in error in obtaining filename [~p] (~p)\n",
                    [NodeDef, Msg]
                )
            ),
            {NodeDef, Msg}
    end.

node_file_in(NodeDef, _WsName) ->
    enter_receivership(?MODULE, NodeDef, only_incoming).
