-module(ered_node_file_in).

%%
%% No Operation node that is used for all unknown types. It represents
%% a deadend for a message, it stops here.

-export([node_file_in/1]).
-export([handle_incoming/2]).

-import(node_receivership, [enter_receivership/3]).

%% Attributes of interest:
%%   "filename": "priv/testdata/helloworld.txt",
%%   "filenameType": "str",
%%   "format": "utf8",
%%   "chunk": false,
%%   "sendError": false,
%%   "encoding": "none",
%%   "allProps": false,
%%
handle_incoming(NodeDef, Msg) ->
    {ok, FileName} = maps:find(filename, NodeDef),
    case file:read_file(FileName) of
        {ok, FileData} ->
            Msg2 = maps:put(payload, FileData, Msg),
            ered_nodes:send_msg_to_connected_nodes(NodeDef, Msg2);
        _ ->
            %%
            %% TODO this should trigger an exception handler if one is defined
            %% TODO within this flow (i.e. the 'z' value defines the flow, is
            %% TODO there an catch node for that 'z' value).
            ered_nodes:this_should_not_happen(
                NodeDef,
                io_lib:format(
                    "File In Node: File not found [~p]\n",
                    [FileName]
                )
            )
    end,

    NodeDef.

node_file_in(NodeDef) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, only_incoming).
