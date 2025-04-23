-module(ered_node_link_out).

-export([node_link_out/2]).
-export([handle_event/2]).
-export([handle_incoming/2]).

-import(ered_node_receivership, [enter_receivership/3]).

-import(ered_nodered_comm, [
    debug/3,
    debug_string/2,
    node_status/5,
    ws_from/1
]).

-import(ered_nodes, [
    nodeid_to_pid/2,
    jstr/2,
    send_msg_on/2,
    send_msg_to_connected_nodes/2,
    this_should_not_happen/2
]).

send_to_link_call({ok, NodeId}, Msg) ->
    NodePid = nodeid_to_pid(ws_from(Msg), NodeId),
    case NodePid of
        {error, _} ->
            ignore;
        {ok, Pid} ->
            Pid ! {link_return, Msg}
    end.

last_value([], _) ->
    empty;
last_value([H | []], Rest) ->
    {ok, H, lists:reverse(Rest)};
last_value([H | Ary], Rest) ->
    last_value(Ary, [H | Rest]).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_incoming(NodeDef, Msg) ->
    case maps:find(mode, NodeDef) of
        {ok, <<"link">>} ->
            case maps:find(links, NodeDef) of
                {ok, Links} ->
                    %% this are all link in nodes and they have no incoming
                    %% wires so we can send them their messages using the
                    %% "incoming" message type this is what send_msg_on does.
                    send_msg_on(Links, Msg);
                _ ->
                    ignore
            end,
            {NodeDef, dont_send_complete_msg};
        {ok, <<"return">>} ->
            %% careful, what this does is take the last entry and respond
            %% to that. link calls can be nested hence this logic
            case maps:find('_linkSource', Msg) of
                {ok, Ary} ->
                    case last_value(Ary, []) of
                        empty ->
                            ignore;
                        {ok, LinkBack, NewAry} ->
                            send_to_link_call(
                                maps:find(node, LinkBack),
                                maps:put('_linkSource', NewAry, Msg)
                            )
                    end;
                _ ->
                    ignore
            end,
            {NodeDef, Msg};
        {ok, Mode} ->
            ErrMsg = jstr("Unknown Mode: '~s'", [Mode]),
            this_should_not_happen(
                NodeDef,
                io_lib:format("~p ~p\n", [ErrMsg, Msg])
            ),
            debug(ws_from(Msg), debug_string(NodeDef, ErrMsg), notice),
            node_status(ws_from(Msg), NodeDef, ErrMsg, "red", "dot"),
            {NodeDef, Msg};
        _ ->
            {NodeDef, Msg}
    end.

node_link_out(NodeDef, _WsName) ->
    enter_receivership(?MODULE, NodeDef, only_incoming).
