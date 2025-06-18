-module(ered_node_link_out).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Link out node is the is node that sends messages to the link in node
%% that can be located somewhere completely different.
%%

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

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

send_to_link_call({ok, NodeId}, Msg) ->
    NodePid = nodeid_to_pid(ws_from(Msg), NodeId),
    case NodePid of
        {error, _} ->
            ignore;
        {ok, Pid} ->
            gen_server:cast(Pid, {link_return, Msg})
    end.

%%
%% last_value returns the last entry in the _linkSource array - that is the
%% last link-call node that passed on this message. That is also the link-call
%% node to which we return this message to. But that message will have a new
%% _linkSource array with that link-call node removed.
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
    case maps:find(<<"mode">>, NodeDef) of
        {ok, <<"link">>} ->
            case maps:find(<<"links">>, NodeDef) of
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
                                maps:find(<<"node">>, LinkBack),
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

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    {NodeDef2, Msg2} = handle_incoming(NodeDef, Msg),
    {handled, NodeDef2, Msg2};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
