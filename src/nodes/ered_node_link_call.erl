-module(ered_node_link_call).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Link call node sends messages to link-in nodes and awaits a return
%% call from the link out node in return mode. Once it gets a return
%% message, it sends that message on to the nodes connected to it.
%%
-import(ered_nodes, [
    generate_id/1,
    jstr/2,
    post_exception_or_debug/3,
    send_msg_on/2,
    send_msg_on_by_pids/2,
    send_msg_to_connected_nodes/2,
    this_should_not_happen/2
]).
-import(ered_nodered_comm, [
    debug/3,
    debug_string/2,
    node_status/5,
    unsupported/3,
    ws_from/1
]).
-import(ered_link_node_exchange, [
    obtain_link_node_pid/2
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

update_linksource(NodeDef, Msg) ->
    {ok, IdStr} = maps:find(id, NodeDef),
    LinkBack = #{id => generate_id(32), node => IdStr},

    case maps:find('_linkSource', Msg) of
        {ok, Ary} ->
            maps:put('_linkSource', Ary ++ [LinkBack], Msg);
        _ ->
            maps:put('_linkSource', [LinkBack], Msg)
    end.

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%% send message to the link in node that this is linked to provided the node
%% isn't in dynamic mode.
%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    case maps:find(linkType, NodeDef) of
        {ok, <<"dynamic">>} ->
            case maps:find(target, Msg) of
                {ok, Target} ->
                    {ok, FlowId} = maps:find(z, NodeDef),
                    WsName = ws_from(Msg),
                    io:format("LinkCall Target[~s,~s]: ~p~n",[FlowId, WsName, Target]),
                    case obtain_link_node_pid(Target, ws_from(Msg)) of
                        [] ->
                            io:format("LinkCall Target[~s,~s]: ~p [NONE FOUND]~n",[FlowId, WsName, Target]),
                            ignore;
                        [Pid] ->
                            %% Exactly one Pid, perfect - everything else is
                            %% is an error
                            io:format("LinkCall Target[~s,~s]: ~p [ONE FOUND] ~p~n",[FlowId,WsName,Target,Pid]),
                            send_msg_on_by_pids([Pid],
                                                update_linksource(NodeDef, Msg));
                        Pids ->
                            io:format("LinkCall Target[~s,~s]: ~p [TOO MANY FOUND] ~p~n",[FlowId,WsName,Target,Pids]),
                            ErrMsg = jstr(
                                "multiple nodes found for msg.target", []
                            ),
                            post_exception_or_debug(NodeDef, Msg, ErrMsg)
                    end;
                _ ->
                    ErrMsg = jstr("not target set on message",[]),
                    post_exception_or_debug(NodeDef, Msg, ErrMsg)
            end;
        {ok, <<"static">>} ->
            case maps:find(links, NodeDef) of
                {ok, Links} ->
                    send_msg_on(Links, update_linksource(NodeDef, Msg));
                _ ->
                    ignore
            end;
        {ok, LinkType} ->
            ErrMsg = jstr("Unknown LinkType: '~s'", [LinkType]),
            this_should_not_happen(
                NodeDef,
                io_lib:format("~p ~p\n", [ErrMsg, Msg])
            ),
            debug(ws_from(Msg), debug_string(NodeDef, ErrMsg), notice),
            node_status(
                ws_from(Msg), NodeDef, "unknown linkType", "red", "dot"
            );
        _ ->
            ignore
    end,
    {handled, NodeDef, dont_send_complete_msg};
handle_msg({link_return, Msg}, NodeDef) ->
    %% This comes from a link out node in return mode, this means we pass
    %% the message on to all the nodes connected to us, i.e. the 'wires'
    %% attribute.
    send_msg_to_connected_nodes(NodeDef, Msg),
    {handled, NodeDef, Msg};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
