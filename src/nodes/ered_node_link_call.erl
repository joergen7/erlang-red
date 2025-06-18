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
    send_msg_on/2,
    send_msg_on_by_pids/2,
    send_msg_to_connected_nodes/2,
    this_should_not_happen/2
]).
-import(ered_nodered_comm, [
    debug/3,
    debug_string/2,
    node_status/5,
    post_exception_or_debug/3,
    unsupported/3,
    ws_from/1
]).
-import(ered_link_node_exchange, [
    obtain_link_node_pid/2
]).

-define(SET_TIMER,
    erlang:start_timer(
        maps:get(<<"timeout">>, NodeDef), self(), {msg_timed_out, Msg}
    )
).

%%
%%
start(NodeDef, _WsName) ->
    % Ensure timeout value is set to a number and converted to milliseconds
    ered_node:start(
        case maps:find(<<"timeout">>, NodeDef) of
            {ok, Val} ->
                try
                    case binary_to_integer(Val) * 1000 of
                        N when N < 0 ->
                            NodeDef#{<<"timeout">> => 0};
                        N ->
                            NodeDef#{<<"timeout">> => N}
                    end
                catch
                    _E:_F:_S ->
                        NodeDef#{<<"timeout">> => 30_000}
                end;
            _ ->
                NodeDef#{<<"timeout">> => 30_000}
        end,
        ?MODULE
    ).

%%
%%
handle_event({msg_timed_out, Msg}, NodeDef) ->
    post_exception_or_debug(NodeDef, Msg, <<"timeout">>),
    NodeDef;
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    case maps:find(<<"linkType">>, NodeDef) of
        {ok, <<"dynamic">>} ->
            case maps:find(<<"target">>, Msg) of
                {ok, Target} ->
                    case obtain_link_node_pid(Target, ws_from(Msg)) of
                        [] ->
                            ErrMsg = jstr(
                                "Error: target link-in node '~s' not found",
                                [Target]
                            ),
                            post_exception_or_debug(NodeDef, Msg, ErrMsg);
                        [Pid] ->
                            %% Exactly one Pid, perfect - everything else is
                            %% is an error
                            send_msg_on_by_pids(
                                [Pid],
                                update_linksource(
                                    NodeDef,
                                    Msg#{'_timeout_ref' => ?SET_TIMER}
                                )
                            );
                        _Pids ->
                            ErrMsg = jstr(
                                "multiple nodes found for msg.target '~s'",
                                [Target]
                            ),
                            post_exception_or_debug(NodeDef, Msg, ErrMsg)
                    end;
                _ ->
                    ErrMsg = jstr("not target set on message", []),
                    post_exception_or_debug(NodeDef, Msg, ErrMsg)
            end;
        {ok, <<"static">>} ->
            case maps:find(<<"links">>, NodeDef) of
                {ok, Links} ->
                    send_msg_on(
                        Links,
                        update_linksource(
                            NodeDef,
                            Msg#{'_timeout_ref' => ?SET_TIMER}
                        )
                    );
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
    case maps:find('_timeout_ref', Msg) of
        {ok, TRef} ->
            erlang:cancel_timer(TRef);
        _ ->
            ignore
    end,
    send_msg_to_connected_nodes(NodeDef, Msg),
    {handled, NodeDef, Msg};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%
update_linksource(NodeDef, Msg) ->
    {ok, IdStr} = maps:find(<<"id">>, NodeDef),
    LinkBack = #{<<"id">> => generate_id(32), <<"node">> => IdStr},

    case maps:find('_linkSource', Msg) of
        {ok, Ary} ->
            maps:put('_linkSource', Ary ++ [LinkBack], Msg);
        _ ->
            maps:put('_linkSource', [LinkBack], Msg)
    end.
