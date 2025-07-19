-module(ered_node_link_call).

-include("ered_nodes.hrl").

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
    unsupported/3
]).
-import(ered_link_node_exchange, [
    obtain_link_node_pid/2
]).

-define(SetUpTimer,
    erlang:start_timer(
        maps:get(<<"timeout">>, NodeDef), self(), {msg_timed_out, Msg}
    )
).

%%
%%
start(#{<<"timeout">> := Val} = NodeDef, _WsName) ->
    % Ensure timeout value is set to a number and converted to milliseconds
    ToutVal =
        try
            case binary_to_integer(Val) * 1000 of
                N when N < 0 -> 0;
                N -> N
            end
        catch
            _E:_F:_S -> 30_000
        end,
    ered_node:start(NodeDef#{<<"timeout">> => ToutVal}, ?MODULE);
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef#{<<"timeout">> => 30_000}, ?MODULE).

%%
%%
handle_event({msg_timed_out, Msg}, NodeDef) ->
    post_exception_or_debug(NodeDef, Msg, <<"timeout">>),
    NodeDef;
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
%% Dynamic link type, the message should contain the target details.
handle_msg(
    {incoming, #{<<"target">> := Target} = Msg},
    #{<<"linkType">> := <<"dynamic">>} = NodeDef
) ->
    #{?GetWsName} = Msg,
    case obtain_link_node_pid(Target, WsName) of
        [] ->
            ErrMsg = jstr("Error: target link-in node '~s' not found", [Target]),
            post_exception_or_debug(NodeDef, Msg, ErrMsg);
        [Pid] ->
            %% Exactly one Pid, perfect - everything else is
            %% is an error
            send_msg_on_by_pids(
                [Pid],
                update_linksource(NodeDef, Msg#{'_timeout_ref' => ?SetUpTimer})
            );
        _Pids ->
            ErrMsg = jstr("multiple nodes found for msg.target '~s'", [Target]),
            post_exception_or_debug(NodeDef, Msg, ErrMsg)
    end,
    {handled, NodeDef, dont_send_complete_msg};
% no target details in the mssage using dynamic target ==> exception.
handle_msg(
    {incoming, Msg},
    #{<<"linkType">> := <<"dynamic">>} = NodeDef
) ->
    ErrMsg = jstr("no target set on message", []),
    post_exception_or_debug(NodeDef, Msg, ErrMsg),
    {handled, NodeDef, dont_send_complete_msg};
% static target
handle_msg(
    {incoming, Msg},
    #{<<"linkType">> := <<"static">>, <<"links">> := Links} = NodeDef
) ->
    send_msg_on(
        Links, update_linksource(NodeDef, Msg#{'_timeout_ref' => ?SetUpTimer})
    ),
    {handled, NodeDef, dont_send_complete_msg};
% static target, no links defined --> ignore
handle_msg(
    {incoming, _Msg},
    #{<<"linkType">> := <<"static">>} = NodeDef
) ->
    {handled, NodeDef, dont_send_complete_msg};
% Unknown link type
handle_msg(
    {incoming, #{?GetWsName} = Msg},
    #{<<"linkType">> := Unknown} = NodeDef
) ->
    ErrMsg = jstr("Unknown LinkType: '~s'", [Unknown]),
    this_should_not_happen(NodeDef, io_lib:format("~p ~p\n", [ErrMsg, Msg])),
    debug(WsName, debug_string(NodeDef, ErrMsg), notice),
    ?NodeStatus("unknown linkType", "red", "dot"),
    {handled, NodeDef, dont_send_complete_msg};
%%
%% This comes from a link out node in return mode, this means we pass
%% the message on to all the nodes connected to us, i.e. the 'wires'
%% attribute.
handle_msg({link_return, #{'_timeout_ref' := TRef} = Msg}, NodeDef) ->
    erlang:cancel_timer(TRef),
    send_msg_to_connected_nodes(NodeDef, Msg),
    {handled, NodeDef, Msg};
handle_msg({link_return, Msg}, NodeDef) ->
    send_msg_to_connected_nodes(NodeDef, Msg),
    {handled, NodeDef, Msg};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%% --------------------- Helpers
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
