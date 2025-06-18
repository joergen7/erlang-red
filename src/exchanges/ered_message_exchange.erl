-module(ered_message_exchange).

-export([
    clear_completed_group/2,
    clear_exception_group/2,
    clear_pg_group/1,
    post_completed/2,
    post_exception/3,
    subscribe_to_completed/3,
    subscribe_to_exception_entire_flow/3,
    subscribe_to_exception_from_node/3
]).

%%
%% Using pg module to group Pids together, this module provides a message
%% exchange for completed messages and exception messages.
%%
%% Any node wanting to get access to these messages can subscribe while
%% all nodes can send messages into the exchange.
%%
%% Whether this should be a gen_server is questionable since pg is already
%% a server and this is just a context specific interface to a pg store.
%%

-import(ered_nodes, [
    jstr/2
]).
-import(ered_nodered_comm, [
    ws_from/1
]).

%%
%%
subscribe_to_exception_entire_flow(NodeDef, WsName, Pid) ->
    {ok, FlowId} = maps:find(<<"z">>, NodeDef),
    pg:join(pg_exception_group_name(FlowId, WsName), Pid).

subscribe_to_exception_from_node(NodeId, WsName, Pid) ->
    pg:join(pg_exception_group_name(NodeId, WsName), Pid).

%%
%%
subscribe_to_completed(NodeDef, WsName, Pid) ->
    pg:join(pg_complete_group_name(NodeDef, WsName), Pid).

%%
%% Post exception passes errors to the catch nodes, if any are defined.
%% It also provides feed back to the caller as to whether there was a
%% catch node or not, if not, deal with it yourself is the message.
%%
%% This is a Node-RED thing that if an exception occurs and no one is
%% listening, then it makes a sound and that sound is in the debug panel.
%% So the user is informed even when there is no catch node there to catch
%% the exception.
%%
post_exception(SrcNode, SrcMsg, ErrMsg) ->
    {ok, FlowId} = maps:find(<<"z">>, SrcNode),
    {ok, NodeId} = maps:find(<<"id">>, SrcNode),

    WsName = ws_from(SrcMsg),

    case get_members_of_someid(FlowId, WsName) of
        {empty} ->
            case get_members_of_someid(NodeId, WsName) of
                {empty} ->
                    deal_with_it_yourself;
                {ok, Members} ->
                    [
                        gen_server:cast(M, {exception, SrcNode, SrcMsg, ErrMsg})
                     || M <- Members
                    ],
                    dealt_with
            end;
        {ok, Members} ->
            [
                gen_server:cast(M, {exception, SrcNode, SrcMsg, ErrMsg})
             || M <- Members
            ],

            case get_members_of_someid(NodeId, WsName) of
                {empty} ->
                    dealt_with;
                {ok, Members} ->
                    [
                        gen_server:cast(M, {exception, SrcNode, SrcMsg, ErrMsg})
                     || M <- Members
                    ],
                    dealt_with
            end
    end.

%%
%% Called after a node has completed handling a msg
post_completed(_NodeDef, dont_send_complete_msg) ->
    ok;
post_completed(NodeDef, Msg) ->
    case pg:get_members(pg_complete_group_name(NodeDef, ws_from(Msg))) of
        Members ->
            [gen_server:cast(M, {completed_msg, NodeDef, Msg}) || M <- Members]
    end.

%% clear out the pg group for the complete nodes.
clear_completed_group(FlowId, WsName) ->
    clear_pg_group(pg_complete_group_name(#{<<"z">> => FlowId}, WsName)).

clear_exception_group(FlowId, WsName) ->
    clear_pg_group(pg_exception_group_name(FlowId, WsName)).

%%
%% Clear pg group
clear_pg_group(GrpName) ->
    case pg:get_members(GrpName) of
        Members ->
            [pg:leave(GrpName, M) || M <- Members]
    end.

%%
%% Generate a group name for the pg module to group together all catch
%% nodes on one flow.
pg_exception_group_name(SomeId, WsName) ->
    %% Since Flow IDs and Node Ids are never the same, this is safe to
    %% do ... - .oO(add to collection of famous last words).
    jstr("catch_nodes_~s_~s", [atom_to_list(WsName), SomeId]).

pg_complete_group_name(NodeDef, WsName) ->
    {ok, FlowId} = maps:find(<<"z">>, NodeDef),
    jstr("complete_nodes_~s_~s", [atom_to_list(WsName), FlowId]).

%%
%% Helper for a friend.
get_members_of_someid(SomeId, WsName) ->
    case pg:get_members(pg_exception_group_name(SomeId, WsName)) of
        [] ->
            {empty};
        Members ->
            {ok, Members}
    end.
