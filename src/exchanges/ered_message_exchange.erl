-module(ered_message_exchange).

-export([
    clear_completed_group/2,
    clear_exception_group/2,
    post_completed/2,
    post_exception/3,
    subscribe_to_completed/3,
    subscribe_to_exception/3
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
subscribe_to_exception(NodeDef, WsName, Pid) ->
    pg:join(pg_exception_group_name(NodeDef, WsName), Pid).

%%
%%
subscribe_to_completed(NodeDef, WsName, Pid) ->
    pg:join(pg_complete_group_name(NodeDef, WsName), Pid).

%%
%% Post exception passes errors to the catch nodes, if any are defined.
%% It also provides feed back to the caller as to whether there was a
%% catch node or not, if not, deal with it yourself is the message.
%%
post_exception(SrcNode, SrcMsg, ErrMsg) ->
    case pg:get_members(pg_exception_group_name(SrcNode, ws_from(SrcMsg))) of
        [] ->
            deal_with_it_yourself;
        Members ->
            [M ! {exception, SrcNode, SrcMsg, ErrMsg} || M <- Members],
            dealt_with
    end.

%%
%% Called after a node has completed handling a msg
post_completed(_NodeDef, dont_send_complete_msg) ->
    ok;
post_completed(NodeDef, Msg) ->
    case pg:get_members(pg_complete_group_name(NodeDef, ws_from(Msg))) of
        Members ->
            [M ! {completed_msg, NodeDef, Msg} || M <- Members]
    end.

%% clear out the pg group for the complete nodes.
clear_completed_group(FlowId, WsName) ->
    clear_pg_group(pg_complete_group_name(#{z => FlowId}, WsName)).

clear_exception_group(FlowId, WsName) ->
    clear_pg_group(pg_exception_group_name(#{z => FlowId}, WsName)).

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
pg_exception_group_name(NodeDef, WsName) ->
    {ok, FlowId} = maps:find(z, NodeDef),
    jstr("catch_nodes_~s_~s", [atom_to_list(WsName), FlowId]).

pg_complete_group_name(NodeDef, WsName) ->
    {ok, FlowId} = maps:find(z, NodeDef),
    jstr("complete_nodes_~s_~s", [atom_to_list(WsName), FlowId]).
