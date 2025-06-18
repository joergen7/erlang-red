-module(ered_link_node_exchange).

-export([
    obtain_link_node_pid/2,
    register_link_in/3,
    unregister_link_in/2
]).

%%
%% Exchange for handling the dynamic link calls. Each link in node registers
%% with the exchange and if a link call receives a messsage and is dynamic,
%% then it comes here to get an Pid to contact.
%%

-import(ered_nodes, [
    jstr/2
]).

%%
%% Called by a link in node to register itself. A link in node is registered
%% by its name and its id. Of course, if the name of an link node changes
%% then this needs to be updated. This is solved by recreating the flow
%% completely on each deploy.
%%
-spec register_link_in(NodeDef :: map(), WsName :: string(), Pid :: pid()) ->
    ok | {error, ErrMsg :: string()}.
register_link_in(NodeDef, WsName, Pid) ->
    {ok, Name} = maps:find(<<"name">>, NodeDef),
    pg:join(pg_link_node_name(Name, WsName), Pid),
    {ok, NodeId} = maps:find(<<"id">>, NodeDef),
    pg:join(pg_link_node_name(NodeId, WsName), Pid),
    ok.

%%
%%
-spec obtain_link_node_pid(Name :: string(), WsName :: string()) ->
    Pids :: [pid()].
obtain_link_node_pid(Name, WsName) ->
    GrpName = pg_link_node_name(Name, WsName),
    pg:get_members(GrpName).

%%
%% Clear the groups for this link in node. Warning this could have strange
%% effects if link in nodes happen to have the same names. Beware of the
%% same named link in nodes ... just don't do it!
-spec unregister_link_in(NodeDef :: map(), WsName :: string()) -> ok.
unregister_link_in(NodeDef, WsName) ->
    {ok, Name} = maps:find(<<"name">>, NodeDef),
    clear_pg_group(pg_link_node_name(Name, WsName)),
    {ok, NodeId} = maps:find(<<"id">>, NodeDef),
    clear_pg_group(pg_link_node_name(NodeId, WsName)),
    ok.

%%
%%
pg_link_node_name(Name, WsName) ->
    jstr("link_node[~s][~s]", [WsName, Name]).

%%
%%
clear_pg_group(GrpName) ->
    case pg:get_members(GrpName) of
        Members ->
            [pg:leave(GrpName, M) || M <- Members]
    end.
