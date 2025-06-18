-module(ered_node_catch).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Catch node receives exceptions and passes these onto its connections.
%%
%% To adds an 'error' object to the Msg containing the following:
%%
%%          message: "Error: ENOENT: no such file or directory, open 'dfsafsdf'"
%%          source: object
%%              id: "39cedd1c01be1c7b"
%%              type: "file in"
%%              name: undefined
%%              count: 1            <--- not sure what this is TODO
%%          stack: "Error: ENOENT: no such file or directory, open 'dfsafsdf'"
%%
%% TODO Not sure about the stack
%%

-import(ered_nodes, [
    get_prop_value_from_map/2,
    jstr/1,
    send_msg_to_connected_nodes/2
]).
-import(ered_message_exchange, [
    subscribe_to_exception_entire_flow/3,
    subscribe_to_exception_from_node/3
]).

start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event({registered, WsName, Pid}, NodeDef) ->
    case maps:find(<<"scope">>, NodeDef) of
        {ok, null} ->
            subscribe_to_exception_entire_flow(NodeDef, WsName, Pid);
        {ok, NodeIds} ->
            [
                subscribe_to_exception_from_node(NodeId, WsName, Pid)
             || NodeId <- NodeIds
            ];
        _ ->
            ignore
    end,
    NodeDef;
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
% erlfmt:ignore - alignment
handle_msg({exception, FromDef, Msg, ErrMsg}, NodeDef) ->
    ErrObj = #{
        <<"message">> => ErrMsg,
        <<"stack">>   => ErrMsg,
        <<"source">>  => #{
            <<"id">>    => get_prop_value_from_map(<<"id">>,        FromDef),
            <<"type">>  => get_prop_value_from_map(<<"type">>,      FromDef),
            <<"name">>  => jstr(get_prop_value_from_map(<<"name">>, FromDef)),
            <<"count">> => maps:get('_mc_exception', NodeDef, 1)
        }
    },

    Msg2 = maps:put(<<"error">>, ErrObj, Msg),
    send_msg_to_connected_nodes(NodeDef, Msg2),
    {handled, NodeDef, Msg2};

%%
%%
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
