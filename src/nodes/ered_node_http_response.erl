-module(ered_node_http_response).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Node to be used in conjunction with the http in node, This nodes sends
%% a response to a http connection.
%%

-import(ered_nodes, [
    jstr/2
]).
-import(ered_nodered_comm, [
    unsupported/3,
    ws_from/1
]).
-import(ered_messages, [
    convert_to_num/1,
    map_keys_to_binary/1
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%% This sends a message to the ered_http_node_http_in_handler telling it
%% to send the payload to the client.
handle_msg({incoming, Msg}, NodeDef) ->
    StatusCode = retrieve_status_code(NodeDef, Msg),
    Headers = retrieve_headers(NodeDef, Msg),
    {ok, ReqPid} = maps:find(<<"reqpid">>, Msg),

    ReqPid !
        {reply, StatusCode, Headers, ws_from(Msg),
            maps:get(<<"payload">>, Msg)},

    {handled, NodeDef, Msg};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%% Retrieve statusCode either from Msg or from NodeDef, NodeDef has preference.
%% erlfmt:ignore alignment
retrieve_status_code(NodeDef, Msg) ->
    case {maps:find(<<"statusCode">>, Msg),
          maps:find(<<"statusCode">>, NodeDef)} of
        {{ok, MsgSC}, {ok, <<>>}} -> convert_to_num(MsgSC);
        {{ok, _},     {ok, NdSC}} -> convert_to_num(NdSC);
        {{ok, MsgSC}, _}          -> convert_to_num(MsgSC);
        {_, {ok, <<>>}}           -> 200;
        {_, {ok, NdSC}}           -> convert_to_num(NdSC);
        {_, _}                    -> 200
    end.

%%
%% Retrieve headers from either the NodeDef or Msg, NodeDef has preference.
%% erlfmt:ignore alignment
retrieve_headers(NodeDef, Msg) ->
    Hdrs = case {maps:find(<<"headers">>, Msg),
                 maps:find(<<"headers">>, NodeDef)} of
               {{ok, MsgHdrs}, {ok, #{}}}    -> MsgHdrs;
               {{ok, _},       {ok, NdHdrs}} -> NdHdrs;
               {_,             {ok, NdHdrs}} -> NdHdrs;
               {{ok, MsgHdrs}, _}            -> MsgHdrs;
               {_, _}                        -> #{}
           end,
    map_keys_to_binary(Hdrs).
