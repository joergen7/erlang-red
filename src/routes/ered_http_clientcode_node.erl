-module(ered_http_clientcode_node).

-behaviour(cowboy_rest).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    handle_clientcode_request/2,
    format_error/2
]).

-import(ered_nodered_comm, [
    node_status/5,
    node_status_clear/2,
    websocket_name_from_request/1
]).

-import(ered_nodes, [
    nodeid_to_pid/2
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

%erlfmt:ignore - alignment
content_types_accepted(Req, State) ->
    {
        [
            {<<"application/json">>,                handle_clientcode_request},
            {<<"application/json; charset=utf-8">>, handle_clientcode_request},
            {<<"application/x-json-restart">>,      handle_clientcode_request}
        ],
        Req,
        State
    }.

handle_clientcode_request(Req, State) ->
    case
        {
            websocket_name_from_request(Req),
            cowboy_req:binding(task, Req),
            cowboy_req:binding(nodeid, Req)
        }
    of
        {_, _, undefined} ->
            ignore;
        {none, _, _} ->
            ignore;
        {WsName, <<"status">>, NodeId} ->
            {ok, Body, _Req2} = ered_http_utils:read_body(Req, <<"">>),
            case json:decode(Body) of
                #{<<"fill">> := Clr, <<"shape">> := Shp, <<"text">> := Txt} ->
                    node_status(WsName, #{<<"id">> => NodeId}, Txt, Clr, Shp);
                #{} ->
                    node_status_clear(WsName, #{<<"id">> => NodeId})
            end;
        {WsName, undefined, NodeId} ->
            {ok, Body, _Req2} = ered_http_utils:read_body(Req, <<"">>),
            case
                {
                    nodeid_to_pid(WsName, NodeId),
                    json:decode(Body)
                }
            of
                {{ok, Pid}, Msg} ->
                    gen_server:cast(Pid, {client_code, Msg#{'_ws' => WsName}});
                _ ->
                    ignored
            end
    end,

    {true, cowboy_req:set_resp_body(<<"ok">>, Req), State}.

format_error(Reason, Req) ->
    {
        [
            {<<"error">>, <<"bad_request">>},
            {<<"reason">>, Reason}
        ],
        Req
    }.
