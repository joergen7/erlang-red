-module(ered_http_nodered_flow_deploy_handler).

-behaviour(cowboy_rest).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    handle_flow_restart/2,
    handle_json_body/2,
    format_error/2
]).

-import(ered_compute_engine, [
    deploy/2,
    reload/1
]).
-import(ered_nodered_comm, [
    websocket_name_from_request/1
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {
        [
            {<<"application/json">>, handle_json_body},
            {<<"application/json; charset=utf-8">>, handle_json_body},
            {<<"application/x-json-restart">>, handle_flow_restart}
        ],
        Req,
        State
    }.

handle_flow_restart(Req, State) ->
    WsName = websocket_name_from_request(Req),
    Resp = cowboy_req:set_resp_body(reload(WsName), Req),
    {true, Resp, State}.

handle_json_body(Req, State) ->
    {ok, Body, Req2} = ered_http_utils:read_body(Req, <<"">>),
    WsName = websocket_name_from_request(Req),
    Resp = cowboy_req:set_resp_body(deploy(Body, WsName), Req2),
    {true, Resp, State}.

format_error(Reason, Req) ->
    {
        [
            {<<"error">>, <<"bad_request">>},
            {<<"reason">>, Reason}
        ],
        Req
    }.
