-module(ered_http_nodered_inject_node_button_handler).

-behaviour(cowboy_rest).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    handle_json_body/2,
    format_error/2
]).

-import(ered_nodes, [
    nodeid_to_pid/2
]).
-import(ered_nodered_comm, [
    websocket_name_from_request/1
]).
-import(ered_messages, [
    create_outgoing_msg/1
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {
        [
            {<<"application/json">>, handle_json_body},
            {<<"application/json; charset=utf-8">>, handle_json_body}
        ],
        Req,
        State
    }.

handle_json_body(Req, State) ->
    Resp = cowboy_req:set_resp_body(<<"OK">>, Req),
    WsName = websocket_name_from_request(Req),

    case cowboy_req:binding(nodeid, Req) of
        undefined ->
            ok;
        IdStr ->
            case nodeid_to_pid(WsName, IdStr) of
                {ok, Pid} ->
                    gen_server:cast(Pid, create_outgoing_msg(WsName));
                {error, _} ->
                    ignore
            end
    end,

    {true, Resp, State}.

format_error(Reason, Req) ->
    {
        [
            {<<"error">>, <<"bad_request">>},
            {<<"reason">>, Reason}
        ],
        Req
    }.
