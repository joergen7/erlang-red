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
    Resp = cowboy_req:set_resp_body(<<"{\"rev\":\"dead73d0\"}">>, Req),
    {true, Resp, State}.

handle_json_body(Req, State) ->
    {ok, _Body, Req2} = ered_http_utils:read_body(Req, <<"">>),

    %%
    %% TODO all this stuff needs to be pushed off to another Pids doing
    %% TODO flow management.
    %%
    %% Push = fun(Key, Value, Acc) ->
    %%    [{binary_to_atom(Key), Value} | Acc]
    %% end,

    %% {FlowMap,_,_} = json:decode(Body, ok, #{object_push => Push}),
    %% {ok, NodeAry} = maps:find(flows,FlowMap),
    %% WsName = nodered:websocket_name_from_request(Req),

    %% Pids = nodes:create_pid_for_node(NodeAry, WsName),

    %% io:format("Pids for flow: ~p\n",[Pids]),

    Resp = cowboy_req:set_resp_body(<<"{\"rev\":\"dead73d0\"}">>, Req2),
    {true, Resp, State}.

format_error(Reason, Req) ->
    {
        [
            {<<"error">>, <<"bad_request">>},
            {<<"reason">>, Reason}
        ],
        Req
    }.
