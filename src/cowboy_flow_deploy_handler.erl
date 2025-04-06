-module(cowboy_flow_deploy_handler).

-behaviour(cowboy_rest).

-export([init/2,
         allowed_methods/2,
         content_types_accepted/2,
         handle_body/2,
         handle_json_body/2,
         format_error/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

%% init(_Transport, _Req, _Opts) ->
%%     {ok, undefined}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/x-www-form-urlencoded">>, handle_body},
      {<<"application/json">>, handle_json_body},
      {<<"application/json; charset=utf-8">>, handle_json_body}
     ], Req, State}.

handle_json_body(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_urlencoded_body(Req),
    %% Process the received body here
    io:format("Found flow boud: ~p\n",[Body]),
    Resp = cowboy_req:set_resp_body(<<"{\"rev\":\"dead73dabcdef12345\"}">>, Req2),
    %%cowboy_req:reply(201, Resp),
    {true, Resp, State}.

handle_body(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_urlencoded_body(Req),
    %% Process the received body here
    {ok, Req2, State}.

format_error(Reason, Req) ->
    {[
        {<<"error">>, <<"bad_request">>},
        {<<"reason">>, Reason}
    ], Req}.
