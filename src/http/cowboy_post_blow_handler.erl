-module(cowboy_post_blow_handler).

%%
%% Post blower - anything that is posted here gets an "ok, you're doing good"
%% response. A feel-good poster person.
%%

-behaviour(cowboy_rest).

-export([init/2,
         allowed_methods/2,
         content_types_accepted/2,
         handle_json_body/2,
         format_error/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_json_body},
      {<<"application/json; charset=utf-8">>, handle_json_body}
     ], Req, State}.

handle_json_body(Req, State) ->
    {ok, _Body, Req2} = cowboy_req:read_urlencoded_body(Req),
    Resp = cowboy_req:set_resp_body(<<"{\"rev\":\"dead73dabcdef12345\"}">>, Req2),
    {true, Resp, State}.


format_error(Reason, Req) ->
    {[
        {<<"error">>, <<"bad_request">>},
        {<<"reason">>, Reason}
    ], Req}.
