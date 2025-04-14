-module(ered_http_unittesting_halt_handler).

%%
%% endpoint to shutdown the entire Erlang process.
%%
-behaviour(cowboy_rest).

-export([
    init/2,
    allowed_methods/2,
    handle_response/2,
    content_types_provided/2,
    format_error/2
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, handle_response}], Req, State}.

handle_response(_Req, _State) ->
    halt(0).

format_error(Reason, Req) ->
    {
        [
            {<<"error">>, <<"bad_request">>},
            {<<"reason">>, Reason}
        ],
        Req
    }.
