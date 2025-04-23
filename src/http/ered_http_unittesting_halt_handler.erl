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

handle_response(Req, State) ->
    %% stupid little endpoint to cycle the server that is stuck in a
    %% while [ 1 ] ; ... loop - in the frontend the functionality is hidden
    %% away behind a shortcut.
    Pid = spawn(fun() ->
        receive
            _ -> halt(0)
        end
    end),
    timer:send_after(500, Pid, ok),
    {<<"{\"status\": \"ok\"}">>, Req, State}.

format_error(Reason, Req) ->
    {
        [
            {<<"error">>, <<"bad_request">>},
            {<<"reason">>, Reason}
        ],
        Req
    }.
