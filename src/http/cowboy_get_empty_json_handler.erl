-module(cowboy_get_empty_json_handler).

%%
%% Get blower - this returns an empty hash. This is used for returning the
%% credentials for a flow tab. Double click on a flow tab and this request
%% is set.
%%
%% Also used for the contexts - that are forever empty.
%%

-behaviour(cowboy_rest).

-export([init/2,
         allowed_methods/2,
         handle_response/2,
         content_types_provided/2,
         format_error/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

content_types_provided(Req,State) ->
    { [{{ <<"application">>, <<"json">>, '*'}, handle_response}], Req, State }.

handle_response(Req, State) ->
    case maps:find(path,Req) of
        {ok, <<"/library/local/flows/">>} ->
            {<<"[]">>, Req, State};
        _ ->
            {<<"{}">>, Req, State}
    end.

format_error(Reason, Req) ->
    {[
        {<<"error">>, <<"bad_request">>},
        {<<"reason">>, Reason}
    ], Req}.
