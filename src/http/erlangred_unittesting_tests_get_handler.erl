-module(erlangred_unittesting_tests_get_handler).

-behaviour(cowboy_rest).

-export([init/2,
         allowed_methods/2,
         handle_response/2,
         content_types_provided/2,
         format_error/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {ok,CurrMeth} = maps:find(method,Req),
    {[CurrMeth], Req, State}.

content_types_provided(Req,State) ->
    { [{{ <<"application">>, <<"json">>, '*'}, handle_response}], Req, State }.

handle_response(Req, State) ->
    flow_store_server:update_all_flows(),
    Response = #{ status => ok,
                  last_updated_at => "",
                  data => flow_store_server:get_flow_data() },
    { jiffy:encode(Response), Req, State}.

format_error(Reason, Req) ->
    {[
        {<<"error">>, <<"bad_request">>},
        {<<"reason">>, Reason}
    ], Req}.
