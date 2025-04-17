-module(ered_http_unittesting_retrieve_flow_handler).

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
    {ok, CurrMeth} = maps:find(method, Req),
    {[CurrMeth], Req, State}.

content_types_provided(Req, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, handle_response}], Req, State}.

handle_response(Req, State) ->
    case cowboy_req:binding(flowid, Req) of
        undefined ->
            {<<"[]">>, Req, State};
        FlowId ->
            FileName = flow_store_server:get_filename(FlowId),
            case file:read_file(FileName) of
                {ok, FileData} ->
                    {
                        json:encode(#{flowdata => json:decode(FileData)}),
                        Req,
                        State
                    };
                %%
                %% File not found, send empty content. This is good for the
                %% FlowCompare plugin/node that uses this endpoint.
                _ ->
                    {json:encode(#{flowdata => []}), Req, State}
            end
    end.

format_error(Reason, Req) ->
    {
        [
            {<<"error">>, <<"bad_request">>},
            {<<"reason">>, Reason}
        ],
        Req
    }.
