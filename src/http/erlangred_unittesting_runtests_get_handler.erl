-module(erlangred_unittesting_runtests_get_handler).

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
    WsName = nodered:websocket_name_from_request(Req),
    case cowboy_req:binding(flowid, Req) of
        undefined ->
            {<<"{}">>, Req, State};
        <<"all">> ->
            AllFlowIds = flow_store_server:all_flow_ids(),
            [
                unittest_engine ! {start_test, FlowId, WsName}
             || FlowId <- AllFlowIds
            ],
            {jiffy:encode(#{status => ok}), Req, State};
        FlowId ->
            unittest_engine ! {start_test, FlowId, WsName},
            {jiffy:encode(#{status => ok}), Req, State}
    end.

format_error(Reason, Req) ->
    {
        [
            {<<"error">>, <<"bad_request">>},
            {<<"reason">>, Reason}
        ],
        Req
    }.
