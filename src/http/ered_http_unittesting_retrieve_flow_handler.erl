-module(ered_http_unittesting_retrieve_flow_handler).

-behaviour(cowboy_rest).

-export([
    init/2,
    allowed_methods/2,
    handle_testflow_sidebar/2,
    handle_testflow_initial/2,
    content_types_provided/2,
    format_error/2
]).

%% erlfmt:ignore alignment
-define(EmptyFlow, {<<"ea246f68766c8630">>, [#{id      => <<"ea246f68766c8630">>,
                                              type     => <<"tab">>,
                                              label    => <<"Flow 1">>,
                                              disabled => false,
                                              info     => <<"">>,
                                              env      => []}]}).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {ok, CurrMeth} = maps:find(method, Req),
    {[CurrMeth], Req, State}.

content_types_provided(Req, State) ->
    {
        [
            {{<<"application">>, <<"json">>, '*'}, handle_testflow_sidebar},
            {
                {<<"application">>, <<"x-json-testflow">>, '*'},
                handle_testflow_initial
            }
        ],
        Req,
        State
    }.

%%
%% This handles the usage of "?tstid=<flowid>" when opening the application.
%% It replaces the initial flow json with a specific test case, so that
%% the browser starts off with that test case.
handle_testflow_initial(Req, State) ->
    ParsedQs = cowboy_req:parse_qs(Req),
    AtomsQs = maps:from_list([{binary_to_atom(K), V} || {K, V} <- ParsedQs]),

    {Fid, Fdata} =
        case maps:find(tstid, AtomsQs) of
            {ok, FlowId} ->
                FileName = ered_flow_store_server:get_filename(FlowId),
                case file:read_file(FileName) of
                    {ok, FileData} ->
                        {FlowId, json:decode(FileData)};
                    _ ->
                        ?EmptyFlow
                end;
            _ ->
                ?EmptyFlow
        end,
    {construct_flow_construct(Fid, Fdata), Req, State}.

%%
%% This handles retrieval of test cases via the testing sidebar.
handle_testflow_sidebar(Req, State) ->
    case cowboy_req:binding(flowid, Req) of
        undefined ->
            {<<"[]">>, Req, State};
        FlowId ->
            FileName = ered_flow_store_server:get_filename(FlowId),
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

%%
%%
construct_flow_construct(FlowId, FlowData) ->
    json:encode(#{
        rev => <<"ea246f68766c8630ea246f68766c8630">>,
        revision => <<"fb0df6d24f37fbdf5b3ff97b723416ab4d5f00f9">>,
        flowid => FlowId,
        flows => FlowData
    }).
