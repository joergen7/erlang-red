-module(ered_http_testcase_post_handler).

%%
%% This stores testcases to disk. Triggered by "create test case" button
%% on the export flow dialog.
%%

-behaviour(cowboy_rest).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    handle_json_body/2,
    format_error/2
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {ok, CurrMeth} = maps:find(method, Req),
    {[CurrMeth], Req, State}.

content_types_accepted(Req, State) ->
    ContentType = cowboy_req:header(<<"content-type">>, Req),
    {[{ContentType, handle_json_body}], Req, State}.

handle_json_body(Req, State) ->
    WorkspaceId = cowboy_req:binding(workspaceid, Req),

    {ok, Body, Req2} = ered_http_utils:read_body(Req, <<"">>),

    Push = fun(Key, Value, Acc) ->
        [{binary_to_atom(Key), Value} | Acc]
    end,
    {FlowMap, _, _} = json:decode(Body, ok, #{object_push => Push}),
    {ok, NodeAry} = maps:find(flow, FlowMap),

    FileName = io_lib:format("flow.~s.json", [WorkspaceId]),
    DestFileName = io_lib:format(
        "~s/testflows/~s",
        [code:priv_dir(erlang_red), FileName]
    ),

    file:write_file(DestFileName, NodeAry),

    flow_store_server:update_flow(
        WorkspaceId,
        list_to_binary(lists:flatten(DestFileName))
    ),

    Resp = cowboy_req:set_resp_body(
        json:encode(#{name => list_to_binary(lists:flatten(FileName))}),
        Req2
    ),

    {true, Resp, State}.

format_error(Reason, Req) ->
    {
        [
            {<<"error">>, <<"bad_request">>},
            {<<"reason">>, Reason}
        ],
        Req
    }.
