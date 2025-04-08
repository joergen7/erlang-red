-module(cowboy_testcase_post_handler).

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
    {ok,CurrMeth} = maps:find(method,Req),
    {[CurrMeth], Req, State}.

content_types_accepted(Req, State) ->
    ContentType = cowboy_req:header(<<"content-type">>, Req),
    {[{ContentType, handle_json_body}], Req, State}.

handle_json_body(Req, State) ->
    io:format("Handling testcase creationg\n"),
    WorkspaceId = cowboy_req:binding(workspaceid, Req),

    {ok, Body, Req2} = cowboy_flow_deploy_handler:read_body(Req, <<"">>),

    Push = fun(Key, Value, Acc) ->
       [{binary_to_atom(Key), Value} | Acc]
    end,
    {FlowMap,_,_} = json:decode(Body, ok, #{object_push => Push}),
    {ok, NodeAry} = maps:find(flow,FlowMap),

    FileName = io_lib:format("flow.~s.json",[WorkspaceId]),
    DestFileName = io_lib:format("priv/testflows/~s",[FileName]),

    file:write_file(DestFileName, NodeAry),

    Resp = cowboy_req:set_resp_body(
             jiffy:encode(#{ name => list_to_binary(lists:flatten(FileName)) } ), Req2),

    {true, Resp, State}.


format_error(Reason, Req) ->
    {[
        {<<"error">>, <<"bad_request">>},
        {<<"reason">>, Reason}
    ], Req}.
