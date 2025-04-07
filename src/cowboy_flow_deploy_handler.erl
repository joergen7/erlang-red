-module(cowboy_flow_deploy_handler).

-behaviour(cowboy_rest).

-export([init/2,
         allowed_methods/2,
         content_types_accepted/2,
         handle_json_body/2,
         format_error/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

%% init(_Transport, _Req, _Opts) ->
%%     {ok, undefined}.

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_json_body},
      {<<"application/json; charset=utf-8">>, handle_json_body}
     ], Req, State}.

handle_json_body(Req, State) ->
    {ok, Body, Req2} = read_body(Req, <<"">>),

    %%
    %% TODO all this stuff needs to be pushed off to another Pids doing
    %% TODO flow management.
    %%
    Push = fun(Key, Value, Acc) ->
       [{binary_to_atom(Key), Value} | Acc]
    end,

    {FlowMap,_,_} = json:decode(Body, ok, #{object_push => Push}),
    {ok, NodeAry} = maps:find(flows,FlowMap),

    Pids = nodes:create_pid_for_node(NodeAry),

    io:format("Pids for flow: ~p\n",[Pids]),

    Resp = cowboy_req:set_resp_body(<<"{\"rev\":\"dead73dabcdef12345\"}">>, Req2),
    %%cowboy_req:reply(201, Resp),
    {true, Resp, State}.

format_error(Reason, Req) ->
    {[
        {<<"error">>, <<"bad_request">>},
        {<<"reason">>, Reason}
    ], Req}.
