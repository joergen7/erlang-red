-module(cowboy_inject_node_handler).

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

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_json_body},
      {<<"application/json; charset=utf-8">>, handle_json_body}
     ], Req, State}.

handle_json_body(Req, State) ->
    Resp = cowboy_req:set_resp_body(<<"OK">>, Req),

    case cowboy_req:binding(nodeid, Req) of
        undefined ->
            ok;
        IdStr ->
            NodeIdToPid = binary_to_atom(
                            list_to_binary(
                              lists:flatten(
                                io_lib:format("~s~s",["node_pid_",IdStr])))),

            case whereis(NodeIdToPid) of
                undefined ->
                    ok;
                _ ->
                    io:format("Inject action found pid!~n"),
                    NodeIdToPid ! { outgoing,
                                    #{ '_msgid' => nodes:generate_id() } }
            end
    end,

    {true, Resp, State}.

format_error(Reason, Req) ->
    {[
        {<<"error">>, <<"bad_request">>},
        {<<"reason">>, Reason}
    ], Req}.
