-module(ered_http_nodered_debug_node_active).

-behaviour(cowboy_rest).

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    handle_json_body/2,
    format_error/2
]).

-import(ered_nodes, [
    nodeid_to_pid/2
]).
-import(nodered, [
    websocket_name_from_request/1
]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    %% Cowboy wildcard seems to be '*' and not <<'*'>>, i.e. the star-atom!
    {[{'*', handle_json_body}], Req, State}.

handle_json_body(Req, State) ->
    case {cowboy_req:binding(nodeid, Req), cowboy_req:binding(action, Req)} of
        {undefined, _} ->
            ok;
        {_, undefined} ->
            ok;
        {IdStr, ActStr} ->
            WsName = websocket_name_from_request(Req),
            NodePid = nodeid_to_pid(WsName, IdStr),

            case whereis(NodePid) of
                undefined ->
                    ok;
                _ ->
                    %% ensure that actstr is correct, there is no checking
                    %% of message types in the receivership code so an
                    %% incorrect value here will kill the process.
                    case ActStr of
                        <<"disable">> ->
                            NodePid ! {disable, WsName};
                        <<"enable">> ->
                            NodePid ! {enable, WsName};
                        _ ->
                            ok
                    end
            end
    end,
    Resp = cowboy_req:set_resp_body(<<"OK">>, Req),
    {true, Resp, State}.

format_error(Reason, Req) ->
    {
        [
            {<<"error">>, <<"bad_request">>},
            {<<"reason">>, Reason}
        ],
        Req
    }.
