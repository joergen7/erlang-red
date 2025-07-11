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
-import(ered_nodered_comm, [
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
    Status =
        case
            {cowboy_req:binding(nodeid, Req), cowboy_req:binding(action, Req)}
        of
            {undefined, _} ->
                404;
            {_, undefined} ->
                404;
            {IdStr, ActStr} ->
                WsName = websocket_name_from_request(Req),
                NodePid = nodeid_to_pid(WsName, IdStr),

                case NodePid of
                    {ok, Pid} ->
                        %% ensure that actstr is correct, there is no checking
                        %% of message types in the receivership code so an
                        %% incorrect value here will kill the process.
                        case ActStr of
                            <<"disable">> ->
                                gen_server:cast(Pid, {disable, WsName}),
                                201;
                            <<"enable">> ->
                                gen_server:cast(Pid, {enable, WsName}),
                                200;
                            _ ->
                                404
                        end;
                    {error, _} ->
                        404
                end
        end,

    Resp = cowboy_req:set_resp_body(<<"OK">>, Req),
    cowboy_req:reply(Status, Resp),
    {stop, Req, State}.

format_error(Reason, Req) ->
    {
        [
            {<<"error">>, <<"bad_request">>},
            {<<"reason">>, Reason}
        ],
        Req
    }.
