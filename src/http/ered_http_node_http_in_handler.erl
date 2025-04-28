-module(ered_http_node_http_in_handler).

-behaviour(cowboy_loop).

-export([
    init/2,
    info/3
]).

%%
%% This is the http handler if a http in node registers a http endpoint.
%% All this does is send the http in node a message and wait for
%% messages back from other nodes to send back to the client.
%%

-import(ered_nodered_comm, [
    websocket_name_from_request/1
]).
-import(ered_msg_handling, [
    create_outgoing_msg/1
]).

init(Req, State) ->
    {ok, HttpInPid} = maps:find(pid, State),
    {ok, WsName} = maps:find(wsname, State),

    {outgoing, Msg} = create_outgoing_msg(WsName),
    Msg2 = maps:put(reqpid, self(), Msg),

    gen_server:cast(HttpInPid, {outgoing, Msg2}),
    {cowboy_loop, Req, State, hibernate}.

%%
%%
info({reply, Headers, Body}, Req, State) ->
    Req2 = cowboy_req:set_resp_cookie(
        <<"wsname">>,
        atom_to_list(maps:get(wsname, State)),
        Req
    ),
    cowboy_req:reply(200, Headers, Body, Req2),
    {stop, Req2, State};
info(_Msg, Req, State) ->
    {ok, Req, State, hibernate}.
