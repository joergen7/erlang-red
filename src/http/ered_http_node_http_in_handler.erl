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
    %% TODO should check the method of the request to better distinguish
    %% TODO between POST, GET, HEAD, PUT & DELETE
    {ok, HttpInPid} = maps:find(pid, State),
    {ok, WsName} = maps:find(wsname, State),

    %% create a base Msg with _msgid and _ws as attributes
    {outgoing, Msg} = create_outgoing_msg(WsName),

    %% add the this pid so that a reply can be sent to the client
    Msg2 = maps:put(reqpid, self(), Msg),

    %% add the bindings of any parameters in the path plus other stuff
    %% into the req object
    {ok, Body, Req2} = read_entire_body(Req),

    ReqObj = #{
       body => Body,
       params => cowboy_req:bindings(Req),
       cookies => cowboy_req:parse_cookies(Req),
       query => cowboy_req:parse_qs(Req)
    },
    Msg3 = maps:put(req, ReqObj, Msg2),

    %% add the body as payload
    Msg4 = maps:put(payload, Body, Msg3),

    gen_server:cast(HttpInPid, {outgoing, Msg4}),

    {cowboy_loop, Req2, State, hibernate}.

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


%%
%%
read_entire_body(Req) ->
    read_body(Req, <<"">>).

read_body(Req0, Acc) ->
    case cowboy_req:read_body(Req0) of
        {ok, Data, Req} -> {ok, << Acc/binary, Data/binary >>, Req};
        {more, Data, Req} -> read_body(Req, << Acc/binary, Data/binary >>)
    end.
