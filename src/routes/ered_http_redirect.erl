-module(ered_http_redirect).

-behaviour(cowboy_handler).

%%
%% Redirect one path to another path, either permament(301) or temporarily(303)
%%
-export([
    init/2
]).

init(Req, {permanently, Location} = State) ->
    #{qs := Qs} = Req,
    Resp =
        cowboy_req:reply(301, #{<<"location">> => Location ++ "?" ++ Qs}, Req),
    {ok, Resp, State};
init(Req, {temporarily, Location} = State) ->
    #{qs := Qs} = Req,
    Resp =
        cowboy_req:reply(307, #{<<"location">> => Location ++ "?" ++ Qs}, Req),
    {ok, Resp, State}.
