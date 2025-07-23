-module(ered_http_redirect).

-behaviour(cowboy_handler).

% erlfmt:ignore - alignment
-define(DefineResponse(Status),
    cowboy_req:reply(Status, #{<<"location">> =>
        case Qs of
            <<>> ->  Location;
            _ ->     Location ++ "?" ++ Qs
        end
    }, Req)
).

%%
%% Redirect one path to another path, either permament(301) or temporarily(303)
%%
-export([
    init/2
]).

init(Req, {permanently, Location} = State) ->
    #{qs := Qs} = Req,
    {ok, ?DefineResponse(301), State};
init(Req, {temporarily, Location} = State) ->
    #{qs := Qs} = Req,
    {ok, ?DefineResponse(307), State}.
