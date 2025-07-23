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
%% Redirect one path to another path, including the query string if defined.
%%
%% Currently: either permament(301) or temporarily(303)
%%
-export([
    init/2
]).

init(#{qs := Qs} = Req, {permanently, Location} = State) ->
    {ok, ?DefineResponse(301), State};
init(#{qs := Qs} = Req, {temporarily, Location} = State) ->
    {ok, ?DefineResponse(307), State}.
