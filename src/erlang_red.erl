-module(erlang_red).

-behaviour(application).
-export([start/0, start/2, stop/1, stop/0]).

start() ->
    application:ensure_all_started([cowboy]).

start(_Type, _Args) ->
    erlang_red_sup:start_link().

stop() ->
    application:stop(cowboy).

stop(_State) ->
    ok.
