-module(erlang_red).

-behaviour(application).
-export([start/0, start/2, stop/1, stop/0]).

start() ->
    application:ensure_all_started([cowboy, erlang_red]).

start(_Type, _Args) ->
    io:format("start/2 called on erlang_red\n"),
    Dispatch = cowboy_router:compile([
        {'_', [
               {"/node-red", cowboy_static,
                {file, "./node-red-frontend/index.html"} },
               {"/[...]", cowboy_static,
                {dir, "./node-red-frontend",
                 [{mimetypes, node_red_mimetypes, mt}]
               }}
              ]
        }
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ).

stop() ->
    application:stop(cowboy).


stop(_State) ->
    ok.
