-module(erlang_red).

-behaviour(application).
-export([start/0, start/2, stop/1, stop/0,constraint/2]).

constraint(Arg1,Arg2) ->
    io:format("Constraint: ~p ~p\n",[Arg1,Arg2]),
    {ok, Arg2}.

start() ->
    application:ensure_all_started([cowboy, erlang_red]).

start(_Type, _Args) ->
    io:format("start/2 called on erlang_red\n"),
    Dispatch = cowboy_router:compile([
        {'_', [
               %%
               %% Sock'em in the eye websocket
               %%
               {"/node-red/comms", cowboy_nodered_websocket,
                [{stats_interval, 15000}]},

               %%
               %% POST handlers
               %%
               %% this is a special one for this project
               {"/testcase/:workspaceid/create",
                cowboy_testcase_post_handler, []},

               %% these are required by Node-RED
               {"/UnitTesting/tests.json",
                cowboy_unittesting_tests_get_handler, []},
               %% {"/UnitTesting/:flowid/runtest",
               %%  cowboy_unittesting_runtests_get_handler, []},
               {"/UnitTesting/:flowid/retrieve",
                cowboy_unittesting_retrieve_flow_handler, []},

               {"/settings/user", cowboy_post_blow_handler, []},
               {"/nodes", cowboy_post_blow_handler, []},
               {"/flows", cowboy_flow_deploy_handler, []},
               {"/inject/:nodeid", cowboy_inject_node_handler, []},

               %%
               %% GET handlers for delivery of the static content
               %%
               %% TODO the constraints here DONT WORK - Cowboy just
               %% TODO ignores them because Bindings is empty.
               %%
               {"/library/local/flows/", [{method,<<"GET">>}],
                cowboy_get_empty_json_handler, []},

               {"/credentials/[...]", [{method,<<"GET">>}],
                cowboy_get_empty_json_handler, []},

               {"/context/[...]", [{method,<<"GET">>}],
                cowboy_get_empty_json_handler, []},

               {"/node-red", [{method,<<"GET">>}], cowboy_static,
                {file, "./node-red-frontend/index.html"} },

               {"/[...]", [{method,<<"GET">>}], cowboy_static,
                {dir, "./node-red-frontend",
                 [{mimetypes, mimetypes_nodered, mt}]
               }}
              ]
        }
    ]),

    flow_store_server:start(),

    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ).

stop() ->
    application:stop(cowboy).


stop(_State) ->
    ok.
