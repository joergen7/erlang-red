-module(erlang_red).

-behaviour(application).
-export([start/0, start/2, stop/1, stop/0, constraint/2]).

constraint(Arg1,Arg2) ->
    io:format("Constraint: ~p ~p\n",[Arg1,Arg2]),
    {ok, Arg2}.

start() ->
    application:ensure_all_started([cowboy, erlang_red]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
               %%
               %% Sock'em in the eye websocket
               %%
               {"/node-red/comms",
                nodered_websocket, #{stats_interval => 30000}},

               %%
               %% POST handlers
               %%
               %% this is a special one for this project
               {"/testcase/:workspaceid/create",
                erlangred_testcase_post_handler, []},

               %% these are required by Node-RED
               {"/UnitTesting/tests.json",
                erlangred_unittesting_tests_get_handler, []},
               {"/UnitTesting/:flowid/runtest",
                erlangred_unittesting_runtests_get_handler, []},
               {"/UnitTesting/:flowid/retrieve",
                erlangred_unittesting_retrieve_flow_handler, []},

               {"/settings/user",  return_empty_json, []},
               {"/nodes",          return_empty_json, []},
               {"/flows",          nodered_flow_deploy_handler, []},
               {"/inject/:nodeid", nodered_inject_node_button_handler, []},

               %%
               %% GET handlers for delivery of the static content
               %%
               %% TODO the constraints here DONT WORK - Cowboy just
               %% TODO ignores them because Bindings is empty.
               %%
               {"/library/local/flows/", [{method,<<"GET">>}], return_empty_json, []},
               {"/credentials/[...]", [{method,<<"GET">>}], return_empty_json, []},
               {"/context/[...]", [{method,<<"GET">>}], return_empty_json, []},

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
    unittest_engine:start(),
    error_store:start(),

    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ).

stop() ->
    application:stop(cowboy).

stop(_State) ->
    ok.
