-module(erlang_red).

-behaviour(application).
-export([start/0, start/2, stop/1, stop/0, constraint/2]).

constraint(Arg1, Arg2) ->
    io:format("Constraint: ~p ~p\n", [Arg1, Arg2]),
    {ok, Arg2}.

start() ->
    application:ensure_all_started([cowboy, erlang_red]).

%%
%% Naming convention is that anything with 'nodered' in the name is part
%% of the orignal NodeRED API that link the flow editor with the server.
%% Other calls are extensions to support ErlangRED functionality.
%%
%% erlfmt:ignore formatting supported by my emacs
start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_',
         [
          %%
          %% Sock'em in the eye websockets
          %%
          {"/node-red/comms",
              ered_http_nodered_websocket, #{stats_interval => 30000}},

          %%
          %% POST handlers
          %%

          %% API for storing test cases to disk.
          {"/testcase/:workspaceid/create",
              ered_http_testcase_post_handler, []},

          %%
          %% APIs for the unit testing nodes
          {"/UnitTesting/tests.json",
              ered_http_unittesting_tests_get_handler, []},
          {"/UnitTesting/:flowid/runtest",
              ered_http_unittesting_runtests_get_handler, []},
          {"/UnitTesting/:flowid/retrieve",
              ered_http_unittesting_retrieve_flow_handler, []},
          {"/UnitTesting/halt",
              ered_http_unittesting_halt_handler, []},

          %%
          %% these APIS are required by Node-RED
          {"/settings/user",  ered_http_nodered_empty_json, []},
          {"/nodes",          ered_http_nodered_empty_json, []},
          {"/flows",          ered_http_nodered_flow_deploy_handler, []},
          {"/inject/:nodeid", ered_http_nodered_inject_node_button_handler, []},

          {"/debug/view/debug-utils.js",
              [{method, <<"GET">>}],
              cowboy_static,
              {file, "./node-red-frontend/debug/view/debug-utils.js"}},

          {"/debug/:nodeid/:action",
              [{method, <<"POST">>}],
              ered_http_nodered_debug_node_active,
              []},

          %%
          %% GET handlers for delivery of the static content
          %%

          %%
          %% Flow Compare node allows comparing the flow data in the browser
          %% with what is stored on the server. Good for knowing the in-browser
          %% changes made to installed test cases.
          {"/FlowCompare/jslib/diff.min.js",
              [{method, <<"GET">>}],
              cowboy_static,
              {file, "./priv/vendor/diff.min.js"}},

          {"/FlowCompare/jslib/flowviewer.min.js",
              [{method, <<"GET">>}],
              cowboy_static,
              {file, "./priv/vendor/flowviewer.min.js"}},

          %% TODO the constraints here DONT WORK - Cowboy just
          %% TODO ignores them because Bindings is empty.
          %%
          {"/library/local/flows/",
              [{method, <<"GET">>}], ered_http_nodered_empty_json, []},
          {"/credentials/[...]",
              [{method, <<"GET">>}], ered_http_nodered_empty_json, []},
          {"/context/[...]",
              [{method, <<"GET">>}], ered_http_nodered_empty_json, []},

          {"/node-red", [{method, <<"GET">>}], cowboy_static,
              {file, "./node-red-frontend/index.html"}},

          {"/[...]", [{method, <<"GET">>}], cowboy_static,
              {dir, "./node-red-frontend", [
                  {mimetypes, ered_http_nodered_mimetypes, mt}
              ]}}
         ]}
    ]),

    websocket_event_exchange:start(),
    flow_store_server:start(),
    unittest_engine:start(),
    error_store:start(),

    {ok, _} = cowboy:start_clear(
        erlang_red_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ).

stop() ->
    application:stop(cowboy).

stop(_State) ->
    ok.
