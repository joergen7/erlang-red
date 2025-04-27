-module(ered_webserver).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).
-export([stop/0]).
-export([start/0]).

-export([register_http_in/3]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    herd_up_the_cattle().

init([]) ->
    {ok, #{routes => []}}.

%%
%%

register_http_in(Path, Method, Pid) ->
    gen_server:call(?MODULE, {add_route, Path, Method, Pid}).

%%
%%
handle_call({add_route, Path, Method, Pid}, _From, State) ->
    %% TODO here check for duplication and ensure that its replaced with the
    %% TODO current handler. I.e. Path + Method should be unique and map
    %% TODO to exactly one pid.
    ExtraRoutes = [
        {Path, [{method, Method}], ered_http_node_http_in_handler, #{
            pid => Pid
        }}
        | maps:get(routes, State)
    ],
    Dispatch = cowboy_router:compile([{'_', ExtraRoutes ++ base_routes()}]),
    cowboy:set_env(erlang_red_listener, dispatch, Dispatch),
    io:format("regesting http in ~p ~p ~p~n", [Path, Method, Pid]),
    {reply, ok, maps:put(routes, ExtraRoutes, State)};
handle_call(_Msg, _From, State) ->
    {reply, State, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, Store) ->
    {noreply, Store}.

handle_info(_Msg, ErrorStore) ->
    {noreply, ErrorStore}.

code_change(_OldVersion, ErrorStore, _Extra) ->
    {ok, ErrorStore}.

stop() ->
    gen_server:cast(?MODULE, stop).

terminate(normal, _State) ->
    ok.

%%
%%

herd_up_the_cattle() ->
    Dispatch = cowboy_router:compile([{'_', base_routes()}]),
    {ok, _} = cowboy:start_clear(
        erlang_red_listener,
        [{port, 8080}],
        #{
            env => #{dispatch => Dispatch},
            middlewares => [ered_http_auth, cowboy_router, cowboy_handler]
        }
    ).

%%
%% Naming convention is that anything with 'nodered' in the name is part
%% of the orignal NodeRED API that link the flow editor with the server.
%% Other calls are extensions to support ErlangRED functionality.
%%

base_routes() ->
    [
        %%
        %% Sock'em in the eye websockets
        %%
        {"/node-red/comms", ered_http_nodered_websocket, #{
            stats_interval => 30000
        }},

        %%
        %% POST handlers
        %%

        %% API for storing test cases to disk.
        {"/testcase/:workspaceid/create", ered_http_testcase_post_handler, []},

        %%
        %% APIs for the unit testing nodes
        {"/UnitTesting/tests.json", ered_http_unittesting_tests_get_handler,
            []},
        {"/UnitTesting/:flowid/runtest",
            ered_http_unittesting_runtests_get_handler, []},
        {"/UnitTesting/:flowid/retrieve",
            ered_http_unittesting_retrieve_flow_handler, []},
        {"/flows.test.json", ered_http_unittesting_retrieve_flow_handler, []},
        {"/UnitTesting/halt", ered_http_unittesting_halt_handler, []},

        %%
        %% these APIS are required by Node-RED
        {"/settings/user", ered_http_nodered_empty_json, []},
        {"/nodes", ered_http_nodered_empty_json, []},
        {"/flows", ered_http_nodered_flow_deploy_handler, []},
        {"/inject/:nodeid", ered_http_nodered_inject_node_button_handler, []},

        {"/debug/view/debug-utils.js", [{method, <<"GET">>}], cowboy_static,
            {priv_file, erlang_red,
                "node-red-frontend/debug/view/debug-utils.js"}},

        {"/debug/:nodeid/:action", [{method, <<"POST">>}],
            ered_http_nodered_debug_node_active, []},

        %%
        %% GET handlers for delivery of the static content
        %%

        %%
        %% Flow Compare node allows comparing the flow data in the browser
        %% with what is stored on the server. Good for knowing the in-browser
        %% changes made to installed test cases.
        {"/FlowCompare/jslib/diff.min.js", [{method, <<"GET">>}], cowboy_static,
            {priv_file, erlang_red, "vendor/diff.min.js"}},

        {"/FlowCompare/jslib/flowviewer.min.js", [{method, <<"GET">>}],
            cowboy_static, {priv_file, erlang_red, "vendor/flowviewer.min.js"}},

        %% TODO the constraints here DONT WORK - Cowboy just
        %% TODO ignores them because Bindings is empty.
        %%
        {"/library/local/flows/", [{method, <<"GET">>}],
            ered_http_nodered_empty_json, []},
        {"/credentials/[...]", [{method, <<"GET">>}],
            ered_http_nodered_empty_json, []},
        {"/context/[...]", [{method, <<"GET">>}], ered_http_nodered_empty_json,
            []},

        {"/node-red", [{method, <<"GET">>}], cowboy_static,
            {priv_file, erlang_red, "node-red-frontend/index.html"}},

        %%% Wrapper site nonsense starts
        {"/", [{method, <<"GET">>}], ered_http_release_status, []},

        {"/media/[...]", [{method, <<"GET">>}], cowboy_static,
            {priv_dir, erlang_red, "wrapper_site/media", []}},

        {"/styles/[...]", [{method, <<"GET">>}], cowboy_static,
            {priv_dir, erlang_red, "wrapper_site/styles", []}},
        %%% Wrapper site nonsense ends

        {"/[...]", [{method, <<"GET">>}], cowboy_static,
            {priv_dir, erlang_red, "node-red-frontend", [
                {mimetypes, ered_http_nodered_mimetypes, mt}
            ]}}
    ].
