-module(erlang_red_sup).

-behaviour(supervisor).
-export([init/1, start_link/0]).

start_link() ->
    %% code required by the Elixir helpers used in the Erlang code.
    %% Taken from :
    %%  -- https://elixirforum.com/t/calling-elixir-from-erlang/10940 -->
    %%  -- https://joearms.github.io/published/2017-12-18-Calling-Elixir-From-Erlang.html
    %%
    io:format("===> Elixir Code Paths~n", []),
    code:add_path("/usr/local/lib/elixir/lib/elixir/ebin"),
    code:add_path("elixir/erlang_red_helpers/_build/dev/lib/earmark/ebin"),
    code:add_path(
        "elixir/erlang_red_helpers/_build/dev/lib/erlang_red_helpers/ebin"
    ),

    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
        {
            {one_for_one, 1000, 3600},
            [
                #{
                    id => ered_ch1_pg_kernel,
                    start => {pg, start_link, []},
                    restart => permanent,
                    type => supervisor,
                    module => [pg]
                },
                #{
                    id => ered_ch2_flow_store,
                    start => {ered_flow_store_server, start, []},
                    restart => permanent,
                    type => worker,
                    module => [ered_flow_store_server]
                },
                #{
                    id => ered_ch3_ws_event_exchange,
                    start => {ered_ws_event_exchange, start, []},
                    restart => permanent,
                    type => worker,
                    module => [ered_ws_event_exchange]
                },
                #{
                    id => ered_ch4_error_store,
                    start => {ered_error_store, start, []},
                    restart => permanent,
                    type => worker,
                    module => [ered_error_store]
                },
                #{
                    id => ered_ch5_unittest_engine,
                    start => {ered_unittest_engine, start, []},
                    restart => permanent,
                    type => worker,
                    module => [ered_unittest_engine]
                },
                #{
                    id => ered_ch6_config_store,
                    start => {ered_config_store, start, []},
                    restart => permanent,
                    type => worker,
                    module => [ered_config_store]
                },
                #{
                    id => ered_ch7_compute_engine,
                    start => {ered_compute_engine, start, []},
                    restart => permanent,
                    type => worker,
                    module => [ered_compute_engine]
                },
                #{
                    id => ered_ch8_red_web,
                    start => {ered_webserver, start, []},
                    restart => permanent,
                    type => worker,
                    module => [ered_webserver]
                }
            ]
        }}.
