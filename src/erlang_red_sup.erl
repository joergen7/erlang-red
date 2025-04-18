-module(erlang_red_sup).

-behaviour(supervisor).
-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok,
        {
            {one_for_one, 1000, 3600},
            [
                #{
                    id => ered_ch1_ws_event_exchange,
                    start => {websocket_event_exchange, start, []},
                    restart => permanent,
                    type => worker,
                    module => [websocket_event_exchange]
                },
                #{
                    id => ered_ch2_unittest_engine,
                    start => {unittest_engine, start, []},
                    restart => permanent,
                    type => worker,
                    module => [unittest_engine]
                },
                #{
                    id => ered_ch3_error_store,
                    start => {ered_error_store, start, []},
                    restart => permanent,
                    type => worker,
                    module => [ered_error_store]
                },
                #{
                    id => ered_ch4_flow_store,
                    start => {ered_flow_store_server, start, []},
                    restart => permanent,
                    type => worker,
                    module => [ered_flow_store_server]
                },
                #{
                    id => ered_ch5_pg_kernel,
                    start => {pg, start_link, []},
                    restart => permanent,
                    type => supervisor,
                    module => [pg]
                },
                #{
                    id => ered_ch6_red_web,
                    start => {ered_webserver, start, []},
                    restart => permanent,
                    type => worker,
                    module => [ered_webserver]
                }
            ]
        }}.
