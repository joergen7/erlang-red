-module(ered_tcp_connector).

%% external export
-export([
    start/3
]).

%% internal export
-export([
    handler/2,
    retrier/5
]).

-spec start(
    HostName :: binary(),
    PortNum :: non_neg_integer(),
    Router :: pid()
) -> ok.
start(HostName, PortNum, Router) ->
    %% Router is the tcp manager.
    SessionId = ered_tcp_manager:session_id(),
    case tcp_connect(HostName, PortNum) of
        {ok, Sock} ->
            spawn_handler(Sock, SessionId, HostName, PortNum, Router);
        {error, _Error} ->
            Pid = spawn(
                ?MODULE,
                retrier,
                [SessionId, HostName, PortNum, Router, 0]
            ),
            Router ! {new_session, {SessionId, Pid, {retry, HostName, PortNum}}}
    end,
    ok.

handler(S, {SessionId, Router, HostName, PortNum} = Details) ->
    io:format("CONN: now looping on ~p with ~w~n", [self(), S]),
    inet:setopts(S, [{active, once}]),
    receive
        {ered_send_out_data, RData} ->
            io:format("CONN: Sending ~w ==> [~p]~n", [inet:sockname(S), RData]),
            gen_tcp:send(S, RData),
            handler(S, Details);
        {tcp, S, Data} ->
            io:format("CONN: Received ~w ==> [~p]~n", [inet:sockname(S), Data]),
            Router ! {route, {conn, SessionId, HostName, PortNum, Data}},
            handler(S, Details);
        {tcp_passive, S} ->
            handler(S, Details);
        {ered_done_with_socket} ->
            inet:close(S),
            io:format("CONN: Socket closed by server~n", []),
            Router ! {del_session, {conn, SessionId, HostName, PortNum}};
        {tcp_error, S, Reason} ->
            io:format(
                "CONN: Socket error ~w closed [~w] ~p~n",
                [S, self(), Reason]
            ),
            Router ! {del_session, {conn, SessionId, HostName, PortNum}};
        {tcp_closed, S} ->
            io:format("CONN: Socket ~w closed [~w]~n", [S, self()]),
            Router ! {del_session, {conn, SessionId, HostName, PortNum}};
        Msg ->
            io:format("CONN: Received Unknown: ~p~n", [Msg])
    end.

retrier(SessionId, HostName, PortNum, Router, Backoff) ->
    erlang:start_timer(
        rand:uniform(512) + (512 * min(20, Backoff)), self(), retry
    ),
    receive
        {timeout, _Ref, retry} ->
            case tcp_connect(HostName, PortNum) of
                {ok, Sock} ->
                    spawn_handler(
                        Sock,
                        ered_tcp_manager:session_id(),
                        HostName,
                        PortNum,
                        Router
                    ),
                    Router !
                        {del_session, {retry, SessionId, HostName, PortNum}};
                _ ->
                    retrier(SessionId, HostName, PortNum, Router, Backoff + 1)
            end;
        _ ->
            Router ! {del_session, {retry, SessionId, HostName, PortNum}}
    end.

%%
%%
tcp_connect(HostName, PortNum) ->
    gen_tcp:connect(
        binary_to_list(HostName),
        PortNum,
        [{active, false}]
    ).

spawn_handler(Sock, SessionId, HostName, PortNum, Router) ->
    Pid = spawn(?MODULE, handler, [Sock, {SessionId, Router, HostName, PortNum}]),
    gen_tcp:controlling_process(Sock, Pid),
    Router ! {new_session, {SessionId, Pid, {conn, HostName, PortNum}}}.
