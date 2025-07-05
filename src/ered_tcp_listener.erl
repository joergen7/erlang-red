-module(ered_tcp_listener).

-export([
    server/3,
    start/2
]).

%%
%% This is an extremely simple TCP port listener that creates process
%% for each connection made. Each listener has a process id to which data
%% packets are sent as they are received.
%%
start(LPort, Router) ->
    case gen_tcp:listen(LPort, [{active, false}, {packet, 0}]) of
        {ok, ListenSock} ->
            spawn(?MODULE, server, [ListenSock, LPort, Router]),
            {ok, ListenSock};
        {error, Reason} ->
            io:format(
                "Error Starting TCP Listener on ~p: [~p]~n",
                [LPort, Reason]
            ),
            {error, Reason}
    end.

server(ListenSocket, Port, Router) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, S} ->
            io:format("connect accepted ~w ~n", [S]),
            %% spin up a new server to listen for more connections, this
            %% one has just received a connection and will handle that
            %% connection
            spawn(?MODULE, server, [ListenSocket, Port, Router]),

            %% Session Id is a Node-RED thing: each connection is uniquely
            %% identified by a "session id" even though it is actually a
            %% connection between peer and server
            SessionId = ered_tcp_manager:session_id(),
            {ok, {InterfaceIp, Port}} = inet:sockname(S),
            IpStr = list_to_binary(inet:ntoa(InterfaceIp)),
            Router ! {new_session, {SessionId, self(), {list, Port, IpStr}}},
            handler(S, {Port, Router, SessionId, IpStr});
        Other ->
            io:format("accept returned ~w - goodbye!~n", [Other]),
            ok
    end.

handler(S, {Port, Router, SessionId, InterfaceIp} = Details) ->
    io:format("now looping on ~p with ~w~n", [self(), S]),
    inet:setopts(S, [{active, once}]),
    receive
        {ered_send_out_data, RData} ->
            io:format("Sending ~w ==> [~p]~n", [inet:sockname(S), RData]),
            gen_tcp:send(S, RData),
            handler(S, Details);
        {tcp, S, Data} ->
            io:format("Received ~w ==> [~p]~n", [inet:sockname(S), Data]),
            Router ! {route, {list, SessionId, Port, InterfaceIp, Data}},
            handler(S, Details);
        {tcp_passive, S} ->
            handler(S, Details);
        {ered_done_with_socket} ->
            inet:close(S),
            io:format("Socket closed by server~n", []),
            Router ! {del_session, {list, SessionId, Port, InterfaceIp}};
        {tcp_error, S, Reason} ->
            io:format("Socket error ~w closed [~w] ~p~n", [S, self(), Reason]),
            Router ! {del_session, {list, SessionId, Port, InterfaceIp}};
        {tcp_closed, S} ->
            io:format("Socket ~w closed [~w]~n", [S, self()]),
            Router ! {del_session, {list, SessionId, Port, InterfaceIp}}
    end.
