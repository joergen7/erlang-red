-module(ered_tcp_manager).

-behaviour(gen_server).

-export([
    init/1,
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    stop/0,
    start_link/0
]).

%%
%% Manager of tcp connections, both outbound and inbound.
%%

-export([
    close/1,
    unregister_listener/2,
    register_listener/3,
    send/2,
    state/0
]).

-spec register_listener(
    HostNameOrIp :: binary(),
    PortNum :: 0..65535,
    PidTcpInNode :: pid() | atom()
) -> ok | {error, hostname_not_allowed}.
register_listener(HostNameOrIp, PortNum, PidOrName) ->
    gen_server:call(
        ?MODULE,
        {register_listener, HostNameOrIp, PortNum, PidOrName}
    ).

-spec unregister_listener(
    PortNum :: 0..65535,
    PidTcpInNode :: pid() | atom()
) -> ok.
unregister_listener(PortNum, PidTcpInNode) ->
    gen_server:call(
        ?MODULE,
        {unregister_listener, PortNum, PidTcpInNode}
    ).

-spec send(
    SessionId :: binary(),
    Data :: binary()
) -> ok | {error, no_process}.
send(SessionId, Data) ->
    gen_server:call(?MODULE, {send, SessionId, Data}).

-spec close(SessionId :: binary()) -> ok.
close(SessionId) ->
    gen_server:call(?MODULE, {close, SessionId}).

-spec state() -> map().
state() ->
    gen_server:call(?MODULE, {state}).

%%
%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% tpcins nodes are TPC in nodes and the sessions are the session ids
    %% with which a response can be sent. socks are is a mapping from
    %% port to listening socket - once all tcpins and sessions for a port
    %% are gone, close the listen socket.
    {ok, #{tcpins => [], sessions => [], socks => #{}}}.

%%
%%
handle_call(
    {close, SessionId},
    _From,
    State
) ->
    case lists:keyfind(SessionId, 1, maps:get(sessions, State)) of
        {SessionId, ProcessId, _Port} ->
            %% the message from the handler will remove session from session
            %% list, no need to do this here.
            ProcessId ! {ered_done_with_socket};
        _ ->
            ignore
    end,
    {reply, ok, State};
handle_call(
    {send, SessionId, Data},
    _From,
    State
) ->
    case lists:keyfind(SessionId, 1, maps:get(sessions, State)) of
        {SessionId, ProcessId, _Port} ->
            ProcessId ! {ered_send_out_data, Data},
            {reply, ok, State};
        _ ->
            {reply, {error, no_process}, State}
    end;
handle_call(
    {unregister_listener, PortNum, PidTcpNode},
    _From,
    State
) ->
    Lst =
        case lists:keyfind(PortNum, 1, maps:get(tcpins, State)) of
            {PortNum, Pids} ->
                NewPids = lists:keydelete(PidTcpNode, 2, Pids),

                case length(NewPids) of
                    0 ->
                        RVal = lists:keydelete(
                            PortNum,
                            1,
                            maps:get(tcpins, State)
                        ),
                        case maps:find(PortNum, maps:get(socks, State)) of
                            {ok, S} ->
                                inet:close(S);
                            _ ->
                                ignore
                        end,
                        RVal;
                    _ ->
                        lists:keyreplace(
                            PortNum,
                            1,
                            maps:get(tcpins, State),
                            {PortNum, NewPids}
                        )
                end;
            _ ->
                maps:get(tcpins, State)
        end,
    {reply, ok, State#{tcpins => Lst}};
handle_call(
    {register_listener, HostNameOrIp, PortNum, PidTcpNode},
    _From,
    State
) ->
    case HostNameOrIp of
        <<"">> ->
            %% this will error out if listener already running, else
            %% it will set one up - we don't care as long as a listener
            %% socket is up after this call.
            Socks =
                case ered_tcp_listener:start(PortNum, self()) of
                    {ok, ListenSock} ->
                        Map = maps:get(socks, State),
                        Map#{PortNum => ListenSock};
                    _ ->
                        maps:get(socks, State)
                end,

            Lst =
                case lists:keyfind(PortNum, 1, maps:get(tcpins, State)) of
                    {PortNum, Pids} ->
                        lists:keyreplace(
                            PortNum,
                            1,
                            maps:get(tcpins, State),
                            {PortNum, [{pid, PidTcpNode} | Pids]}
                        );
                    _ ->
                        [
                            {PortNum, [{pid, PidTcpNode}]}
                            | maps:get(tcpins, State)
                        ]
                end,

            {reply, ok, State#{tcpins => Lst, socks => Socks}};
        _HostName ->
            {replay, {error, hostname_not_allowed}, State}
    end;
handle_call({state}, _From, State) ->
    {reply, State, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%%
%%
handle_cast(stop, State) ->
    [inet:close(S) || {_PortNum, S} <- maps:to_list(maps:get(socks, State))],
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%
%%
handle_info({new_session, {SessionId, Pid, Port, InterfaceIp}}, State) ->
    send_msg_to_pids_for_port(
        Port,
        {tcpl_initiated, {SessionId, InterfaceIp}},
        State
    ),
    {noreply, State#{
        sessions => [
            {SessionId, Pid, Port}
            | maps:get(sessions, State)
        ]
    }};
handle_info({del_session, {SessionId, Port, InterfaceIp}}, State) ->
    send_msg_to_pids_for_port(
        Port,
        {tcpl_closed, {SessionId, InterfaceIp}},
        State
    ),
    {noreply, State#{
        sessions => lists:keydelete(SessionId, 1, maps:get(sessions, State))
    }};
handle_info({route, {Port, Data, SessionId, InterfaceIp}}, State) ->
    send_msg_to_pids_for_port(
        Port,
        {tcpl_data, {Data, SessionId, InterfaceIp}},
        State
    ),
    {noreply, State};
handle_info(stop, State) ->
    gen_server:cast(?MODULE, stop),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

%%
%%
stop() ->
    gen_server:cast(?MODULE, stop).

terminate(normal, _State) ->
    ok;
terminate(Event, _State) ->
    io:format("Tcp Manager Terminated with {{{ ~p }}}~n", [Event]),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%
%%
send_msg_to_pids_for_port(Port, Msg, State) ->
    case lists:keyfind(Port, 1, maps:get(tcpins, State)) of
        false ->
            %% no tcpins, drop message to the floor.
            ok;
        {Port, Pids} ->
            [Pid ! {tcp_comm, Msg} || {pid, Pid} <- Pids]
    end.
