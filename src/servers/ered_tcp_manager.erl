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
%% The logic is this:
%%   1. there are listeners that setup a server connection socket
%%   2. there are connectors that connect to a server and expect data to come in
%%   3. there are multiple connectors connected to the same host/port combi
%%   4. there are multiple listeners listening for connections on same port
%%   5. all data goes to all connectors and listeners <<<---- the triggery bit
%%
%% this is a little bit different to the orignal Node-RED implementation but
%% this is more inclusive, i.e., multiple tcp-in listeners on the same port
%% can co-exist and generate the same data.
%%
%% What this does is to maintain routing tables of:
%%
%%  - session ids to process ids - these are used to shutdown processes on
%%    stop but also to direct data to the correct connections when a tcp out
%%    sends data. All the tcp out node receives is a session id - that is mapped
%%    to the process id and that process is sent the data.
%%
%%  - host/port combinations to node process id - when data comes in, it either
%%    comes from a listener connections (i.e., a listner on a port received a
%%    connection) or a connect-to connection (i.e., a connector connected to a
%%    remove host/port). When the data comes in, it is marked either with 'conn'
%%    or 'list' meaning listener or connect-to. The corresponding node process
%%    ids are looked up and each is sent the data.
%%
%% This has all been tested only with string data, no binary content has been
%% tested.
%%

%% -----------------------------------------------------------------------------
%%
%% External APIs - these are used by the tcp nodes.
%%
%% -----------------------------------------------------------------------------

-export([
    close/1,
    unregister_listener/2,
    unregister_connector/3,
    register_listener/3,
    register_connector/3,
    send/2,
    state/0,
    session_id/0
]).

-spec register_connector(
    HostNameOrIp :: binary(),
    PortNum :: 0..65535,
    PidTcpInNode :: pid() | atom()
) -> connected | connecting.
register_connector(HostNameOrIp, PortNum, PidOrName) ->
    gen_server:call(
        ?MODULE,
        {register_connector, HostNameOrIp, PortNum, PidOrName}
    ).

-spec register_listener(
    HostNameOrIp :: binary(),
    PortNum :: 0..65535,
    PidTcpInNode :: pid() | atom()
) -> ok | {error, hostname_not_allowed} | {error, Reason :: any()}.
register_listener(HostNameOrIp, PortNum, PidOrName) ->
    gen_server:call(
        ?MODULE,
        {register_listener, HostNameOrIp, PortNum, PidOrName}
    ).

-spec unregister_connector(
    HostNameOrIp :: binary(),
    PortNum :: 0..65535,
    PidTcpInNode :: pid() | atom()
) -> ok.
unregister_connector(HostNameOrIp, PortNum, PidOrName) ->
    gen_server:call(
        ?MODULE,
        {unregister_connector, HostNameOrIp, PortNum, PidOrName}
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

%%
%% Send data out on a open connection
-spec send(
    SessionId :: binary(),
    Data :: binary()
) -> ok | {error, no_process}.
send(SessionId, Data) ->
    gen_server:call(?MODULE, {send, SessionId, Data}).

%%
%% close an existing connection
-spec close(SessionId :: binary()) -> ok.
close(SessionId) ->
    gen_server:call(?MODULE, {close, SessionId}).

%%
%% provide a unique session id for all connections
%%
%%
-spec session_id() -> binary().
session_id() ->
    ered_nodes:generate_id().

%%
%% this might be removed.
-spec state() -> map().
state() ->
    gen_server:call(?MODULE, {state}).

%% -----------------------------------------------------------------------------
%%
%% gen_server interface
%%
%% -----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% tpcins nodes are TPC in nodes and the sessions are the session ids
    %% with which a response can be sent. socks are is a mapping from
    %% port to listening socket - once all tcpnodes and sessions for a port
    %% are gone, close the listen socket.
    {ok, #{tcpnodes => [], sessions => [], socks => #{}}}.

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

%% -----------------------------------------------------------------------------
%%
%% Call events are the "backend" for the external APIs.
%%
%% -----------------------------------------------------------------------------
handle_call(
    {close, SessionId},
    _From,
    #{sessions := Sessions} = State
) ->
    case lists:keyfind(SessionId, 1, Sessions) of
        {SessionId, ProcessId, _} ->
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
    #{sessions := Sessions} = State
) ->
    case lists:keyfind(SessionId, 1, Sessions) of
        {SessionId, ProcessId, _} ->
            ProcessId ! {ered_send_out_data, Data},
            {reply, ok, State};
        _ ->
            {reply, {error, lost_connection}, State}
    end;
%%
%%
handle_call(
    {register_connector, HostName, PortNum, PidTcpNode},
    _From,
    #{sessions := Sessions, tcpnodes := CurrTcpNodes} = State
) ->
    TcpNodes =
        add_pid_to_tcpnodes(
            {conn, HostName, PortNum}, PidTcpNode, CurrTcpNodes
        ),

    %% TODO here we should check the PID included with the session
    %% TODO if no longer alive, the ditch it and try again.
    case
        {
            lists:keyfind({conn, HostName, PortNum}, 3, Sessions) =/= false,
            lists:keyfind({retry, HostName, PortNum}, 3, Sessions) =/= false
        }
    of
        {false, false} ->
            ered_tcp_connector:start(HostName, PortNum, self()),
            {reply, connecting, State#{tcpnodes => TcpNodes}};
        {true, _} ->
            {reply, connected, State#{tcpnodes => TcpNodes}};
        {_, true} ->
            {reply, connecting, State#{tcpnodes => TcpNodes}}
    end;
handle_call(
    {register_listener, <<"">>, PortNum, PidTcpNode},
    _From,
    #{socks := ListenerSockets, tcpnodes := CurrTcpNodes} = State
) ->
    TcpNodes = add_pid_to_tcpnodes({list, PortNum}, PidTcpNode, CurrTcpNodes),

    case maps:find(PortNum, ListenerSockets) of
        {ok, _} ->
            {reply, ok, State#{tcpnodes => TcpNodes}};
        _ ->
            case ered_tcp_listener:start(PortNum, self()) of
                {ok, ListenSock} ->
                    {reply, ok, State#{
                        tcpnodes => TcpNodes,
                        socks => ListenerSockets#{PortNum => ListenSock}
                    }};
                {error, Reason} ->
                    {reply, {error, Reason}, State#{tcpnodes => TcpNodes}}
            end
    end;
handle_call(
    {register_listener, _HostNameOrIp, _PortNum, _PidTcpNode},
    _From,
    State
) ->
    {replay, {error, hostname_not_allowed}, State};
%%
handle_call(
    {unregister_listener, PortNum, PidTcpNode},
    _From,
    State
) ->
    TcpNodes = remove_pid_from_tcpnodes(
        {list, PortNum}, PidTcpNode, maps:get(tcpnodes, State)
    ),

    {reply, ok, State#{tcpnodes => TcpNodes}};
%%
handle_call(
    {unregister_connector, HostName, PortNum, PidTcpNode},
    _From,
    State
) ->
    TcpNodes = remove_pid_from_tcpnodes(
        {conn, HostName, PortNum}, PidTcpNode, maps:get(tcpnodes, State)
    ),

    {reply, ok, State#{tcpnodes => TcpNodes}};
%%
handle_call({state}, _From, State) ->
    {reply, State, State};
%%
%% Catch all.
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

%% -----------------------------------------------------------------------------
%%
%% Cast messages only cast the first stop.
%%
%% -----------------------------------------------------------------------------
handle_cast(stop, #{socks := Socks, sessions := Sessions} = State) ->
    [Pid ! {ered_done_with_socket} || {_, Pid, _} <- Sessions],
    [inet:close(S) || {_, S} <- maps:to_list(Socks)],
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% -----------------------------------------------------------------------------
%%
%% Handle for managing internal communication between the tcp_connector and
%% this manager and the tcp_listener and this manager.
%%
%% Basic lifecycle is a new session - this represents a new connection, either
%% to a host or by a host to our listener. This is true except for retry
%% sessions that are actually sessions that continually try to connect to a
%% remote host.
%%
%% Because each connection/session can have multiple clients (i.e. tcp nodes),
%% the route event sends data to all those clients. I.e., a session represents
%% a single connection to something. A client is a tcp node, either tcp in or
%% tcp out or tcp request. These nodes have connection details. If these happen
%% to overlap, then they receive the same data. These clients then generate
%% message with different message ids however the same session id.
%%
%% Caveat Emptor - let the buyer beware.
%%
%% This behaviour is the same as the classical Node-RED but then again
%% blindly following the rules leads to where everyone else is. There is
%% no standardised behaviour here and I'm willing to try something else.
%%
%% -----------------------------------------------------------------------------
%% create session
handle_info(
    {new_session, {SessionId, Pid, {retry, HostName, PortNum}}},
    #{sessions := Sessions} = State
) ->
    %% this is a special kind of session, it's a retry session, It's
    %% connecting to end point and while it's try, it comes in the
    %% list of sessions so that it can also be shutdown if this server
    %% shuts down.
    send_msg_to_nodes(
        {HostName, PortNum},
        {tcpc_retry, {SessionId, HostName, PortNum}},
        State
    ),

    {noreply, State#{
        sessions => [{SessionId, Pid, {retry, HostName, PortNum}} | Sessions]
    }};
handle_info(
    {new_session, {SessionId, Pid, {conn, HostName, PortNum}}},
    #{sessions := Sessions} = State
) ->
    send_msg_to_nodes(
        {HostName, PortNum},
        {tcpc_initiated, {SessionId, HostName, PortNum}},
        State
    ),

    {noreply, State#{
        sessions => [{SessionId, Pid, {conn, HostName, PortNum}} | Sessions]
    }};
handle_info(
    {new_session, {SessionId, Pid, {list, Port, InterfaceIp}}},
    #{sessions := Sessions} = State
) ->
    send_msg_to_nodes(
        Port,
        {tcpl_initiated, {SessionId, InterfaceIp}},
        State
    ),

    {noreply, State#{sessions => [{SessionId, Pid, {list, Port}} | Sessions]}};
%%
%% delete session
handle_info(
    {del_session, {retry, SessionId, _HostName, _PortNum}},
    #{sessions := Sessions} = State
) ->
    {noreply, State#{sessions => lists:keydelete(SessionId, 1, Sessions)}};
handle_info(
    {del_session, {conn, SessionId, HostName, PortNum}},
    #{sessions := Sessions} = State
) ->
    send_msg_to_nodes(
        {HostName, PortNum},
        {tcpc_closed, {SessionId, HostName, PortNum}},
        State
    ),
    {noreply, State#{sessions => lists:keydelete(SessionId, 1, Sessions)}};
handle_info(
    {del_session, {list, SessionId, Port, InterfaceIp}},
    #{sessions := Sessions} = State
) ->
    send_msg_to_nodes(
        Port,
        {tcpl_closed, {SessionId, InterfaceIp}},
        State
    ),
    {noreply, State#{sessions => lists:keydelete(SessionId, 1, Sessions)}};
%%
%%
handle_info({route, {conn, SessionId, HostName, PortNum, Data}}, State) ->
    send_msg_to_nodes(
        {HostName, PortNum},
        {tcpc_data, {Data, SessionId, HostName, PortNum}},
        State
    ),
    {noreply, State};
handle_info({route, {list, SessionId, Port, InterfaceIp, Data}}, State) ->
    send_msg_to_nodes(
        Port,
        {tcpl_data, {Data, SessionId, InterfaceIp}},
        State
    ),
    {noreply, State};
%%
%%
handle_info(stop, State) ->
    gen_server:cast(?MODULE, stop),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

%% -----------------------------------------------------------------------------
%%
%% Helpers
%%
%% -----------------------------------------------------------------------------
send_msg_to_nodes({HostName, Port}, Msg, State) ->
    send_msg_pids({conn, HostName, Port}, Msg, maps:get(tcpnodes, State));
send_msg_to_nodes(Port, Msg, State) ->
    send_msg_pids({list, Port}, Msg, maps:get(tcpnodes, State)).

send_msg_pids(Key, Msg, Tcpnodes) ->
    case lists:keyfind(Key, 1, Tcpnodes) of
        {_, Pids} ->
            [Pid ! {tcp_comm, Msg} || {pid, Pid} <- Pids];
        %% no tcpnodes, drop message to the floor.
        false ->
            ok
    end.

add_pid_to_tcpnodes(Key, Pid, Tcpnodes) ->
    case lists:keyfind(Key, 1, Tcpnodes) of
        {Key, Pids} ->
            lists:keyreplace(Key, 1, Tcpnodes, {Key, [{pid, Pid} | Pids]});
        _ ->
            [{Key, [{pid, Pid}]} | Tcpnodes]
    end.

remove_pid_from_tcpnodes(Key, Pid, Tcpnodes) ->
    case lists:keyfind(Key, 1, Tcpnodes) of
        {Key, Pids} ->
            NewPids = lists:keydelete(Pid, 2, Pids),

            %% have we removed the last pid for the socket?
            case length(NewPids) of
                0 ->
                    lists:keydelete(Key, 1, Tcpnodes);
                _ ->
                    lists:keyreplace(Key, 1, Tcpnodes, {Key, NewPids})
            end;
        _ ->
            Tcpnodes
    end.
