-module(ered_compute_engine).

-behaviour(gen_server).

-export([
    init/1,
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    stop/0,
    start/0
]).

-export([
    deploy/2,
    reload/1
]).

%%
%% Compute engine for executing flows on this server.
%%
%% Compute engine creates the pids for each node and lets those processes
%% run. The unitest engine runs flows and then brings down the processes
%% after the test.
%%

-import(ered_nodes, [
    create_pid_for_node/2,
    get_node_name/2
]).
-import(ered_msg_handling, [
    decode_json/1
]).

start() ->
    %% trigger the loading of an initial flow file. This can be used in
    %% conjunction with DISABLE_FLOWEDITOR to have just a single flow running
    %% on the ErlangRED compute engine, i.e. a headless deployment.
    case os:getenv("COMPUTEFLOW") of
        false ->
            ignore;
        FlowId ->
            erlang:start_timer(
                750,
                ered_compute_engine,
                {load_flowid, FlowId}
            )
    end,
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%% External API
deploy(JsonStr, WsName) ->
    gen_server:call(?MODULE, {deploy, JsonStr, WsName}).

reload(WsName) ->
    gen_server:call(?MODULE, {reload, WsName}).

%%
%%
init([]) ->
    {ok, #{}}.

%%
%%
handle_call({reload, WsName}, _From, State) ->
    %%
    %% this is a restart so go through all nodes that have a prefix
    %% node_pid_ws<....> in the pg groups and reset their state, ie.
    %% send them an event to reset their state, i.e., the reload event.
    %%
    Prefix = get_node_name(WsName, <<"">>),
    Nodes = lists:filter(
        fun(AA) ->
            case binary:match(AA, Prefix) of
                nomatch ->
                    false;
                _ ->
                    true
            end
        end,
        pg:which_groups()
    ),
    [[M ! {reload} || M <- pg:get_members(GrpName)] || GrpName <- Nodes],
    {reply, <<"{\"rev\":\"dead060d\"}">>, State};
handle_call({deploy, JsonStr, WsName}, _From, State) ->
    %% here we want to do a deploy which means using the prefix
    %% node_pid_ws<....> and either stopping those processes that no
    %% no longer exist or reseting their state if they do. or simply
    %% remove all nodes with the prefix and restart create them all
    Prefix = get_node_name(WsName, <<"">>),
    Nodes = lists:filter(
        fun(AA) ->
            case binary:match(AA, Prefix) of
                nomatch ->
                    false;
                _ ->
                    true
            end
        end,
        pg:which_groups()
    ),
    [[M ! {stop, WsName} || M <- pg:get_members(GrpName)] || GrpName <- Nodes],

    FlowMap = decode_json(JsonStr),
    {ok, NodeAry} = maps:find(flows, FlowMap),
    create_pid_for_node(NodeAry, WsName),

    {reply, <<"{\"rev\":\"fed00d06\"}">>, State};
handle_call(_Msg, _From, State) ->
    {reply, State, State}.

%%
%%
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%
%% This should only be called once at the beginning of the app load
%% it is used in conjunection with a headless deployment to have a Erlang
%% setup running based on a single flow file.
handle_info({timeout, _From, {load_flowid, FlowId}}, State) ->
    io:format("Loading initial flow id ~p~n", [FlowId]),
    case ered_flow_store_server:get_filename(FlowId) of
        error ->
            io:format(
                "WARNING: FlowId '~s' not found, doing nothing.~n",
                [FlowId]
            ),
            ignore;
        FileName ->
            Ary = ered_flows:parse_flow_file(FileName),
            %% Websockets are scopes that identify a client using the
            %% floweditor to execute flows. If there is no floweditor
            %% frontend and a flow is loaded what then? Well it's a none
            %% websocket. Important is that a websocket name is supplied.
            ered_nodes:create_pid_for_node(Ary, none)
    end,
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

%%
%%
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

stop() ->
    gen_server:cast(?MODULE, stop).

terminate(normal, _State) ->
    ok;
terminate(Event, _State) ->
    io:format("Compute Engine Terminated with {{{ ~p }}}~n", [Event]),
    ok.
