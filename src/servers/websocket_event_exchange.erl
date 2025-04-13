-module(websocket_event_exchange).

-behaviour(gen_server).

%%
%% This service distributes websocket events to listeners. These listners are
%% unit test assertors that ensure that specific things happen that specific
%% nodes promised to do. This is not a store of events, rather a bus that
%% passes events on to listeners - these listeners check the events and
%% potentially cause unit tests to fail.
%%
%% Data is scoped by websocket name, with each event store in a large
%% array which contains tuples indexed by NodeId - FlowId is not necessary
%% since NodeId are unique across all flows in a NodeRED instance, i.e. it
%% is possible to duplicate NodeIds across flows but once they are installed
%% (imported) into an NodeRED instance, the duplicated is flagged and node ids
%% are replaced with new Ids.
%%
-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).
-export([stop/0, start/0]).


%% subscribe to specific events
-export([subscribe/4]).
-export([subscribe/5]).

%% generate events as websocket calls are made
-export([node_status/5]).
-export([debug_msg/3]).

%% unscribe the listeners
-export([unsubscribe/2]).
-export([remove_ws/1]).


%%
%%%%%
%%%
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

debug_msg({ok,WsName},Type,Data) ->
    {ok, NodeId} = maps:find(id, Data),
    gen_server:call(?MODULE, {debug_event, WsName, NodeId, Type, Data});
debug_msg(_,_,_) ->
    ignore.

node_status({ok,WsName},NodeId,Txt,Clr,Shp) ->
    gen_server:call(?MODULE, {status_event, WsName, NodeId, Txt, Clr, Shp});
node_status(_,_,_,_,_) ->
    ignore.

%%
%%
subscribe(WsName,NodeId,debug,Type,Cb) ->
    gen_server:call(?MODULE, {subscribe_to_debug, WsName, NodeId, binary_to_atom(Type), Cb});

subscribe(_,_,_,_,_) ->
    ignore.

subscribe(WsName,NodeId,status,Cb) ->
    gen_server:call(?MODULE, {subscribe_to_status, WsName, NodeId, Cb});

subscribe(_,_,_,_) ->
    ignore.

unsubscribe(_WsName,_NodePid) ->
    %%
    %% TODO implement this - remove the node from all lists
    %%
    ok.

remove_ws(WsName) ->
    gen_server:call(?MODULE, {remove_ws, WsName}).

%%
%%
init([]) ->
    {ok, #{ status => #{}, debug => #{}}}.

%%
handle_call({debug_event, WsName, NodeId, Type, Data}, _From, SubscriberStore) ->
    {ok, DebugStore} = maps:find(debug,SubscriberStore),
    case maps:find(WsName,DebugStore) of
        {ok, Map} ->
            case maps:find(NodeId,Map) of
                {ok, CbAry} ->
                    List = lists:filter( fun ({A,_}) -> A =:= Type end, CbAry ),
                    [Cb ! {ws_event,{debug,WsName,NodeId,Type,Data}} || {_,Cb} <- List];
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end,

    {reply, ok, SubscriberStore};


handle_call({status_event, WsName, NodeId, Txt, Clr, Shp}, _From, SubscriberStore) ->
    {ok, StatusStore} = maps:find(status,SubscriberStore),
    case maps:find(WsName,StatusStore) of
        {ok, Map} ->
            case maps:find(NodeId,Map) of
                {ok, CbAry} ->
                    [Cb ! {ws_event,{status,WsName,NodeId,Txt,Clr,Shp}} || Cb <- CbAry];
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end,

    {reply, ok, SubscriberStore};

handle_call({remove_ws, WsName}, _From, SubscriberStore) ->
    {ok, StatusStore} = maps:find(status,SubscriberStore),
    {ok, DebugStore} = maps:find(debug,SubscriberStore),

    {reply, ok, #{ status => maps:remove(WsName,StatusStore),
                   debug  => maps:remove(WsName,DebugStore) }};

handle_call({subscribe_to_status, WsName, NodeId, Cb}, _From, SubscriberStore) ->
    {ok, StatusStore} = maps:find(status,SubscriberStore),

    case maps:find(WsName,StatusStore) of
        {ok, Map} ->
            case maps:find(NodeId,Map) of
                {ok, Ary} ->
                    %%
                    %% Avoid duplication in the callbacks
                    case lists:member(Cb,Ary) of
                        false ->
                            SStore2 = maps:put(WsName, maps:put(NodeId,
                                                                [Cb|Ary],Map),
                                               StatusStore);
                        _ ->
                            SStore2 = maps:put(WsName, maps:put(NodeId,Ary,Map),
                                               StatusStore)
                    end;
                _ ->
                    SStore2 = maps:put(WsName, maps:put(NodeId,[Cb],Map),
                                       StatusStore)
            end;
        _ ->
            SStore2 = maps:put(WsName,maps:put(NodeId,[Cb],#{}),StatusStore)
    end,

    {reply, ok, maps:put(status, SStore2, SubscriberStore)};


handle_call({subscribe_to_debug, WsName, NodeId, Type, Cb}, _From, SubscriberStore) ->
    {ok, DebugStore} = maps:find(debug,SubscriberStore),

    case maps:find(WsName,DebugStore) of
        {ok, Map} ->
            case maps:find(NodeId,Map) of
                {ok, Ary} ->
                    %%
                    %% Avoid duplication
                    case lists:keyfind(Cb,2,Ary) of
                        false ->
                            DStore2 = maps:put(WsName, maps:put(NodeId,[{Type,Cb}|Ary],
                                                                Map),
                                               DebugStore);
                        _ ->
                            DStore2 = maps:put(WsName, maps:put(NodeId,Ary,Map),
                                               DebugStore)
                    end;
                _ ->
                    DStore2 = maps:put(WsName, maps:put(NodeId,[{Type,Cb}],Map),
                                       DebugStore)
            end;
        _ ->
            DStore2 = maps:put(WsName,maps:put(NodeId, [{Type,Cb}], #{}),
                               DebugStore)
    end,

    {reply, ok, maps:put(debug, DStore2, SubscriberStore)};



handle_call(_Msg, _From, SubscriberStore) ->
    {reply, ok, SubscriberStore}.

%%
handle_cast(_Msg, Store) ->
    {noreply, Store}.

%%
handle_info(stop, SubscriberStore) ->
    gen_server:cast(?MODULE, stop),
    {noreply, SubscriberStore}.


code_change(_OldVersion, SubscriberStore, _Extra) ->
    {ok, SubscriberStore}.


stop() ->
    gen_server:cast(?MODULE, stop).

terminate(normal, _State) ->
    ok.
