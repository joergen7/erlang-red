-module(unittest_engine).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).
-export([stop/0]).
-export([start/0]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{}}.


handle_call(_Msg, _From, FlowStore) ->
    {reply, FlowStore, FlowStore}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, Store) ->
    {noreply, Store}.

handle_info(stop, ErrorStore) ->
    gen_server:cast(?MODULE, stop),
    {noreply, ErrorStore };

handle_info(_Msg, ErrorStore) ->
    {noreply, ErrorStore }.

code_change(_OldVersion, ErrorStore, _Extra) ->
    {ok, ErrorStore}.

stop() ->
    gen_server:cast(?MODULE, stop).

terminate(normal, _State) ->
    ok.
