-module(error_store).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).

-export([stop/0]).

-export([start/0, get_store/0]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_store() ->
    gen_server:call(?MODULE, {store}).

init([]) ->
    {ok, []}.

handle_call({store}, _From, ErrorStore) ->
    {reply, ErrorStore, ErrorStore};

handle_call(_Msg, _From, ErrorStore) ->
    {reply, ErrorStore, ErrorStore}.

stop() ->
    gen_server:cast(?MODULE, stop).

terminate(normal, _State) ->
    ok.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, Store) ->
    {noreply, Store}.

handle_info({it_happened, Msg}, ErrorStore) ->
    {noreply, [ Msg | ErrorStore ] };

handle_info(stop, ErrorStore) ->
    gen_server:cast(?MODULE, stop),
    {noreply, ErrorStore }.

code_change(_OldVersion, ErrorStore, _Extra) ->
    {ok, ErrorStore}.
