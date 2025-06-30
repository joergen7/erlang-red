-module(ered_error_store).
-behaviour(gen_server).

%%
%% When doing testing, this server becomes the store of errors that happened
%% while the flow was executed. Errors are stored by flow id.
%%
-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).

-export([stop/0, start_link/0]).

-export([get_store/0]).
-export([reset_errors/1]).
-export([get_errors/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%%
get_store() ->
    gen_server:call(?MODULE, {store}).

reset_errors(FlowId) ->
    gen_server:call(?MODULE, {clear_errors, FlowId}).

get_errors(FlowId) ->
    gen_server:call(?MODULE, {get_errors, FlowId}).

%%
%%
init([]) ->
    {ok, #{}}.

%%
%% specific implementaions
handle_call({store}, _From, ErrorStore) ->
    {reply, ErrorStore, ErrorStore};
handle_call({clear_errors, FlowId}, _From, ErrorStore) ->
    NewStore = maps:put(FlowId, [], ErrorStore),
    {reply, NewStore, NewStore};
handle_call({get_errors, FlowId}, _From, ErrorStore) ->
    case maps:find(FlowId, ErrorStore) of
        {ok, V} ->
            {reply, V, ErrorStore};
        _ ->
            {reply, [], ErrorStore}
    end;
%%
handle_call(_Msg, _From, ErrorStore) ->
    {reply, ErrorStore, ErrorStore}.

stop() ->
    gen_server:cast(?MODULE, stop).

%%
%%
terminate(normal, _State) ->
    ok;
terminate(Event, _State) ->
    io:format("Error Store Terminated with {{{ ~p }}}~n", [Event]),
    ok.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, Store) ->
    {noreply, Store}.

%%
handle_info({store_msg, {NodeId, TabId, Msg}}, ErrorStore) ->
    case maps:find(TabId, ErrorStore) of
        {ok, Val} ->
            Val2 = [{NodeId, Msg} | Val],
            {noreply, maps:put(TabId, Val2, ErrorStore)};
        _ ->
            {noreply, maps:put(TabId, [{NodeId, Msg}], ErrorStore)}
    end;
handle_info(stop, ErrorStore) ->
    gen_server:cast(?MODULE, stop),
    {noreply, ErrorStore}.

code_change(_OldVersion, ErrorStore, _Extra) ->
    {ok, ErrorStore}.
