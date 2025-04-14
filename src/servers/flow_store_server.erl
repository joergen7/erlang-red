-module(flow_store_server).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).
-export([stop/0]).
-export([start/0]).

-export([get_flow_data/0]).
-export([update_all_flows/0]).
-export([update_flow/2]).
-export([get_filename/1]).
-export([all_flow_ids/0]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    update_all_flows().

get_flow_data() ->
    gen_server:call(?MODULE, {get_store}).

update_all_flows() ->
    gen_server:call(?MODULE, {update_all}).

update_flow(FlowId, Filename) ->
    gen_server:call(?MODULE, {update_one, FlowId, Filename}).

get_filename(FlowId) ->
    gen_server:call(?MODULE, {filename, FlowId}).

all_flow_ids() ->
    gen_server:call(?MODULE, {all_flow_ids}).

init([]) ->
    {ok, #{}}.

%%
%% Specific implementation for the flow store
%%
handle_call({all_flow_ids}, _From, FlowStore) ->
    AllFlowIds = maps:keys(FlowStore),
    {reply, AllFlowIds, FlowStore};
handle_call({get_store}, _From, FlowStore) ->
    {reply, FlowStore, FlowStore};
handle_call({update_all}, _From, _FlowStore) ->
    {reply, true, compile_file_store(compile_file_list(), #{})};
handle_call({update_one, FlowId, Filename}, _From, FlowStore) ->
    FlowDetails = compile_file_store([{FlowId, Filename}], #{}),
    {reply, true, maps:merge(FlowStore, FlowDetails)};
handle_call({filename, FlowId}, _From, FlowStore) ->
    case maps:find(FlowId, FlowStore) of
        {ok, Val} ->
            case maps:find(path, Val) of
                {ok, Path} ->
                    {reply, Path, FlowStore};
                _ ->
                    {reply, error, FlowStore}
            end;
        _ ->
            {reply, error, FlowStore}
    end;
%%
%%

handle_call(_Msg, _From, FlowStore) ->
    {reply, FlowStore, FlowStore}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, Store) ->
    {noreply, Store}.

handle_info(stop, ErrorStore) ->
    gen_server:cast(?MODULE, stop),
    {noreply, ErrorStore}.

code_change(_OldVersion, ErrorStore, _Extra) ->
    {ok, ErrorStore}.

stop() ->
    gen_server:cast(?MODULE, stop).

terminate(normal, _State) ->
    ok.

%%
%%
tab_name_or_filename([], FileName) ->
    FileName;
tab_name_or_filename([NodeDef | MoreNodeDefs], FileName) ->
    case maps:find(type, NodeDef) of
        {ok, <<"tab">>} ->
            {ok, Val} = maps:find(label, NodeDef),
            Val;
        _ ->
            tab_name_or_filename(MoreNodeDefs, FileName)
    end.

compile_file_list() ->
    {ok, MP} = re:compile("flow.([A-Z0-9]{16}).json", [caseless]),

    FileNames = filelib:fold_files(
        "priv/testflows",
        "",
        false,
        fun(Fname, Acc) ->
            case re:run(Fname, MP) of
                {match, [{_, _}, {S, L}]} ->
                    [{string:substr(Fname, S + 1, L), Fname} | Acc];
                _ ->
                    Acc
            end
        end,
        []
    ),
    FileNames.

compile_file_store([], FileStore) ->
    FileStore;
compile_file_store([FileDetails | MoreFileNames], FileStore) ->
    FlowId = element(1, FileDetails),
    FileName = element(2, FileDetails),

    Ary = ered_flows:parse_flow_file(FileName),
    TestName = tab_name_or_filename(Ary, FlowId),

    compile_file_store(
        MoreFileNames,
        maps:put(
            nodes:jstr(FlowId),
            #{
                path => nodes:jstr(FileName),
                id => nodes:jstr(FlowId),
                name => TestName
            },
            FileStore
        )
    ).
