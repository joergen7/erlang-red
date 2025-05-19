-module(ered_csv_parser_store).

-behaviour(gen_server).

-export([
    start/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3,
    stop/0
]).

%%
%% CSV parser store maintains a collection of CSV parsers created by the
%% CSV node. These parser are modules differentiated by their definition
%% of what is a separator and what is an escape.
%%

-export([get_parser_for/2]).

-export([parse_string/3]).
-export([parse_string/4]).

-export([get_store/0]).

-import(ered_nodes, [
    generate_id/0
]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%%
get_parser_for(Separator, Escape) ->
    gen_server:call(?MODULE, {get_parser_for, Separator, Escape}).

parse_string(Separator, Escape, Str) ->
    erlang:apply(
        get_parser_for(Separator, Escape),
        parse_string,
        [to_binary(Str)]
    ).

parse_string(Separator, Escape, Str, ParseOpts) ->
    erlang:apply(
        get_parser_for(Separator, Escape),
        parse_string,
        [to_binary(Str), ParseOpts]
    ).

get_store() ->
    gen_server:call(?MODULE, {get_store}).

%%
%%
init([]) ->
    {module, P1, _, ok} =
        'Elixir.ErlangRedHelpers':define_csv_parser(
            csv_parser_01, [{separator, <<",">>}, {escape, <<"\"">>}]
        ),

    {module, P3, _, ok} =
        'Elixir.ErlangRedHelpers':define_csv_parser(
            csv_parser_03, [{separator, <<",">>}, {escape, <<"'">>}]
        ),

    Store = [
        {<<",">>, <<"\"">>, P1},
        {<<",">>, <<"'">>, P3}
    ],
    {ok, Store}.

%%
%%
handle_call({get_store}, _From, ParserStore) ->
    {reply, ParserStore, ParserStore};
handle_call({get_parser_for, Separator, Escape}, _From, ParserStore) ->
    case
        lists:keyfind(
            Escape,
            2,
            lists:filter(
                fun({V, _, _}) -> V =:= Separator end,
                ParserStore
            )
        )
    of
        false ->
            ModuleName =
                list_to_atom(
                    lists:flatten(
                        io_lib:format(
                            "csv_parser_~s",
                            [binary_to_list(generate_id())]
                        )
                    )
                ),
            ParserOpts = add_escape([{separator, Separator}], Escape),
            {module, Parser, _, ok} =
                'Elixir.ErlangRedHelpers':define_csv_parser(
                    ModuleName, ParserOpts
                ),
            {reply, Parser, [{Separator, Escape, Parser} | ParserStore]};
        {_, _, Parser} ->
            {reply, Parser, ParserStore}
    end;
handle_call(_Msg, _From, ParserStore) ->
    {reply, ok, ParserStore}.

%%
%%
stop() ->
    gen_server:cast(?MODULE, stop).

%%
%%
terminate(normal, _State) ->
    ok;
terminate(Event, _State) ->
    io:format("CSV Parser Store Terminated with {{{ ~p }}}~n", [Event]),
    ok.

%%
%%
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, Store) ->
    {noreply, Store}.

%%
%%
handle_info(stop, ParserStore) ->
    gen_server:cast(?MODULE, stop),
    {noreply, ParserStore};
handle_info(_Msg, ParserStore) ->
    {noreply, ParserStore}.

%%
%%
code_change(_OldVersion, ParserStore, _Extra) ->
    {ok, ParserStore}.

%%
%%
to_binary(V) when is_binary(V) ->
    V;
to_binary(V) when is_list(V) ->
    list_to_binary(V);
to_binary(V) when is_atom(V) ->
    atom_to_binary(V).

%%
%%
%% Having an empty escape leads to an error in the underlying nimble library
add_escape(Opts, <<>>) ->
    Opts;
add_escape(Opts, Escape) ->
    [{escape, Escape} | Opts].
