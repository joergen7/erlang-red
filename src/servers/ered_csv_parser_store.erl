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

-export([parse_string/3]).
-export([parse_string/4]).
-export([encode_something/4]).

%% -export([get_parser_for/2]).
-export([get_store/0]).

-import(ered_nodes, [
    generate_id/0
]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%
%%
-spec parse_string(
    Separator :: binary(),
    Escape :: binary(),
    Str :: binary()
) -> [map()].
parse_string(Separator, Escape, Str) ->
    erlang:apply(
        get_parser_for(Separator, Escape),
        parse_string,
        [to_binary(Str)]
    ).

-spec parse_string(
    Separator :: binary(),
    Escape :: binary(),
    Str :: binary(),
    ParseOpts :: [tuple()]
) -> [map()].
parse_string(Separator, Escape, Str, ParseOpts) ->
    erlang:apply(
        get_parser_for(Separator, Escape),
        parse_string,
        [to_binary(Str), ParseOpts]
    ).

-spec encode_something(
    Payload :: [map()],
    Separator :: binary(),
    Escape :: binary(),
    LineSep :: binary()
) -> iodata().
encode_something(Payload, Separator, Escape, LineSep) ->
    erlang:apply(
        get_parser_for_with_line_separator(Separator, Escape, LineSep),
        dump_to_iodata,
        [Payload]
    ).

get_parser_for_with_line_separator(Separator, Escape, LineSep) ->
    gen_server:call(?MODULE, {get_parser_for, Separator, Escape, LineSep}).

get_parser_for(Separator, Escape) ->
    gen_server:call(?MODULE, {get_parser_for, Separator, Escape}).

get_store() ->
    gen_server:call(?MODULE, {get_store}).

%%
%%
init([]) ->
    {ok, []}.

%%
%%
handle_call({get_store}, _From, ParserStore) ->
    {reply, ParserStore, ParserStore};
handle_call({get_parser_for, Separator, Escape, LineSep}, _From, ParserStore) ->
    case
        lists:filter(
            fun({S, E, L, _P}) ->
                S =:= Separator andalso L =:= LineSep andalso E =:= Escape
            end,
            ParserStore
        )
    of
        [{Separator, Escape, LineSep, Parser}] ->
            {reply, Parser, ParserStore};
        _ ->
            ModuleName =
                list_to_atom(
                    lists:flatten(
                        io_lib:format(
                            "csv_parser_~s",
                            [binary_to_list(generate_id())]
                        )
                    )
                ),

            ParserOpts = add_escape(
                [
                    {separator, Separator},
                    {line_separator, LineSep}
                ],
                Escape
            ),

            {module, Parser, _, ok} =
                'Elixir.ErlangRedHelpers':define_csv_parser(
                    ModuleName, ParserOpts
                ),

            {reply, Parser, [
                {Separator, Escape, LineSep, Parser} | ParserStore
            ]}
    end;
handle_call({get_parser_for, Separator, Escape}, _From, ParserStore) ->
    case
        lists:filter(
            fun({S, E, L, _P}) ->
                S =:= Separator andalso L =:= no_line_sep andalso E =:= Escape
            end,
            ParserStore
        )
    of
        [{Separator, Escape, no_line_sep, Parser}] ->
            {reply, Parser, ParserStore};
        _ ->
            ModuleName =
                list_to_atom(
                    lists:flatten(
                        io_lib:format(
                            "csv_parser_~s",
                            [binary_to_list(generate_id())]
                        )
                    )
                ),

            ParserOpts = add_escape(
                [
                    {separator, Separator},
                    {newlines, [
                        <<"\r\n">>,
                        <<"\n">>,
                        <<"\r">>
                    ]}
                ],
                Escape
            ),

            {module, Parser, _, ok} =
                'Elixir.ErlangRedHelpers':define_csv_parser(
                    ModuleName, ParserOpts
                ),

            {reply, Parser, [
                {Separator, Escape, no_line_sep, Parser} | ParserStore
            ]}
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
