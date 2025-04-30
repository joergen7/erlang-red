-module(jsonata_leex_test).

-include_lib("eunit/include/eunit.hrl").

did_it_final({{error, _}, _}) ->
    true;
did_it_final(_) ->
    false.

lexical_test() ->
    {ok, FileData} = file:read_file(
        io_lib:format(
            "~s/jsonata/leex.examples.json",
            [code:priv_dir(erlang_red)]
        )
    ),
    TestCases = lists:map(
        fun(A) -> binary_to_list(A) end, json:decode(FileData)
    ),
    Lst = [{jsonata_leex:string(T), T} || T <- TestCases],

    FinalResult = lists:filter(fun(A) -> did_it_final(A) end, Lst),
    ?assertEqual([], FinalResult).
