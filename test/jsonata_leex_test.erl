-module(jsonata_leex_test).

-include_lib("eunit/include/eunit.hrl").

did_it_fail(T) ->
    element(1, element(1, T)) == error.

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

    FinalResult = lists:filter(fun(A) -> did_it_fail(A) end, Lst),
    ?assertEqual([], FinalResult).
