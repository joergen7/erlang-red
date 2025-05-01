-module(jsonata_parser_test).

-include_lib("eunit/include/eunit.hrl").

parser_test() ->
    {ok, Tokens, _} = jsonata_leex:string("$count($$.payload)"),

    {ok, Result} = jsonata_parser:parse(Tokens),

    ?assertEqual(
        <<"fun (Msg) -> erlang:length(maps:get(payload,Msg)) end">>,
        Result
    ).
