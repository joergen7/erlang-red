-module(jsonata_executor_test).

-include_lib("eunit/include/eunit.hrl").

jsonata_executor_count_test() ->
    ?assertEqual(
        {ok, 4},
        jsonata_executor:execute(
            "$count($$.payload)",
            #{payload => [1, 2, 3, 4]}
        )
    ),
    ?assertEqual(
        {ok, 4},
        jsonata_executor:execute(
            "$count(msg.payload)",
            #{payload => [1, 2, 3, 4]}
        )
    ).

jsonata_executor_parse_error_test() ->
    ?assertEqual(
        {error,
            {error,
                {1, jsonata_parser, ["syntax error before: ", "comment_start"]}}},
        jsonata_executor:execute(
            "$count($$.payload) /* comment",
            #{payload => [1, 2, 3, 4]}
        )
    ).

jsonata_executor_replace_test() ->
    ?assertEqual(
        {ok, "ggkkggkkgg"},
        jsonata_executor:execute(
            "$replace($$.payload,\"rr\",\"gg\")",
            #{payload => "rrkkrrkkrr"}
        )
    ),
    ?assertEqual(
        {ok, "ggkkggkkgg"},
        jsonata_executor:execute(
            "$replace(msg.payload,\"rr\",\"gg\")",
            #{payload => "rrkkrrkkrr"}
        )
    ).

jsonata_executor_multistatement_test() ->
    ?assertEqual(
        {ok, '_underscore'},
        jsonata_executor:execute(
            "$$.payload ; \"string\" ;\n"
            "                                           123 ; 321.123 ; $$._underscore",
            #{
                payload => "rrkkrrkkrr",
                '_underscore' => '_underscore'
            }
        )
    ).

jsonata_executor_single_expr_test() ->
    ?assertEqual(
        {ok, 4},
        jsonata_executor:execute(
            "$$.payload",
            #{payload => 4}
        )
    ),
    ?assertEqual(
        {ok, "string"},
        jsonata_executor:execute(
            "\"string\"",
            #{payload => 4}
        )
    ),
    ?assertEqual(
        {ok, 123},
        jsonata_executor:execute(
            "123",
            #{payload => 4}
        )
    ),
    ?assertEqual(
        {ok, 123.1},
        jsonata_executor:execute(
            "123.1",
            #{payload => 4}
        )
    ).

jsonata_executor_map_test() ->
    ?assertEqual(
        {ok, #{key => 4}},
        jsonata_executor:execute(
            "{ \"key\": $$.payload }",
            #{payload => 4}
        )
    ),

    Msg = #{
        payload => #{
            key => 4,
            key2 => #{
                key3 => 3,
                key4 => #{key5 => 5}
            }
        }
    },

    ?assertEqual(
        {ok, #{key => 4, key2 => 3, key3 => 5}},
        jsonata_executor:execute(
            "{ \"key\": $$.payload.key,\n"
            "                                              \"key2\": $$.payload.key2.key3,\n"
            "                                              \"key3\": $$.payload.key2.key4.key5\n"
            "                                            }",
            Msg
        )
    ).
