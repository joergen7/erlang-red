-module(jsonata_evaluator_test).

-include_lib("eunit/include/eunit.hrl").

jsonata_evaluator_count_test() ->
    ?assertEqual(
        {ok, 4},
        jsonata_evaluator:execute(
            "$count($$.payload)",
            #{payload => [1, 2, 3, 4]}

        )
    ),
    ?assertEqual(
        {ok, 4},
        jsonata_evaluator:execute(
            "$count(msg.payload)",
            #{payload => [1, 2, 3, 4]}
        )
    ).

jsonata_evaluator_parse_error_test() ->
    ?assertEqual(
        {error, {error, {1, jsonata_parser, ["syntax error before: ", []]}}},
        jsonata_evaluator:execute(
            "$count($$.payload) /* comment",
            #{payload => [1, 2, 3, 4]}
        )
    ).

jsonata_evaluator_replace_test() ->
    ?assertEqual(
        {ok, "ggkkggkkgg"},
        jsonata_evaluator:execute(
            "$replace($$.payload,\"rr\",\"gg\")",
            #{payload => "rrkkrrkkrr"}
        )
    ),

    %% using 'msg' for the '$$' variable. They are equivalent but '$$' is
    %% preferred.
    ?assertEqual(
        {ok, "ggkkggkkgg"},
        jsonata_evaluator:execute(
            "$replace(msg.payload,\"rr\",\"gg\")",
            #{payload => "rrkkrrkkrr"}
        )
    ).

%% erlfmt:ignore
jsonata_evaluator_multistatement_test() ->
    ?assertEqual(
       {ok, '_underscore'},
       jsonata_evaluator:execute(
         "$$.payload ; \"string\" ; 123 ; 321.123 ; $$._underscore",
         #{
           payload => "rrkkrrkkrr",
           '_underscore' => '_underscore'
          }
        )
    ).

jsonata_evaluator_single_expr_test() ->
    ?assertEqual(
        {ok, 4},
        jsonata_evaluator:execute(
            "$$.payload",
            #{payload => 4}
        )
    ),
    ?assertEqual(
        {ok, "string"},
        jsonata_evaluator:execute(
            "\"string\"",
            #{payload => 4}
        )
    ),
    ?assertEqual(
        {ok, 123},
        jsonata_evaluator:execute(
            "123",
            #{payload => 4}
        )
    ),
    ?assertEqual(
        {ok, 123.1},
        jsonata_evaluator:execute(
            "123.1",
            #{payload => 4}
        )
    ).

jsonata_evaluator_string_concat_test() ->
    Msg = #{
        payload => "world"
    },
    ?assertEqual(
        {ok, "hello world"},
        jsonata_evaluator:execute(
            "\"hello\" & \" \" & $$.payload",
            Msg
        )
    ).

%% erlfmt:ignore strings are mismanaged by erlfmt
jsonata_evaluator_map_test() ->
    ?assertEqual(
        {ok, #{key => 4}},
        jsonata_evaluator:execute(
            "{ \"key\": $$.payload }",
            #{payload => 4}
        )
    ),

    Msg = #{
        payload => #{
            key => 4,
            key2 => #{
                key3 => 3,
                key4 => #{
                    key5 => 5
               }
            }
        }
    },

    ?assertEqual(
        {ok, #{key => 4, key2 => 3, key3 => 5}},
        jsonata_evaluator:execute(
            "{ \"key\": $$.payload.key,
               \"key2\": $$.payload.key2.key3,
               \"key3\": $$.payload.key2.key4.key5
          }",
            Msg
        )
    ).

%% erlfmt:ignore strings are mismanaged by erlfmt
jsonata_evaluator_map_with_string_concat_test() ->
    Msg = #{
        payload => #{
            key => "4",
            key2 => #{
                key3 => "3",
                key4 => #{
                    key5 => "5"
               }
            }
        }
    },

    ?assertEqual(
        {ok, #{ key => "4Hello 3 space 5" }},
        jsonata_evaluator:execute(
            "/* commenter there */ { \"key\": $$.payload.key & \"Hello \" &
                   $$.payload.key2.key3 & \" space \" &
                 $$.payload.key2.key4.key5 } /* comment here */",
            Msg
        )
    ).

%% erlfmt:ignore strings are mismanaged by erlfmt
jsonata_evaluator_map_with_string_binary_concat_test() ->
    Msg = #{
        payload => #{
            key => <<"4">>,
            key2 => #{
                key3 => <<"3">>,
                key4 => #{
                    key5 => <<"5">>
               }
            }
        }
    },

    ?assertEqual(
        {ok, #{ key => "4Hello 3 space 5" }},
        jsonata_evaluator:execute(
            "/* commenter there */ { \"key\": $$.payload.key & \"Hello \" &
                   $$.payload.key2.key3 & \" space \" &
                 $$.payload.key2.key4.key5 } /* comment here */",
            Msg
        )
    ).

%% erlfmt:ignore strings are mismanaged by erlfmt
jsonata_evaluator_map_string_concat_with_int_test() ->
    ?assertEqual(
        {ok, "hello  world  1234 10 goodbye  cruel world"},
        jsonata_evaluator:execute(
         "\"hello \" & \" world \" & \" 1234 \" & 10
                & \" goodbye \" & \" cruel \" & world",
            #{}
        )
    ),

    ?assertEqual(
        {ok, "yet another testhello  world  1234 10 goodbye  cruel world"},
        jsonata_evaluator:execute(
         "$$.payload & \"hello \" & \" world \" & \" 1234 \" & 10
                & \" goodbye \" & \" cruel \" & world",
            #{payload => <<"yet another test">>}
        )
    ).

jsonata_tostring_from_anything_test() ->
    ?assertEqual(
        {ok, <<"what">>},
        jsonata_evaluator:execute(
            "$toString($$.payload)",
            #{payload => <<"what">>}
        )
    ),
    ?assertEqual(
        {ok, <<"when">>},
        jsonata_evaluator:execute(
            "$toString($$.payload)",
            #{payload => "when"}
        )
    ),
    ?assertEqual(
        {ok, <<"12131312">>},
        jsonata_evaluator:execute(
            "$toString($$.payload)",
            #{payload => 12131312}
        )
    ).
