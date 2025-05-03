-module(jsonata_evaluator_test).

-include_lib("eunit/include/eunit.hrl").

count_test() ->
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

parse_error_test() ->
    ?assertEqual(
        {error, {error, {1, jsonata_parser, ["syntax error before: ", []]}}},
        jsonata_evaluator:execute(
            "$count($$.payload) /* comment",
            #{payload => [1, 2, 3, 4]}
        )
    ).

replace_test() ->
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
multistatement_test() ->
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

single_expr_test() ->
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

string_concat_test() ->
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
map_test() ->
    ?assertEqual(
        {ok, #{key => "hello world"}},
        jsonata_evaluator:execute(
            "{ \"key\": \"hello world\" }",
            #{}
        )
    ),

    ?assertEqual(
        {ok, #{key => value}},
        jsonata_evaluator:execute(
            "{ key: value }",
            #{}
        )
    ),

    ?assertEqual(
        {ok, #{key => "hello world"}},
        jsonata_evaluator:execute(
            "{ 'key': 'hello world' }",
            #{}
        )
    ),

    ?assertEqual(
        {ok, #{key => 4}},
        jsonata_evaluator:execute(
            "{ \"key\": $$.payload }",
            #{payload => 4}
        )
    ),

    ?assertEqual(
        {ok, #{integer => 4, float => 12.32}},
        jsonata_evaluator:execute(
            "{ 'integer': 4, 'float': 12.32 }",
            #{}
        )
    ),

    ?assertEqual(
        {ok, #{key => 4, key2 => "value two"}},
        jsonata_evaluator:execute(
            "{ \"key\": $$.payload, 'key2': 'value two' }",
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
map_with_string_concat_test() ->
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
map_with_string_binary_concat_test() ->
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
map_string_concat_with_int_test() ->
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

%%
%% $toString is a ErlangRED special function from converting anything to
%% a binary - a string for all intense purposes in the flow editor. For
%% Erlang a binary for the flow editor a string, hence the name toString
%% which is what the user (of the flow editor) will be using.
tostring_from_anything_test() ->
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
    ),
    ?assertEqual(
        {ok, <<"12131312.123">>},
        jsonata_evaluator:execute(
            "$toString($$.payload)",
            #{payload => 12131312.123}
        )
    ).

%%
%% algorithmic is purposefully misspelled here. Why? Dunno.
%% erlfmt:ignore
algorithimc_test() ->
    ?assertEqual(
        {ok, 2},
        jsonata_evaluator:execute(
            "1 + 1",
            #{}
        )
    ),
    ?assertEqual(
        {ok, 40},
        jsonata_evaluator:execute(
            "$count($$.payload) * 5",
            #{payload => [one, two, three, four, five, six, 'and', seven]}
        )
    ),
    ?assertEqual(
        {ok, 440},
        jsonata_evaluator:execute(
            "$count($$.payload) * 5 * $length($$.str)",
            #{
                payload => [one, two, three, four, five, six, 'and', seven],
                str => "onetwothree"
            }
        )
    ),
    ?assertEqual(
        {ok, 38.743},
        jsonata_evaluator:execute(
            "$$.payload.fuba.dad + 1.2 + 2.3 + $$.payload.name.name
                                                            + 3.243 + 4 * 6",
            #{payload => #{fuba => #{dad => 4}, name => #{name => 4}}}
        )
    ).

single_quote_string_test() ->
    ?assertEqual(
        {ok, "hello world"},
        jsonata_evaluator:execute(
            "'hello' & ' ' & 'world'",
            #{}
        )
    ).

%% erlfmt:ignore
name_as_funct_argument_test() ->
    ?assertEqual(
        {ok, #{
            float => 1.23,
            key => <<"single quote strings">>,
            key2 => <<"value">>,
            banaint => 4
        }},
        jsonata_evaluator:execute(
            "{ \"key\": $toString('single quote strings'), banaint: 4,
                                   float: 1.23, key2: $toString(value) }",
            #{}
        )
    ).
