%%% % @noformat

-module(attribute_parser_test).

-include_lib("eunit/include/eunit.hrl").

foreach_parser_test_() ->
    Tests = [
      {
       single_char_also_works,
       "k",
       [k]
      },
      {
       single_capital_char_also_works,
       "K",
       ['K']
      },
      {
       multiple_mixed_case_char_also_works,
       "K.l.M",
       ['K',l,'M']
      },
      {
       multiple_mixed_case_char_also_works_lower_first,
       "k.L.m",
       [k,'L',m]
      },
      {
       simple_atomised_string,
       "key.key1.key2",
       [key, key1, key2]
      },
      {
       even_keys_are_single_double_quotes,
       "[\"key\"]",
       [<<"key">>]
      },
      {
       even_keys_are_single_single_quotes,
       "['key-1-2']",
       ['key-1-2']
      },
      {
       even_keys_are_single,
       "key",
       [key]
      },
      {
       single_bracket,
       "['key']",
       [key]
      },
      {
       even_capital_keys_are_single,
       "Key",
       ['Key']
      },
      {
       mix_of_quites_and_dots,
       "Key['Key-2-'].Fubar.snafu[\"ddd\"]",
       ['Key', 'Key-2-', 'Fubar', snafu, <<"ddd">>]
      },
      {
       another_combination,
       "req.headers[\"x-proto-forward-for\"]",
       [req, headers, <<"x-proto-forward-for">>]
      },
      {
       quoted_brackets,
       "req['[fubar]'].snafu",
       [req, '[fubar]', snafu]
      },
      {
       quoted_with_double_quotes_brackets,
       "req[\"[fubar]\"].snafu",
       [req,<<"[fubar]">>,snafu]
      },
      {
       all_numbers,
       "[\"123\"]['456'][\"789\"]",
       [<<"123">>,'456',<<"789">>]
      },
      {
       all_array_access,
       "[\"abc\"]['def'][\"ghi\"]['H']['I']",
       [<<"abc">>,'def',<<"ghi">>,'H','I']
      },
      {
       collection_of_dots,
       "abcd.a121.b2.c4.d5.e6.f7.g8.h9.i10",
       [abcd,a121,b2,c4,d5,e6,f7,g8,h9,i10]
      }
    ],

    TestList = [{
      atom_to_binary(TestCaseName),
      timeout,5, fun() ->
                         {ok, Tokens, _} =
                             erlang_red_attr_leex:string(SrcString),

                         {ok, Result} = erlang_red_attr_parser:parse(Tokens),

                         ?assertEqual(ResultValue, Result)
                 end
                } || {TestCaseName, SrcString, ResultValue} <- Tests],

    {inparallel, TestList}.

foreach_parser_failure_test_() ->
    Tests = [
      {
       dot_bracket_does_not_work,
       "key.['ddd']"
      },
      {
       name_after_bracket,
       "key['ddd']fubar"
      },
      {
       double_dots,
       "key..ddd"
      },
      {
       triple_dots,
       "key...ddd"
      },
      {
       brackets_without_quotes_does_not_work,
       "key[ddd]fubar"
      },
      {
       double_brackets,
       "key[['ddd']]fubar"
      },
      {
       missing_brackets,
       "key['ddd'fubar]"
      }
    ],

    TestList = [{
      atom_to_binary(TestCaseName),
      timeout,5, fun() ->
                         {ok, Tokens, _} =
                             erlang_red_attr_leex:string(SrcString),
                         R = erlang_red_attr_parser:parse(Tokens),
                         ?assertEqual(error, element(1,R))
                 end
     } || {TestCaseName, SrcString} <- Tests],

    {inparallel, TestList}.

foreach_leex_failure_test_() ->
    Tests = [
      {
       uncomplete_single_quote,
       "key['ddd]fubar"
      },
      {
       uncomplete_double_quote,
       "key[\"ddd]fubar"
      },
      {
       unquoted_numbers_wont_fly,
       "key.123.456"
      }
    ],

    TestList = [{
      atom_to_binary(TestCaseName),
      timeout,5, fun() ->
                         R = erlang_red_attr_leex:string(SrcString),
                         ?assertEqual(error, element(1,R))
                 end
     } || {TestCaseName, SrcString} <- Tests],

    {inparallel, TestList}.
