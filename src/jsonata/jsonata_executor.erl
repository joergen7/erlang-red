-module(jsonata_executor).

-export([
    evaluate_erlang/1,
    execute/2,
    jsonata_to_erlang/1
]).

execute(JSONata, Msg) ->
    case jsonata_to_erlang(JSONata) of
        {ok, ErlangCode} ->
            case evaluate_erlang(binary_to_list(ErlangCode)) of
                {ok, Func} ->
                    {ok, Func(Msg)};
                Error ->
                    {error, Error}
            end;
        Error ->
            {error, Error}
    end.

evaluate_erlang(Expression) ->
    case erl_scan:string(Expression) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, Parsed} ->
                    case erl_eval:exprs(Parsed, []) of
                        {value, Result, _} ->
                            {ok, Result};
                        Error ->
                            {error, Error}
                    end;
                Error ->
                    {error, Error}
            end;
        Error ->
            {error, Error}
    end.

jsonata_to_erlang(JSONataString) ->
    {ok, Tokens, _} = jsonata_leex:string(JSONataString),
    case jsonata_leex:string(JSONataString) of
        {ok, Tokens, _} ->
            case jsonata_parser:parse(Tokens) of
                {ok, Result} ->
                    {ok, Result};
                {error, Error} ->
                    {error, Error};
                Error ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error};
        Error ->
            {error, Error}
    end.
