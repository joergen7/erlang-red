-module(jsonata_evaluator).

-export([
    evaluate_erlang/1,
    execute/2,
    jsonata_to_erlang/1
]).

%%
%% The UberAPI to the jsonata_leex and jsonata_parser modules.
%%
%% This module provides a single interface for evaluating a JSONata stanza
%% in the presence of the Msg object.
%%

execute(JSONata, Msg) when is_binary(JSONata) ->
    execute(binary_to_list(JSONata), Msg);
execute(JSONata, Msg) ->
    case jsonata_to_erlang(JSONata) of
        {ok, ErlangCode} ->
            case evaluate_erlang(binary_to_list(ErlangCode)) of
                {ok, Func} ->
                    {ok, Func(Msg)};
                Error ->
                    ErrMsg = io_lib:format(
                        "Stanza: {{{ ~p }}} Error: ~p",
                        [ErlangCode, Error]
                    ),
                    {error, ErrMsg}
            end;
        Error ->
            {error, Error}
    end.

%%
%% Inspired by this blog post:
%% https://grantwinney.com/how-to-evaluate-a-string-of-code-in-erlang-at-runtime/
%%
handle_local_function(FunctionName, [Arg]) ->
    case FunctionName of
        any_to_list when is_float(Arg) ->
            float_to_list(Arg);
        any_to_list when is_integer(Arg) ->
            integer_to_list(Arg);
        any_to_list when is_binary(Arg) ->
            binary_to_list(Arg);
        any_to_list when is_atom(Arg) ->
            atom_to_list(Arg);
        any_to_list ->
            Arg;
        %%
        %% misnomer - converts the value to a binary that is displayed as
        %% a string in the flow editor.
        to_string when is_binary(Arg) ->
            Arg;
        to_string when is_list(Arg) ->
            list_to_binary(Arg);
        to_string when is_atom(Arg) ->
            atom_to_binary(Arg);
        to_string when is_integer(Arg) ->
            integer_to_binary(Arg);
        to_string ->
            list_to_binary(io_lib:format("~p", [Arg]))
    end.

evaluate_erlang(Expression) ->
    case erl_scan:string(Expression) of
        {ok, Tokens, _} ->
            case erl_parse:parse_exprs(Tokens) of
                {ok, Parsed} ->
                    case
                        erl_eval:exprs(
                            Parsed,
                            [],
                            {value, fun handle_local_function/2}
                        )
                    of
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

%%
%%
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
