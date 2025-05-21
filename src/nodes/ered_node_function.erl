-module(ered_node_function).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Function node allows for Erlang to be executed on the server. I won't go
%% into what could possibly go wrong, it will fine. Don't worry about it.
%%

%%         "id": "f9fb40d63b894db4",
%%         "type": "function",
%%         "z": "3bba732ae17b01a9",
%%         "name": "function 2",
%%         "func": "\nfun (Msg) ->\n    Msg\nend.\n",
%%         "outputs": 1,
%%         "timeout": 0,
%%         "noerr": 0,
%%         "initialize": "",
%%         "finalize": "",
%%         "libs": [],
%%         "x": 639,
%%         "y": 385,
%%         "wires": [
%%             [
%%                 "295273281dd5b5c5"
%%             ]
%%         ]

-import(ered_nodes, [
    jstr/2,
    post_exception_or_debug/3,
    send_msg_to_connected_nodes/2
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

handle_event({registered, WsName, _Pid}, NodeDef) ->
    case maps:find(initialize, NodeDef) of
        {ok, <<>>} ->
            NodeDef;
        {ok, Code} ->
            execute(
                io_lib:format("fun(NodeDef,Msg) -> ~n ~s ~n end.", [Code]),
                NodeDef,
                #{'_ws' => WsName}
            );
        _ ->
            NodeDef
    end;
handle_event({stop, WsName}, NodeDef) ->
    case maps:find(finalize, NodeDef) of
        {ok, <<>>} ->
            NodeDef;
        {ok, Code} ->
            execute(
                io_lib:format("fun(NodeDef,Msg) -> ~n ~s ~n end.", [Code]),
                NodeDef,
                #{'_ws' => WsName}
            );
        _ ->
            NodeDef
    end;
handle_event(_, NodeDef) ->
    NodeDef.

handle_msg({incoming, Msg}, NodeDef) ->
    %% TODO don't support multiple outputs
    Msg2 =
        case maps:find(func, NodeDef) of
            {ok, <<>>} ->
                Msg;
            {ok, Code} ->
                execute(
                    io_lib:format(
                        "fun(NodeDef,Msg) -> ~n ~s ~n end.",
                        [Code]
                    ),
                    NodeDef,
                    Msg
                );
            _ ->
                Msg
        end,
    send_msg_to_connected_nodes(NodeDef, Msg2),
    {handled, NodeDef, Msg2};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%
execute(ErlangCode, NodeDef, Msg) when is_list(ErlangCode) ->
    execute(list_to_binary(ErlangCode), NodeDef, Msg);
execute(ErlangCode, NodeDef, Msg) ->
    try
        case evaluate_erlang(binary_to_list(ErlangCode)) of
            {ok, Func} ->
                Func(NodeDef, Msg);
            Error ->
                ErrMsg = io_lib:format(
                    "Stanza: {{{ ~p }}} Error: ~p",
                    [ErlangCode, Error]
                ),

                post_exception_or_debug(NodeDef, Msg, ErrMsg),
                NodeDef
        end
    catch
        E:F:S ->
            ErrMsg2 = io_lib:format(
                "Stanza: {{{ ~p }}}~nError: ~p:~p~nStack ~p~n",
                [ErlangCode, E, F, S]
            ),
            post_exception_or_debug(NodeDef, Msg, ErrMsg2),
            NodeDef
    end.

%%
%%
handle_local_function(FunctionName, Args) ->
    io:format("~p(~p)", [FunctionName, Args]).

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
                            {ok, Result}
                    end;
                Error ->
                    {error, Error}
            end;
        Error ->
            {error, Error}
    end.
