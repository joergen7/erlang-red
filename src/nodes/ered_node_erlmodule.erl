-module(ered_node_erlmodule).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

-export([install/2]).

%%
%% Erlang-Red Erlang module node for defining Erlang modules.
%%
%% Code inject modules.
%%
%% This does not nothing except for install code into the BEAM.
%%

-import(ered_nodes, [
    post_exception_or_debug/3
]).

-import(ered_nodered_comm, [
    node_status/5,
    node_status_clear/2
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%
install(NodeDef, WsName) ->
    ModuleName = binary_to_atom(maps:get(module_name, NodeDef)),
    ModuleCode = maps:get(code, NodeDef),

    FileName = binary_to_list(
        list_to_binary(
            io_lib:format("/tmp/~s.erl", [ModuleName])
        )
    ),

    file:write_file(FileName, ModuleCode),

    case compile:file(FileName, [binary, return_errors, return_warnings]) of
        {ok, ModuleName, Binary, _} ->
            node_status(WsName, NodeDef, "installed", "green", "dot"),
            NodeId = #{id => maps:get(id, NodeDef)},
            spawn(fun() -> clear_status_after_one_sec(WsName, NodeId) end),
            {module, ModuleName} = code:load_binary(ModuleName, [], Binary);
        {error, ErrorList, WarnList} ->
            Msg = #{
                '_ws' => WsName,
                error => compiler_list_to_list(ErrorList),
                warning => compiler_list_to_list(WarnList)
            },
            node_status(WsName, NodeDef, "compile failed", "red", "dot"),
            post_exception_or_debug(NodeDef, Msg, <<"compile failed">>)
    end,

    file:delete(FileName).

%%
%% ErrorLists and WarnLists aren't JSON compatible therefore they need to be
%% massaged into form.
%% See https://www.erlang.org/doc/apps/compiler/compile.html for details.
compiler_list_to_list([]) ->
    [];
compiler_list_to_list([{_Filename, [ErrorInfo]} | Lst]) ->
    [errorinfo_to_list(ErrorInfo) | compiler_list_to_list(Lst)].

errorinfo_to_list({{Line, Char}, Module, Desc}) ->
    [
        list_to_binary(io_lib:format("Line: ~p, Char: ~p", [Line, Char])),
        Module,
        list_to_binary(io_lib:format("~p", [Desc]))
    ].

%%
%%
clear_status_after_one_sec(WsName, NodeId) ->
    timer:sleep(1000),
    node_status_clear(WsName, NodeId).
