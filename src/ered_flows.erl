-module(ered_flows).

-export([parse_flow_file/1]).
-export([compute_timeout/1]).
-export([append_tab_name_to_filename/2]).
-export([is_test_case_pending/1]).
-export([should_keep_flow_running/1]).

-import(ered_msg_handling, [
   decode_json/1
]).

%%
%% Compute a timeout for the flow test, can be set in the flows.json file.
%% Since flows have delays or even test the delay node, it must be possible
%% to define a timeout per flow test. This is possible by using the ENV
%% variables for a flow tab:
%%
%%   double-click on flow tab --> properties --> env --> TIMEOUT
%%
%% a timeout value is always in seconds.
%%
%% Default timeout is 1234 which allows eunit to complete.
compute_timeout([], get_time_in_ms) ->
    1234;
compute_timeout([H | T], get_time_in_ms) ->
    case maps:find(name, H) of
        {ok, <<"ERED_TIMEOUT">>} ->
            case maps:find(value, H) of
                {ok, Val} ->
                    element(1, string:to_integer(Val)) * 1000;
                _ ->
                    1234
            end;
        _ ->
            compute_timeout(T, get_time_in_ms)
    end.

compute_timeout([]) ->
    1234;
compute_timeout([NodeDef | Ary]) ->
    case maps:find(type, NodeDef) of
        {ok, <<"tab">>} ->
            case maps:find(env, NodeDef) of
                {ok, EnvAry} ->
                    compute_timeout(EnvAry, get_time_in_ms);
                _ ->
                    1234
            end;
        _ ->
            compute_timeout(Ary)
    end.

%%
%%
parse_flow_file(FileName) ->
    {ok, Json} = file:read_file(FileName),
    decode_json(Json).

%%
%%
append_tab_name_to_filename(Ary, FileName) ->
    append_tab_name_to_filename(Ary, FileName, maps:find(z, lists:nth(2, Ary))).

append_tab_name_to_filename([], FileName, {ok, TabId}) ->
    {TabId, FileName};
append_tab_name_to_filename([NodeDef | MoreNodeDefs], FileName, {ok, TabId}) ->
    case maps:find(type, NodeDef) of
        {ok, <<"tab">>} ->
            {ok, Val} = maps:find(label, NodeDef),
            {ok, TabId2} = maps:find(id, NodeDef),

            {TabId2,
                binary_to_list(
                    list_to_binary(
                        io_lib:format(
                            "~s (~s)",
                            [Val, FileName]
                        )
                    )
                )};
        _ ->
            append_tab_name_to_filename(MoreNodeDefs, FileName, {ok, TabId})
    end.

%%
%%
get_pending_envvar([]) ->
    false;
get_pending_envvar([H | T]) ->
    case maps:find(name, H) of
        {ok, <<"ERED_PENDING">>} ->
            case maps:find(value, H) of
                {ok, Val} ->
                    (Val == <<"true">>) or (Val == <<"TRUE">>);
                _ ->
                    false
            end;
        _ ->
            get_pending_envvar(T)
    end.

is_test_case_pending([]) ->
    false;
is_test_case_pending([NodeDef | MoreNodeDefs]) ->
    case maps:find(type, NodeDef) of
        {ok, <<"tab">>} ->
            case maps:find(env, NodeDef) of
                {ok, EnvAry} ->
                    get_pending_envvar(EnvAry);
                _ ->
                    false
            end;
        _ ->
            is_test_case_pending(MoreNodeDefs)
    end.

%%
%%
keep_running([]) ->
    false;
keep_running([H | T]) ->
    case maps:find(name, H) of
        {ok, <<"ERED_KEEPRUNNING">>} ->
            case maps:find(value, H) of
                {ok, Val} ->
                    (Val == <<"true">>) or (Val == <<"TRUE">>);
                _ ->
                    false
            end;
        _ ->
            keep_running(T)
    end.

should_keep_flow_running([]) ->
    false;
should_keep_flow_running([NodeDef | MoreNodeDefs]) ->
    case maps:find(type, NodeDef) of
        {ok, <<"tab">>} ->
            case maps:find(env, NodeDef) of
                {ok, EnvAry} ->
                    keep_running(EnvAry);
                _ ->
                    false
            end;
        _ ->
            should_keep_flow_running(MoreNodeDefs)
    end.
