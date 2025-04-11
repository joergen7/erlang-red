-module(flows).

-export([parse_flow_file/1]).
-export([compute_timeout/1]).
-export([append_tab_name_to_filename/2]).

%%
%% Compute a timeout for the flow test, can be set in the flows.json file.
compute_timeout([],get_time_in_ms) ->
    1234;
compute_timeout([H|T],get_time_in_ms) ->
    case maps:find(name,H) of
        {ok, <<"TIMEOUT">>} ->
            case maps:find(value,H) of
                {ok, Val} ->
                    element(1,string:to_integer(Val)) * 1000;
                 _ ->
                    1234
            end;
         _ ->
            compute_timeout(T,get_time_in_ms)
    end.

compute_timeout([]) ->
    1234;
compute_timeout([NodeDef|Ary]) ->
    case maps:find(type,NodeDef) of
        {ok, <<"tab">>} ->
            case maps:find(env,NodeDef) of
                {ok,EnvAry} ->
                    compute_timeout(EnvAry,get_time_in_ms);
                _ ->
                    1234
            end;

        _ ->
            compute_timeout(Ary)
    end.


parse_flow_file(FileName) ->
    {ok, Json} = file:read_file(FileName),

    Push = fun(Key, Value, Acc) ->
       [{binary_to_atom(Key), Value} | Acc]
    end,

    {Ary,_,_} = json:decode(Json, ok, #{object_push => Push}),

    Ary.



append_tab_name_to_filename(Ary,FileName) ->
    append_tab_name_to_filename(Ary, FileName, maps:find(z,lists:nth(2,Ary))).

append_tab_name_to_filename([],FileName,{ok,TabId}) ->
    {TabId,FileName};

append_tab_name_to_filename([NodeDef|MoreNodeDefs],FileName,{ok,TabId}) ->
    case maps:find(type,NodeDef) of
        {ok, <<"tab">>} ->
            {ok,Val} = maps:find(label,NodeDef),
            {ok,TabId2} = maps:find(id,NodeDef),

            {TabId2,
             binary_to_list(list_to_binary(io_lib:format("~s (~s)",
                                                         [Val, FileName])))};
        _ ->
            append_tab_name_to_filename(MoreNodeDefs, FileName,{ok,TabId})
    end.
