-module(ered_msg_handling).

-export([
    convert_to_num/1,
    convert_units_to_milliseconds/2,
    create_outgoing_msg/1,
    decode_json/1,
    delete_prop/2,
    get_prop/2,
    map_keys_to_binary/1,
    map_keys_to_lists/1,
    retrieve_prop_value/2,
    set_prop_value/3,
    timestamp/0
]).

%%
%% Helper functions for dealing with messages and their electics.
%%

-import(ered_nodes, [
    generate_id/0,
    jstr/2
]).

%%
%% convert to num - a num can be a integer or float, so to convert a string
%% to one of these inluding the negativity that is minus.
convert_to_num(V) when is_binary(V) ->
    convert_to_num(binary_to_list(V));
convert_to_num(V) when is_number(V) ->
    V;
convert_to_num(V) when is_integer(V) ->
    V;
convert_to_num(V) when is_list(V) ->
    case string:to_float(V) of
        {error, _} ->
            case string:to_integer(V) of
                {error, _} ->
                    {error,
                        jstr("WARNING: Value ~p cannot be convereted to num", [
                            V
                        ])};
                {V2, _} ->
                    V2
            end;
        {V3, _} ->
            V3
    end.

%%
%% these are from the trigger node
convert_units_to_milliseconds({ok, <<"hr">>}, {ok, Val}) ->
    {ok, element(1, string:to_integer(Val)) * 1000 * 60 * 60};
convert_units_to_milliseconds({ok, <<"min">>}, {ok, Val}) ->
    {ok, element(1, string:to_integer(Val)) * 1000 * 60};
convert_units_to_milliseconds({ok, <<"s">>}, {ok, Val}) ->
    {ok, element(1, string:to_integer(Val)) * 1000};
convert_units_to_milliseconds({ok, <<"ms">>}, {ok, Val}) ->
    {ok, element(1, string:to_integer(Val))};
%%
%% these are from the delay node
convert_units_to_milliseconds({ok, <<"days">>}, {ok, Val}) ->
    {ok, element(1, string:to_integer(Val)) * 1000 * 60 * 60 * 24};
convert_units_to_milliseconds({ok, <<"hours">>}, {ok, Val}) ->
    {ok, element(1, string:to_integer(Val)) * 1000 * 60 * 60};
convert_units_to_milliseconds({ok, <<"minutes">>}, {ok, Val}) ->
    {ok, element(1, string:to_integer(Val)) * 1000 * 60};
convert_units_to_milliseconds({ok, <<"seconds">>}, {ok, Val}) ->
    {ok, element(1, string:to_integer(Val)) * 1000};
convert_units_to_milliseconds({ok, <<"milliseconds">>}, {ok, Val}) ->
    {ok, element(1, string:to_integer(Val))};
convert_units_to_milliseconds(A, B) ->
    {error, jstr("WARNING: convert to ms: nomatch for ~p & ~p~n", [A, B])}.

%%
%% When something is a date type, it gets this value.
timestamp() ->
    erlang:system_time(millisecond).

%%
%% Decode a json string just like we like it, with atoms as keys
decode_json(Val) when is_list(Val) ->
    decode_json(list_to_binary(Val));
decode_json(Val) ->
    AtomizeKeys = fun(Key, Value, Acc) ->
        [{binary_to_atom(Key), Value} | Acc]
    end,
    {Obj, _, _} = json:decode(Val, ok, #{object_push => AtomizeKeys}),
    Obj.

any_to_atom(V) when is_atom(V) ->
    V;
any_to_atom(V) when is_binary(V) ->
    binary_to_atom(V);
any_to_atom(V) when is_list(V) ->
    list_to_atom(V);
any_to_atom(V) when is_integer(V) ->
    list_to_atom(integer_to_list(V));
any_to_atom(V) when is_float(V) ->
    list_to_atom(float_to_list(V, [short]));
any_to_atom(V) ->
    V.

%%
%% get prop can used to retrieve a nestd value from the Msg map, i.e.,
%%    msg.payload.key1.key2.key3
%% from this:
%%    #{ paylad => #{ key1 => #{ key2 => key3 => <<"this is the value">> }}}
%% gives
%%    <<"this is the value">>
%%
%% designed to be used with a maps:find, e.g.
%%
%%    get_prop(maps:find(p,Rule), Msg)
%%
%% retuns
%%
%%    {ok, Value, Prop}
%% or
%%    {undefined, Prop}
%%
get_prop({ok, Prop}, Msg) ->
    KeyNames = lists:map(fun any_to_atom/1, string:split(Prop, ".", all)),
    case mapz:deep_find(KeyNames, Msg) of
        {ok, V} ->
            {ok, V, Prop};
        error ->
            {undefined, Prop}
    end.

%%
%% Retrieve a nested parameters from the Msg map.
-spec retrieve_prop_value(PropName :: string(), Msg :: map()) -> any().
retrieve_prop_value(PropName, Msg) ->
    case get_prop({ok, PropName}, Msg) of
        {ok, V, _} ->
            V;
        _ ->
            ""
    end.

%% Set a value in a nested map. This supports using nesting parameters to
%% set a value somewhere in a map.
-spec set_prop_value(PropName :: string(), Msg :: map(), Value :: any()) ->
    map().
set_prop_value(PropName, Msg, Value) ->
    KeyNames = lists:map(fun any_to_atom/1, string:split(PropName, ".", all)),
    %% silently ignore any key that isn't available
    try
        mapz:deep_put(KeyNames, Value, Msg)
    catch
        error:_Error ->
            Msg
    end.

%%
%% remove an property that happens to match the nested path provided.
-spec delete_prop(
    PropName :: string() | {ok, PropName :: string()}, Msg :: map()
) -> map().
delete_prop({ok, PropName}, Msg) ->
    delete_prop(PropName, Msg);
delete_prop(PropName, Msg) ->
    KeyNames = lists:map(fun any_to_atom/1, string:split(PropName, ".", all)),
    %% silently ignore any key that isn't available

    %% deep_remove seems to delete keeps if other keys don't exist, so this
    %% pre-check ensures there is a value
    %% see: https://github.com/eproxus/mapz/issues/1
    %% TODO remove this if deep_remove is fixed
    case mapz:deep_find(KeyNames, Msg) of
        {ok, _} ->
            try
                mapz:deep_remove(KeyNames, Msg)
            catch
                error:_Error ->
                    Msg
            end;
        _ ->
            Msg
    end.

%%
%% Generate an empty message map with just an _msgid
create_outgoing_msg(WsName) ->
    {outgoing, #{'_msgid' => generate_id(), '_ws' => WsName}}.

%%
%%
key_to_binary(V) when is_binary(V) ->
    V;
key_to_binary(V) when is_atom(V) ->
    erlang:atom_to_binary(V);
key_to_binary(V) when is_list(V) ->
    erlang:list_to_binary(V);
key_to_binary(V) ->
    V.

%%
%%
map_keys_to_binary(Map) ->
    maps:from_list(
        lists:map(
            fun({D, E}) -> {key_to_binary(D), E} end,
            maps:to_list(Map)
        )
    ).

%%
%%
key_to_list(V) when is_binary(V) ->
    erlang:binary_to_list(V);
key_to_list(V) when is_atom(V) ->
    erlang:atom_to_list(V);
key_to_list(V) when is_list(V) ->
    V;
key_to_list(V) ->
    V.

%%
%%
map_keys_to_lists(Map) ->
    maps:from_list(
        lists:map(
            fun({D, E}) -> {key_to_list(D), E} end,
            maps:to_list(Map)
        )
    ).
