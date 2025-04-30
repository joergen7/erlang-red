-module(ered_msg_handling).

-export([
    convert_to_num/1,
    convert_units_to_milliseconds/2,
    create_outgoing_msg/1,
    decode_json/1,
    get_prop/2,
    map_keys_to_binary/1,
    map_keys_to_lists/1,
    retrieve_prop_value/2,
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

%%
%% Helper for getting values for properties of the form:
%%    payload.key1.key2.key3
%% from maps of the form
%%    #{ payload => #{ key1 => #{ key2 => #{ key3 => "final value" }}}}
%% So a call to get nested value would return
%%    final value
%% The array argument is ["payload", "key1", "key2", "key3"], i.e. a split
%% by dot on the original string.
%%
get_nested_value(V, []) ->
    {ok, V};
get_nested_value(Map, [H | T]) when is_map(Map) and is_binary(H) ->
    get_nested_value(Map, [binary_to_atom(H) | T]);
get_nested_value(Map, [H | T]) when is_map(Map) and is_list(H) ->
    get_nested_value(Map, [list_to_atom(H) | T]);
get_nested_value(Map, [H | T]) when is_map(Map) and is_atom(H) ->
    case maps:find(H, Map) of
        {ok, Val} ->
            get_nested_value(Val, T);
        _ ->
            undefined
    end;
get_nested_value(_, _) ->
    undefined.

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
    case get_nested_value(Msg, string:split(Prop, ".", all)) of
        {ok, V} ->
            {ok, V, Prop};
        undefined ->
            {undefined, Prop}
    end.

%%
%% slightly simpler API, without the {ok, ...}
retrieve_prop_value(PropName, Msg) ->
    case get_prop({ok, PropName}, Msg) of
        {ok, V, _} ->
            V;
        _ ->
            ""
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
