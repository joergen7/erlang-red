-module(flows).

-export([parse_flow_file/1]).

parse_flow_file(FileName) ->
    {ok, Json} = file:read_file(FileName),

    Push = fun(Key, Value, Acc) ->
       [{binary_to_atom(Key), Value} | Acc]
    end,

    {Ary,_,_} = json:decode(Json, ok, #{object_push => Push}),

    Ary.
