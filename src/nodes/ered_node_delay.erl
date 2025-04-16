-module(ered_node_delay).

-export([node_delay/2]).
-export([handle_incoming/2]).

-import(ered_node_receivership, [enter_receivership/3]).
-import(nodered, [
    unsupported/3
]).
-import(ered_nodes, [
    jstr/2,
    send_msg_to_connected_nodes/2
]).

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
    {error, jstr("WARNING: Delay nomatch for ~p & ~p~n", [A, B])}.

%%
%% this must return a millisecond value.
compute_pause({ok, <<"delay">>}, NodeDef, Msg) ->
    ConversionResult = convert_units_to_milliseconds(
        maps:find(timeoutUnits, NodeDef),
        maps:find(timeout, NodeDef)
    ),
    case ConversionResult of
        {ok, V} ->
            V;
        {error, ErrMsg} ->
            unsupported(NodeDef, Msg, ErrMsg),
            0
    end;
compute_pause(PType, NodeDef, Msg) ->
    unsupported(NodeDef, Msg, jstr("PauseType: '~p'", [PType])),
    0.

%%
%%
handle_incoming(NodeDef, Msg) ->
    timer:sleep(compute_pause(maps:find(pauseType, NodeDef), NodeDef, Msg)),
    send_msg_to_connected_nodes(NodeDef, Msg),
    NodeDef.

node_delay(NodeDef,_WsName) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, only_incoming).
