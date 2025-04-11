-module(node_delay).

-export([node_delay/1]).
-export([handle_incoming/2]).

convert_units_to_milliseconds({ok,<<"days">>},{ok,Val}) ->
    element(1, string:to_integer(Val)) * 1000 * 60 * 60 * 24;

convert_units_to_milliseconds({ok,<<"hours">>},{ok,Val}) ->
    element(1, string:to_integer(Val)) * 1000 * 60 * 60;

convert_units_to_milliseconds({ok,<<"minutes">>},{ok,Val}) ->
    element(1, string:to_integer(Val)) * 1000 * 60;

convert_units_to_milliseconds({ok,<<"seconds">>},{ok,Val}) ->
    element(1, string:to_integer(Val)) * 1000;

convert_units_to_milliseconds({ok,<<"milliseconds">>},{ok,Val}) ->
    element(1, string:to_integer(Val));

convert_units_to_milliseconds(A,B) ->
    io:format("WARNING: Delay nomatch for ~p & ~p~n",[A,B]),
    0.

%%
%% this must return a millisecond value.
compute_pause({ok,<<"delay">>},NodeDef,_Msg) ->
    convert_units_to_milliseconds(maps:find(timeoutUnits,NodeDef),
                                  maps:find(timeout,NodeDef));

compute_pause(PType,NodeDef,Msg) ->
    ErrMsg = nodes:jstr("Unknown PauseType: '~p'",[PType]),
    nodes:this_should_not_happen(NodeDef, io_lib:format("~p ~p\n", [ErrMsg,Msg])),
    nodered:debug(nodered:debug_string(NodeDef, ErrMsg), notice),
    nodered:node_status(NodeDef, "unknown pauseType", "red", "dot"),
    0.

%%
%%
handle_incoming(NodeDef,Msg) ->
    timer:sleep(compute_pause(maps:find(pauseType,NodeDef),NodeDef,Msg)),

    nodes:send_msg_to_connected_nodes(NodeDef,Msg),

    NodeDef.

node_delay(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE, NodeDef, only_incoming).
