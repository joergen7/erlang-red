-module(ered_node_delay).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Delay pauses the travels of a message by XX units of time.
%%

-import(ered_nodered_comm, [
    unsupported/3
]).
-import(ered_nodes, [
    jstr/2,
    send_msg_to_connected_nodes/2
]).
-import(ered_msg_handling, [
    convert_units_to_milliseconds/2
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

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
handle_event(_, NodeDef) ->
    NodeDef.
%%
%%
handle_incoming(NodeDef, Msg) ->
    timer:sleep(compute_pause(maps:find(pauseType, NodeDef), NodeDef, Msg)),
    send_msg_to_connected_nodes(NodeDef, Msg),
    {NodeDef, Msg}.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    {NodeDef2, Msg2} = handle_incoming(NodeDef, Msg),
    {handled, NodeDef2, Msg2};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
