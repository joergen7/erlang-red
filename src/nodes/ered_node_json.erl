-module(ered_node_json).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Json node only deals with data coming in on the msg object:
%%
%% Attributes of interest:
%%
%%    "property": "payload",
%%    "action": "",
%%    "pretty": false,

-import(ered_nodes, [
    jstr/2,
    send_msg_to_connected_nodes/2
]).
-import(ered_nodered_comm, [
    debug/3,
    debug_string/2,
    ws_from/1,
    unsupported/3
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

string_like(Val) when is_binary(Val) ->
    true;
string_like(Val) when is_atom(Val) ->
    true;
string_like(Val) when is_list(Val) ->
    true;
string_like(_) ->
    false.

action_to_content(Val, <<"">>, _Pretty) ->
    case string_like(Val) of
        true ->
            io:format("object is string~n", []),
            {ok, json:decode(Val)};
        false ->
            io:format("object is object~n", []),
            {ok, json:encode(Val)}
    end;
action_to_content(Val, <<"obj">>, _Pretty) ->
    %% convert to Javascript object
    {ok, json:decode(Val)};
action_to_content(Val, <<"str">>, _Pretty) ->
    %% convert to object to Javascript string
    {ok, json:encode(Val)};
action_to_content(_, _, _) ->
    unsupported.

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_incoming(NodeDef, Msg) ->
    {ok, Prop} = maps:find(property, NodeDef),
    case maps:find(binary_to_atom(Prop), Msg) of
        {ok, Val} ->
            {ok, Action} = maps:find(action, NodeDef),
            {ok, Pretty} = maps:find(pretty, NodeDef),

            case action_to_content(Val, Action, Pretty) of
                {ok, Response} ->
                    Msg2 = maps:put(payload, Response, Msg),
                    send_msg_to_connected_nodes(NodeDef, Msg2),
                    {NodeDef, Msg2};
                unsupported ->
                    ErrMsg = jstr("Unsupported Action: ~p", [Action]),
                    unsupported(NodeDef, Msg, ErrMsg),
                    {NodeDef, Msg}
            end;
        _ ->
            ErrMsg = jstr("Property not defined on Msg: ~p --> ~p", [Prop, Msg]),
            debug(
                ws_from(Msg),
                debug_string(NodeDef, ErrMsg),
                error
            ),
            {NodeDef, Msg}
    end.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    {NodeDef2, Msg2} = handle_incoming(NodeDef, Msg),
    {handled, NodeDef2, Msg2};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
