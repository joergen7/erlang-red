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
    post_exception_or_debug/3,
    send_msg_to_connected_nodes/2
]).
-import(ered_nodered_comm, [
    debug/3,
    debug_string/2,
    ws_from/1,
    unsupported/3
]).
-import(ered_msg_handling, [
    decode_json/1
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
            {ok, decode_json(Val)};
        false ->
            {ok, decode_json(Val)}
    end;
action_to_content(Val, <<"obj">>, _Pretty) ->
    %% convert to Javascript object
    {ok, decode_json(Val)};
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
handle_msg({incoming, Msg}, NodeDef) ->
    {ok, Prop} = maps:find(property, NodeDef),
    case maps:find(binary_to_atom(Prop), Msg) of
        {ok, Val} ->
            {ok, Action} = maps:find(action, NodeDef),
            {ok, Pretty} = maps:find(pretty, NodeDef),

            try
                case action_to_content(Val, Action, Pretty) of
                    {ok, Response} ->
                        Msg2 = maps:put(payload, Response, Msg),
                        send_msg_to_connected_nodes(NodeDef, Msg2),
                        {handled, NodeDef, Msg2};
                    unsupported ->
                        ErrMsg = jstr("Unsupported Action: ~p", [Action]),
                        unsupported(NodeDef, Msg, ErrMsg),
                        {handled, NodeDef, Msg}
                end
            catch
                E:F ->
                    ErrMsg2 = io_lib:format("Exception: ~p ~p", [E, F]),
                    post_exception_or_debug(NodeDef, Msg, ErrMsg2),
                    {handled, NodeDef, Msg}
            end;
        _ ->
            ErrMsg = jstr("Property not defined on Msg: ~p --> ~p", [Prop, Msg]),
            post_exception_or_debug(NodeDef, Msg, ErrMsg),
            {handled, NodeDef, Msg}
    end;
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
