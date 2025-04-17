-module(ered_node_json).

-export([node_json/2]).
-export([handle_incoming/2]).

%%
%% Json node only deals with data coming in on the msg object:
%%
%% Attributes of interest:
%%
%%    "property": "payload",
%%    "action": "",
%%    "pretty": false,

-import(ered_node_receivership, [enter_receivership/3]).
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
            {ok, jiffy:decode(Val)};
        false ->
            io:format("object is object~n", []),
            {ok, jiffy:encode(Val)}
    end;
action_to_content(Val, <<"obj">>, _Pretty) ->
    %% convert to Javascript object
    {ok, jiffy:decode(Val)};
action_to_content(Val, <<"str">>, _Pretty) ->
    %% convert to object to Javascript string
    {ok, jiffy:encode(Val)};
action_to_content(_, _, _) ->
    unsupported.

handle_incoming(NodeDef, Msg) ->
    {ok, Prop} = maps:find(property, NodeDef),
    case maps:find(binary_to_atom(Prop), Msg) of
        {ok, Val} ->
            {ok, Action} = maps:find(action, NodeDef),
            {ok, Pretty} = maps:find(pretty, NodeDef),

            case action_to_content(Val, Action, Pretty) of
                {ok, Response} ->
                    Msg2 = maps:put(payload, Response, Msg),
                    send_msg_to_connected_nodes(NodeDef, Msg2);
                unsupported ->
                    ErrMsg = jstr("Unsupported Action: ~p", [Action]),
                    unsupported(NodeDef, Msg, ErrMsg)
            end;
        _ ->
            ErrMsg = jstr("Property not defined on Msg: ~p --> ~p", [Prop, Msg]),
            debug(
                ws_from(Msg),
                debug_string(NodeDef, ErrMsg),
                error
            )
    end,
    NodeDef.

node_json(NodeDef, _WsName) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, only_incoming).
