-module(ered_node_json).

-export([node_json/1]).
-export([handle_incoming/2]).

-import(ered_node_receivership, [enter_receivership/3]).

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

%% json does not deal with other sources than msg.
%%    "property": "payload",
%%    "action": "",
%%    "pretty": false,
handle_incoming(NodeDef, Msg) ->
    {ok, Prop} = maps:find(property, NodeDef),
    case maps:find(binary_to_atom(Prop), Msg) of
        {ok, Val} ->
            {ok, Action} = maps:find(action, NodeDef),
            {ok, Pretty} = maps:find(pretty, NodeDef),

            case action_to_content(Val, Action, Pretty) of
                {ok, Response} ->
                    Msg2 = maps:put(payload, Response, Msg),
                    ered_nodes:send_msg_to_connected_nodes(NodeDef, Msg2);
                unsupported ->
                    ErrMsg = ered_nodes:jstr(
                        "Unsupported Action: ~p",
                        [Action]
                    ),
                    nodered:debug(
                        nodered:ws(Msg),
                        nodered:debug_string(NodeDef, ErrMsg),
                        warning
                    );
                _ ->
                    ErrMsg = ered_nodes:jstr(
                        "Unknown error occured: ~p",
                        [Msg]
                    ),
                    nodered:debug(
                        nodered:ws(Msg),
                        nodered:debug_string(NodeDef, ErrMsg),
                        error
                    )
            end;
        _ ->
            ErrMsg = ered_nodes:jstr(
                "Property not defined on Msg: ~p --> ~p",
                [Prop, Msg]
            ),
            nodered:debug(
                nodered:ws(Msg),
                nodered:debug_string(NodeDef, ErrMsg),
                error
            )
    end,
    NodeDef.

node_json(NodeDef) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, only_incoming).
