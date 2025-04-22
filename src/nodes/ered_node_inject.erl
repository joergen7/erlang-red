-module(ered_node_inject).

-export([node_inject/2]).
-export([handle_event/2]).
-export([handle_outgoing/2]).

-import(ered_node_receivership, [enter_receivership/3]).

-import(ered_nodes, [
    get_prop_value_from_map/2,
    get_prop_value_from_map/3,
    jstr/2,
    send_msg_to_connected_nodes/2
]).

-import(ered_nodered_comm, [
    unsupported/3
]).

-import(ered_msg_handling, [
    timestamp/0,
    decode_json/1
]).

%%
%% Inject node should have at least one outgoing wire, if not then the
%% needle won't hit the vein, i.e. the message won't flow through any nodes.
%%

handle_topic_value(NodeDef, _Prop, {ok, <<"str">>}) ->
    get_prop_value_from_map(topic, NodeDef);
handle_topic_value(_NodeDef, Prop, _) ->
    get_prop_value_from_map(v, Prop).

parse_props([], _, Msg) ->
    Msg;
parse_props([Prop | RestProps], NodeDef, Msg) ->
    %%
    %% TODO ignoring type definitions here - so be it. Type is defined by the
    %% TODO 'vt' attribute of the Prop map except in the case of payload.
    %% TODO payload has payloadType on the NodeDef.
    %%
    case maps:find(p, Prop) of
        {ok, <<"payload">>} ->
            Val = get_prop_value_from_map(payload, NodeDef),
            PType = get_prop_value_from_map(payloadType, NodeDef),
            io:format("Prop: Payload: ~p of type ~p\n", [Val, PType]),

            case PType of
                <<"date">> ->
                    parse_props(
                        RestProps,
                        NodeDef,
                        maps:put(payload, timestamp(), Msg)
                    );
                <<"json">> ->
                    parse_props(
                        RestProps,
                        NodeDef,
                        maps:put(payload, decode_json(Val), Msg)
                    );
                <<"str">> ->
                    parse_props(
                        RestProps,
                        NodeDef,
                        maps:put(payload, Val, Msg)
                    );
                PropType ->
                    unsupported(
                        NodeDef,
                        Msg,
                        jstr("proptype ~p for ~p, handling as string type", [
                            PropType, Prop
                        ])
                    ),
                    parse_props(RestProps, NodeDef, maps:put(payload, Val, Msg))
            end;
        {ok, <<"topic">>} ->
            Val = handle_topic_value(NodeDef, Prop, maps:find(vt, Prop)),
            io:format("Prop: Topic: ~p\n", [Val]),
            parse_props(RestProps, NodeDef, maps:put(topic, Val, Msg));
        {ok, PropName} ->
            Val = get_prop_value_from_map(v, Prop),
            PropType = get_prop_value_from_map(vt, Prop),
            case PropType of
                <<"date">> ->
                    parse_props(
                        RestProps,
                        NodeDef,
                        maps:put(binary_to_atom(PropName), timestamp(), Msg)
                    );
                <<"json">> ->
                    parse_props(
                        RestProps,
                        NodeDef,
                        maps:put(
                            binary_to_atom(PropName), decode_json(Val), Msg
                        )
                    );
                <<"str">> ->
                    parse_props(
                        RestProps,
                        NodeDef,
                        maps:put(
                            binary_to_atom(PropName), Val, Msg
                        )
                    );
                PropType ->
                    unsupported(
                        NodeDef,
                        Msg,
                        jstr("proptype ~p for ~p, handling as string type", [
                            PropType, Prop
                        ])
                    ),
                    parse_props(
                        RestProps,
                        NodeDef,
                        maps:put(binary_to_atom(PropName), Val, Msg)
                    )
            end;
        _ ->
            unsupported(NodeDef, Msg, jstr("Prop: NoMATCH: ~p\n", [Prop])),
            parse_props(RestProps, NodeDef, Msg)
    end.

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%% outgoing messages are triggered by button presses on the UI
%%
handle_outgoing(NodeDef, Msg) ->
    case maps:find(props, NodeDef) of
        {ok, Val} ->
            Props = Val;
        _ ->
            Props = []
    end,

    Msg2 = parse_props(Props, NodeDef, Msg),
    send_msg_to_connected_nodes(NodeDef, Msg2),
    {NodeDef, Msg2}.

node_inject(NodeDef, _WsName) ->
    enter_receivership(?MODULE, NodeDef, only_outgoing).
