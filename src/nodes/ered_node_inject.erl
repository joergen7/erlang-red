-module(ered_node_inject).

-export([node_inject/2]).
-export([handle_outgoing/2]).

-import(ered_node_receivership, [enter_receivership/3]).

-import(ered_nodes, [
    get_prop_value_from_map/2,
    get_prop_value_from_map/3,
    jstr/2,
    send_msg_to_connected_nodes/2
]).

-import(nodered, [
    unsupported/3
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
                        maps:put(
                            payload,
                            erlang:system_time(millisecond),
                            Msg
                        )
                    );
                _ ->
                    parse_props(RestProps, NodeDef, maps:put(payload, Val, Msg))
            end;
        {ok, <<"topic">>} ->
            Val = handle_topic_value(NodeDef, Prop, maps:find(vt, Prop)),
            io:format("Prop: Topic: ~p\n", [Val]),
            parse_props(RestProps, NodeDef, maps:put(topic, Val, Msg));
        {ok, PropName} ->
            Val = get_prop_value_from_map(v, Prop),
            parse_props(
                RestProps,
                NodeDef,
                maps:put(binary_to_atom(PropName), Val, Msg)
            );
        _ ->
            unsupported(NodeDef, Msg, jstr("Prop: NoMATCH: ~p\n", [Prop])),
            parse_props(RestProps, NodeDef, Msg)
    end.

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
    send_msg_to_connected_nodes(NodeDef, parse_props(Props, NodeDef, Msg)),
    NodeDef.

node_inject(NodeDef, _WsName) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, only_outgoing).
