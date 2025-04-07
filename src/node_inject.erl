-module(node_inject).
-export([node_inject/1]).

%%
%% Inject node should have at least one outgoing wire, if not then the
%% needle won't hit the vein, i.e. the message won't flow through any nodes.
%%

handle_topic_value(NodeDef, _Prop, {ok, <<"str">>}) ->
    nodes:get_prop_value_from_map(topic,NodeDef);

handle_topic_value(_NodeDef, Prop, _) ->
    nodes:get_prop_value_from_map(v,Prop).

parse_props([],_,Msg) ->
    Msg;

parse_props([Prop|RestProps],NodeDef,Msg) ->
    %%
    %% TODO ignoring type definitions here - so be it. Type is defined by the
    %% TODO 'vt' attribute of the Prop map except in the case of payload.
    %% TODO payload has payloadType on the NodeDef.
    %%
    io:format("Prop: ~p\n",[Prop]),
    case maps:find(p,Prop) of
        {ok, <<"payload">>} ->
            Val = nodes:get_prop_value_from_map(payload,NodeDef),
            PType = nodes:get_prop_value_from_map(payloadType,NodeDef),
            io:format("Prop: Payload: ~p of type ~p\n",[Val,PType]),

            case PType of
                <<"date">> ->
                    parse_props(RestProps,NodeDef,
                                maps:put(payload,
                                         erlang:system_time(millisecond), Msg));
                _ ->
                    parse_props(RestProps,NodeDef, maps:put(payload, Val, Msg))
            end;

        {ok, <<"topic">>} ->
            Val = handle_topic_value(NodeDef,Prop,maps:find(vt,Prop)),
            io:format("Prop: Topic: ~p\n",[Val]),
            parse_props(RestProps,NodeDef, maps:put(topic, Val, Msg));

        {ok, PropName} ->
            Val = nodes:get_prop_value_from_map(v,Prop),
            io:format("Prop: Name: ~p = ~p\n",[PropName,Val]),
            parse_props(RestProps,NodeDef,
                        maps:put(binary_to_atom(PropName), Val, Msg));

        _ ->
            io:format("Prop: NoMATCH: ~p\n",[Prop]),
            parse_props(RestProps,NodeDef, Msg)
    end.

%%
%% Listen for messages
%%
receive_loop(NodeDef) ->
    receive
        %%
        %% outgoing messages are triggered by button presses on the UI
        %%
        {outgoing,Msg} ->
            case maps:find(props,NodeDef) of
                {ok,Val} ->
                    Props = Val;
                _ ->
                    Props = []
            end,

            io:format("INJECT: Msg being sent\n"),
            nodes:send_msg_to_connected_nodes(NodeDef,
                                              parse_props(Props,NodeDef,Msg)),

            receive_loop(NodeDef);

        {incoming,_} ->
            io:format("ERROR: inject node should not receive message\n"),
            receive_loop(NodeDef);

        stop ->
            ok
    end.

node_inject(NodeDef) ->
    io:format("inject node init\n"),
    receive_loop(NodeDef).
