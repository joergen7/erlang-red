-module(ered_node_mqtt_out).

-behaviour(ered_node).

-include("ered_nodes.hrl").
-include("ered_mqtt.hrl").

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% MQTT Out node for feeding messages to an MQTT broker somewhere.
%%
%% This is very much a copy of the MQTT in node, see it for more details
%% on the configuration options.
%%

%%
%% See the mqtt in node for the logic behind this code.

%%
%% TODO needs to be refactored as the mqtt in node - much can be done
%% TODO via function signature pattern matching.
%%

-import(ered_config_store, [
    retrieve_config_node/1
]).
-import(ered_nodered_comm, [
    node_status/5,
    post_exception_or_debug/3,
    ws_from/1
]).
-import(ered_messages, [
    convert_to_num/1,
    to_bool/1
]).
-import(ered_nodes, [
    jstr/1
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event({registered, WsName, _NodePid}, NodeDef) ->
    setup_mqtt_manager(NodeDef, WsName);
%%
%% mqtt manager pid is available
handle_event(
    {mqtt_disconnected, _Reason, _Properties},
    #{?MQTT_MGR_PID} = NodeDef
) ->
    case is_process_alive(MqttMgrPid) of
        true ->
            TRef = erlang:start_timer(
                750,
                self(),
                {connect_to_broker, MqttMgrPid}
            ),
            maps:put('_timer', TRef, NodeDef);
        _ ->
            setup_mqtt_manager(NodeDef, ws_from(NodeDef))
    end;
%%
%% No mqtt manager pid
handle_event(
    {mqtt_disconnected, _Reason, _Properties},
    NodeDef
) ->
    setup_mqtt_manager(NodeDef, ws_from(NodeDef));
%%
%%
handle_event({connect_to_broker, MqttMgrPid}, NodeDef) ->
    WsName = ws_from(NodeDef),

    case is_process_alive(MqttMgrPid) of
        true ->
            case gen_server:call(MqttMgrPid, start_mqtt) of
                ok ->
                    try
                        case gen_server:call(MqttMgrPid, connect) of
                            {ok, _Props} ->
                                ?STATUS("connected", "green", "dot"),
                                maps:remove('_timer', NodeDef);
                            _ ->
                                TRef = erlang:start_timer(
                                    750,
                                    self(),
                                    {connect_to_broker, MqttMgrPid}
                                ),
                                NodeDef#{'_timer' => TRef}
                        end
                    catch
                        exit:_ ->
                            %% this exit comes from the eqmtt library and its
                            %% taken our mqtt manager with it :( But this
                            %% exception represents a missing broker - so what!
                            %% We want to keep trying so we have to recreate
                            %% the manager.
                            %% What we do is capture the DOWN event of the
                            %% manager, restart it and then add a another timer.
                            ?STATUS("connecting", "yellow", "dot"),
                            NodeDef
                    end;
                _ ->
                    TRef = erlang:start_timer(
                        750,
                        self(),
                        {connect_to_broker, MqttMgrPid}
                    ),
                    ?STATUS("connecting", "yellow", "dot"),
                    NodeDef#{'_timer' => TRef}
            end;
        _ ->
            NodeDef
    end;
%%
%%
handle_event({'DOWN', _MonitorRef, _Type, _Object, _Info}, NodeDef) ->
    setup_mqtt_manager(NodeDef, ws_from(NodeDef));
%%
%% stop event - with timer or without, with mqtt mgr process or not.
handle_event(?MSG_STOP, #{?TIMER, ?MQTT_MGR_PID} = NodeDef) ->
    erlang:cancel_timer(TRef),
    gen_server:cast(MqttMgrPid, stop),
    NodeDef;
handle_event(?MSG_STOP, #{?TIMER} = NodeDef) ->
    erlang:cancel_timer(TRef),
    NodeDef;
handle_event(?MSG_STOP, #{?MQTT_MGR_PID} = NodeDef) ->
    gen_server:cast(MqttMgrPid, stop),
    NodeDef;
%%
%% fall through
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
%% MQTT manager process id is available
handle_msg(
    {incoming,
        #{
            <<"topic">> := MsgTopic,
            <<"payload">> := Payload
        } = Msg},
    #{
        <<"topic">> := NodeTopic,
        <<"qos">> := QoS,
        <<"retain">> := Retain,
        ?MQTT_MGR_PID
    } = NodeDef
) when NodeTopic == <<>>, MsgTopic =/= <<>> ->
    Data = {
        publish_payload,
        Payload,
        MsgTopic,
        convert_to_num(QoS),
        to_bool(Retain)
    },

    gen_server:cast(MqttMgrPid, Data),

    update_status(
        is_process_alive(MqttMgrPid), NodeDef, "connecting", "yellow", "dot"
    ),

    {handled, NodeDef, Msg};
handle_msg(
    {incoming,
        #{
            <<"payload">> := Payload
        } = Msg},
    #{
        <<"topic">> := NodeTopic,
        <<"qos">> := QoS,
        <<"retain">> := Retain,
        ?MQTT_MGR_PID
    } = NodeDef
) when NodeTopic =/= <<>> ->
    Data = {
        publish_payload,
        Payload,
        NodeTopic,
        convert_to_num(QoS),
        to_bool(Retain)
    },

    gen_server:cast(MqttMgrPid, Data),

    update_status(
        is_process_alive(MqttMgrPid), NodeDef, "connecting", "yellow", "dot"
    ),

    {handled, NodeDef, Msg};
%%
%% No MQTT manager process id
handle_msg(
    ?MSG_INCOMING,
    NodeDef
) ->
    ErrMsg = jstr("no connection available"),
    post_exception_or_debug(NodeDef, Msg, ErrMsg),
    {handled, NodeDef, dont_send_complete_msg};
handle_msg({mqtt_not_sent, Msg}, NodeDef) ->
    %% TODO these are messages that were sent out using the publish_payload
    %% TODO but because the broker went away, the message has come back
    %% TODO so it needs to be pushed further up the chain.
    io:format("MQTT out, message was not sent ~p~n", [Msg]),
    {handled, NodeDef, dont_send_complete_msg};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%% -------------- Helpers
%%

update_status(true, NodeDef, Txt, Clr, Shp) ->
    node_status(ws_from(NodeDef), NodeDef, Txt, Clr, Shp);
update_status(_, _, _, _, _) ->
    ignore.

%% erlfmt:ignore alignment
create_mqtt_manager(Cfg) ->
    Options = [
        {host,        maps:get(<<"broker">>,              Cfg)},
        {port,        convert_to_num(maps:get(<<"port">>,  Cfg))},
        {ssl,         maps:get(<<"usetls">>,                 Cfg)},
        {clean_start, maps:get(<<"cleansession">>,            Cfg)},
        {proto_ver,   maps:get(<<"protocolVersion">>,          Cfg)},
        {keepalive,   convert_to_num(maps:get(<<"keepalive">>,  Cfg))},
        {will_topic,  maps:get(<<"willTopic">>,                 Cfg)},
        {will_qos,    convert_to_num(maps:get(<<"willQos">>,  Cfg))},
        {will_retain, to_bool(maps:get(<<"willRetain">>,     Cfg))},
        {will_props,  maps:get(<<"willMsg">>,               Cfg)},
        {force_ping,  true}
        %% TODO respect the client id but we don't
        %% {clientid, maps:get(<<"clientid">>, Cfg)},
        %% {will_payload, maps:get(<<"willPayload">>, Cfg)},
        %% {properties, maps:get(<<"userProps">>, Cfg)}
    ],

    {ok, MqttMgrPid} = ered_mqtt_manager:start(self(), Options),

    MqttMgrPid.

%%
%%
setup_mqtt_manager(
    #{<<"broker">> := CfgNodeId} = NodeDef, WsName
) ->
    case retrieve_config_node(CfgNodeId) of
        {ok, Cfg} ->
            ?STATUS("connecting", "yellow", "dot"),

            MqttMgrPid = create_mqtt_manager(Cfg),

            erlang:monitor(process, MqttMgrPid),

            TRef = erlang:start_timer(
                750,
                self(),
                {connect_to_broker, MqttMgrPid}
            ),

            ?PUT_WS(NodeDef#{
                '_timer' => TRef,
                '_mqtt_mgr_id' => MqttMgrPid
            });
        _ ->
            ?STATUS("connecting (no cfg)", "yellow", "dot"),
            NodeDef
    end;
setup_mqtt_manager(
    NodeDef, WsName
) ->
    ?STATUS("connecting (no broker)", "yellow", "dot"),
    NodeDef.
