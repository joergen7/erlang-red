-module(ered_node_mqtt_in).

-behaviour(ered_node).

-include("ered_nodes.hrl").

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% MQTT In node, a node that connects to a MQTT broker and streams all messags
%% into the flow it is embedded in.
%%
%% An MQTT node is configured by a Config Node with the following properties:
%%
%% id => <<"97960d98b2837fa0">>,
%% name => <<"asdasdasd">>,
%% port => 1883,
%% type => <<"mqtt-broker">>,
%% keepalive => 60,
%% credentials => #{user => <<>>,password => <<>>},
%% broker => <<"renderbox">>,
%% userProps => <<>>,
%% clientid => <<>>,
%% autoConnect => true,
%% usetls => false,
%% protocolVersion => 4,
%% cleansession => true,
%% autoUnsubscribe => true,
%% birthTopic => <<>>,
%% birthQos => <<"0">>,
%% birthRetain => <<"false">>,
%% birthPayload => <<>>,
%% birthMsg => #{},
%% closeTopic => <<>>,
%% closeQos => <<"0">>,
%% closeRetain => <<"false">>,
%% closePayload => <<>>,
%% closeMsg => #{},
%% willTopic => <<>>,
%% willQos => <<"0">>,
%% willRetain => <<"false">>,
%% willPayload => <<>>,
%% willMsg => #{},
%% sessionExpiry => <<>>

%%
%% Reconnect strategy is to have the MQTT Manager take the hit - when the
%% eqmtt goes down because there isn't a broker available, it takes the manager
%% with it but leaves this node to recover everything. It does this by
%% capturing the DOWN message from the MQTT Manager and restarting it.
%%
%% Connecting to a broker is done via a timer that is constantly being
%% renewed and cycling through the Erlang processes. When the emqtt goes down
%% because an MQTT broker isn't available, it's recreated by this timer
%% or a DOWN event. Endlessly looping around until finally a connection
%% to the broker is made.
%%

-import(ered_config_store, [
    retrieve_config_node/1
]).
-import(ered_nodered_comm, [
    node_status/5,
    ws_from/1
]).
-import(ered_messages, [
    create_outgoing_msg/1,
    convert_to_num/1,
    to_bool/1
]).
-import(ered_nodes, [
    send_msg_to_connected_nodes/2
]).

-define(STATUS(EM, CLR, SHP), node_status(WsName, NodeDef, EM, CLR, SHP)).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event({registered, WsName, _Pid}, NodeDef) ->
    setup_mqtt_manager(NodeDef, WsName);
handle_event({'DOWN', _MonitorRef, _Type, _Object, _Info}, NodeDef) ->
    setup_mqtt_manager(NodeDef, ws_from(NodeDef));

%%
%% mqtt_disconnected event, with and without a running mqtt process
handle_event(
  {mqtt_disconnected, _Reason, _Properties},
  #{ '_mqtt_mgr_id' := MqttMgrPid } = NodeDef
) ->
    case is_process_alive(MqttMgrPid) of
        true ->
            TRef = erlang:start_timer(
                     750,
                     self(),
                     {connect_to_broker, MqttMgrPid}
            ),
            NodeDef#{'_timer' => TRef};
        _ ->
            setup_mqtt_manager(NodeDef, ws_from(NodeDef))
    end;
handle_event(
  {mqtt_disconnected, _Reason, _Properties},
  NodeDef
) ->
    setup_mqtt_manager(NodeDef, ws_from(NodeDef));
%%
handle_event(
  {connect_to_broker, MqttMgrPid},
  #{ '_ws' := WsName } = NodeDef
) ->
    case is_process_alive(MqttMgrPid) of
        true ->
            case gen_server:call(MqttMgrPid, start_mqtt) of
                ok ->
                    try
                        case gen_server:call(MqttMgrPid, connect) of
                            {ok, _Props} ->
                                %% in the wild, this subscribe call will
                                %% probably fail - f2k it. This code is
                                %% indented far enough.
                                gen_server:call(
                                    MqttMgrPid,
                                    {subscribe, #{}, [
                                        {maps:get(<<"topic">>, NodeDef), [
                                            {qos, 1}
                                        ]}
                                    ]}
                                ),

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
%% stop event - with timer or without, with mqtt mgr process or not.
handle_event(
  {stop, _WsName},
  #{'_timer' := TRef, '_mqtt_mgr_id' := MqttMgrPid} = NodeDef
) ->
    erlang:cancel_timer(TRef),
    gen_server:cast(MqttMgrPid, stop),
    NodeDef;
handle_event({stop, _WsName}, #{'_timer' := TRef} = NodeDef) ->
    erlang:cancel_timer(TRef),
    NodeDef;
handle_event({stop, _WsName}, #{'_mqtt_mgr_id' := MqttMgrPid} = NodeDef) ->
    gen_server:cast(MqttMgrPid, stop),
    NodeDef;
%% fall through
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%

handle_msg({mqtt_incoming, MqttDataPacket}, NodeDef) ->
    {outgoing, Msg} = create_outgoing_msg(ws_from(NodeDef)),
    Msg2 = copy_attributes([payload, topic, retain, qos], Msg, MqttDataPacket),
    send_msg_to_connected_nodes(NodeDef, Msg2),
    {handled, NodeDef, Msg2};
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%% -------------- Helpers
%%

copy_attributes([], Msg, _MqttDataPacket) ->
    Msg;
copy_attributes([Attr | Attrs], Msg, MqttDataPacket) ->
    copy_attributes(
        Attrs,
        maps:put(atom_to_binary(Attr), maps:get(Attr, MqttDataPacket), Msg),
        MqttDataPacket
    ).

%% erlfmt:ignore alignment
create_mqtt_manager(Cfg) ->
    Options = [
        {host,        maps:get(<<"broker">>,               Cfg)},
        {port,        maps:get(<<"port">>,              Cfg)},
        {ssl,         maps:get(<<"usetls">>,          Cfg)},
        {clean_start, maps:get(<<"cleansession">>,    Cfg)},
        {proto_ver,   maps:get(<<"protocolVersion">>,   Cfg)},
        {keepalive,   maps:get(<<"keepalive">>,            Cfg)},
        {will_topic,  maps:get(<<"willTopic">>,              Cfg)},
        {will_qos,    convert_to_num(maps:get(<<"willQos">>,  Cfg))},
        {will_retain, to_bool(maps:get(<<"willRetain">>,       Cfg))},
        {will_props,  maps:get(<<"willMsg">>,                 Cfg)},
        {force_ping,  true}
        %% TODO respect the client id but we don't
        %% {clientid, maps:get(<<"clientid">>, Cfg)},
        %% {will_payload, maps:get(<<"willPayload">>, Cfg)},
        %% {properties, maps:get(<<"userProps">>, Cfg)}
    ],

    {ok, MqttMgrPid} = ered_mqtt_manager:start(self(), Options),

    MqttMgrPid.

setup_mqtt_manager(NodeDef, WsName) ->
    case maps:find(<<"broker">>, NodeDef) of
        {ok, CfgNodeId} ->
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
        _ ->
            ?STATUS("connecting (no broker)", "yellow", "dot"),
            NodeDef
    end.
