-module(ered_node_mqtt_in).

-behaviour(ered_node).

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

-import(ered_config_store, [
    retrieve_config_node/1
]).
-import(ered_nodered_comm, [
    node_status/5,
    ws_from/1
]).
-import(ered_msg_handling, [
    create_outgoing_msg/1,
    convert_to_num/1
]).
-import(ered_nodes, [
    send_msg_to_connected_nodes/2
]).

-define(CONNERR(EM), node_status(WsName, NodeDef, EM, "red", "dot"), NodeDef).
-define(STATUS(EM, CLR, SHP), node_status(WsName, NodeDef, EM, CLR, SHP)).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event({registered, WsName, Pid}, NodeDef) ->
    case maps:find(broker, NodeDef) of
        {ok, CfgNodeId} ->
            case retrieve_config_node(CfgNodeId) of
                {ok, Cfg} ->
                    Options = [
                        {host, maps:get(broker, Cfg)},
                        {port, maps:get(port, Cfg)},
                        %% TODO respect the client id but we don't
                        %% {clientid, maps:get(clientid, Cfg)},
                        {ssl, maps:get(usetls, Cfg)},
                        {clean_start, maps:get(cleansession, Cfg)},
                        {proto_ver, maps:get(protocolVersion, Cfg)},
                        {keepalive, maps:get(keepalive, Cfg)},
                        {will_topic, maps:get(willTopic, Cfg)},
                        {will_qos, convert_to_num(maps:get(willQos, Cfg))},
                        {will_retain, to_bool(maps:get(willRetain, Cfg))},
                        {will_props, maps:get(willMsg, Cfg)},
                        {force_ping, true}
                        %% {will_payload, maps:get(willPayload, Cfg)},
                        %% {properties, maps:get(userProps, Cfg)}
                    ],

                    ?STATUS("connecting", "yellow", "dot"),

                    {ok, MqttPid} = ered_mqtt_manager:start(Pid, Options),

                    case gen_server:call(MqttPid, start_mqtt) of
                        ok ->
                            ?STATUS("connecting", "yellow", "ring"),
                            try
                                case gen_server:call(MqttPid, connect) of
                                    {ok, _Props} ->
                                        gen_server:call(
                                            MqttPid,
                                            {subscribe, #{}, [
                                                {maps:get(topic, NodeDef), [
                                                    {qos, 1}
                                                ]}
                                            ]}
                                        ),

                                        ?STATUS("connected", "green", "dot"),

                                        add_to_nodedef(
                                            NodeDef, MqttPid, WsName
                                        );
                                    _ ->
                                        ?CONNERR("failed to connect")
                                end
                            catch
                                exit:_ ->
                                    ?CONNERR("connection refused")
                            end;
                        _ ->
                            ?CONNERR("no broker")
                    end;
                _ ->
                    ?CONNERR("no config node found")
            end;
        _ ->
            ?CONNERR("no config node defined")
    end;
handle_event({mqtt_disconnected, _Reason, _Properties}, NodeDef) ->
    node_status(ws_from(NodeDef), NodeDef, "disconnected", "red", "ring"),
    NodeDef;
handle_event({stop, _WsName}, NodeDef) ->
    %% TODO here we need to do some cleanup from MQTT broker.
    NodeDef;
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
%%
to_bool(<<"">>) -> false;
to_bool("") -> false;
to_bool(<<"false">>) -> false;
to_bool(false) -> false;
to_bool("false") -> false;
to_bool(_) -> true.

copy_attributes([], Msg, _MqttDataPacket) ->
    Msg;
copy_attributes([Attr | Attrs], Msg, MqttDataPacket) ->
    copy_attributes(
        Attrs,
        maps:put(Attr, maps:get(Attr, MqttDataPacket), Msg),
        MqttDataPacket
    ).

add_to_nodedef(NodeDef, EmqttPid, WsName) ->
    maps:put(emqtt_client_id, EmqttPid, maps:put('_ws', WsName, NodeDef)).
