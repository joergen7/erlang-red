-module(ered_node_mqtt_out).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% MQTT Out node for feeding messages to an MQTT broker somewhere.
%%
%% This is very much a copy of the MQTT in node, see it for more details
%% on the configuration options.
%%

-import(ered_config_store, [
    retrieve_config_node/1
]).
-import(ered_nodered_comm, [
    node_status/5,
    ws_from/1
]).
-import(ered_msg_handling, [
    convert_to_num/1,
    to_bool/1
]).
-import(ered_nodes, [
    jstr/1,
    post_exception_or_debug/3
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
    NodeDef;
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_msg({incoming, Msg}, NodeDef) ->
    case maps:find(emqtt_client_id, NodeDef) of
        {ok, Pid} ->
            {ok, Topic} = maps:find(topic, NodeDef),
            {ok, QoS} = maps:find(qos, NodeDef),
            {ok, Retain} = maps:find(retain, NodeDef),

            Data = {
                publish_payload,
                maps:get(payload, Msg),
                Topic,
                convert_to_num(QoS),
                to_bool(Retain)
            },

            gen_server:cast(Pid, Data),

            case erlang:process_info(Pid) of
                undefined ->
                    node_status(
                        ws_from(NodeDef), NodeDef, "disconnected", "red", "dot"
                    );
                _ ->
                    ignore
            end,
            {handled, NodeDef, Msg};
        _ ->
            ErrMsg = jstr("no connection available"),
            post_exception_or_debug(NodeDef, Msg, ErrMsg),
            {handled, NodeDef, dont_send_complete_msg}
    end;
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%

add_to_nodedef(NodeDef, EmqttPid, WsName) ->
    maps:put(emqtt_client_id, EmqttPid, maps:put('_ws', WsName, NodeDef)).
