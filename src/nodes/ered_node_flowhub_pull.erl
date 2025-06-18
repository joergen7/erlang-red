-module(ered_node_flowhub_pull).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% FlowHub node is designed to retrieve flows for usage within a flow.
%%
%% In Erlang-RED, it additionally installs the flow when it receives a message.
%%
%% Also unlike the Node-RED variation, this node does not retrieve content
%% from anywhere else, its only locally (priv/testflows/.../) installed flows
%% that it retrieves - until this comment is removed.
%%

-import(ered_nodes, [
    get_prop_value_from_map/3,
    jstr/1,
    jstr/2,
    post_exception_or_debug/3,
    send_msg_on/2
]).

-import(ered_messages, [
    to_bool/1
]).

-import(ered_nodered_comm, [
    ws_from/1
]).

%%
%%
start(NodeDef, _WsName) ->
    ered_node:start(NodeDef, ?MODULE).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

handle_msg({incoming, Msg}, NodeDef) ->
    % If no flowid is configured, take it from the Msg, if there is none
    % there also, then silently ignore the message.
    FlowId = get_prop_value_from_map(
        <<"flowid">>,
        NodeDef,
        get_prop_value_from_map(<<"flowid">>, Msg, undefined)
    ),

    case FlowId of
        undefined ->
            ErrMsg = jstr("Empty flowids not supported"),
            post_exception_or_debug(NodeDef, Msg, ErrMsg),
            {handled, NodeDef, Msg};
        _ ->
            FileName = io_lib:format(
                "~s/testflows/~s/flows.json",
                [code:priv_dir(erlang_red), FlowId]
            ),
            case file:read_file(FileName) of
                {ok, FileData} ->
                    % node has two ports, therefore wires is
                    %   [ [WiresPort1], [WiresPort2] ]
                    [WiresPort1 | _] = maps:get(<<"wires">>, NodeDef),

                    Msg2 = maps:put(<<"payload">>, FileData, Msg),
                    send_msg_on(WiresPort1, Msg2),

                    % should we install the flow?
                    case to_bool(maps:get(<<"install_flow">>, Msg, false)) of
                        true ->
                            Ary = ered_flows:parse_flow_file(FileName),
                            ered_nodes:create_pid_for_node(Ary, ws_from(Msg));
                        _ ->
                            ignore
                    end,

                    {handled, NodeDef, Msg2};
                _ ->
                    ErrMsg = jstr("Flow id not found: ~p", [FlowId]),
                    post_exception_or_debug(NodeDef, Msg, ErrMsg),
                    {handled, NodeDef, Msg}
            end
    end;
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.
