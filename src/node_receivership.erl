-module(node_receivership).

-export([enter_receivership/3]).

increment_message_counter(NodeDef, CntName) ->
    {ok, V} = maps:find(CntName, NodeDef),
    maps:put(CntName, V + 1, NodeDef).

%%
%% TODO Not to self: once I get the hang of MACROS replace this receivership
%% TODO stuff with MACROS - this stuff needs to be fast since its called
%% TODO whenever messages are bounced around a flow.
%%
%% TODO2 enter_receivership is bullshit. Because this does not allow
%% TODO2 module nodes to be defined in a single module. What I want to
%% TODO2 do is put all the link nodes (link in, link out and link call) into
%% TODO2 into a single module but because enter_receiveship calls
%% TODO2 "Module, handling_incoming" (for example) there can only be one
%% TODO2 handle_incoming per module ... that defeats the purpose of having
%% TODO2 "module,func" pairs defining nodes. Damn.
%% TODO2 Alternative would be to pass in functions that handle the callbacks
%% TODO2 but is that the best alternative?
%%

%% assert values fails if it never recevied a message
enter_receivership(Module, NodeDef, stop_and_incoming) ->
    receive
        {stop, WsName} ->
            erlang:apply(Module, handle_stop, [NodeDef, WsName]);
        {incoming, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_incoming'),
            NodeDef3 = erlang:apply(Module, handle_incoming, [NodeDef2, Msg]),
            enter_receivership(Module, NodeDef3, stop_and_incoming);
        {outgoing, _Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_outgoing'),
            enter_receivership(Module, NodeDef2, stop_and_incoming)
    end;

%% this is used  by the assert success node, since it does nothing with a
%% message (i.e. it has no output ports), it only needs the stop notification
%% so shortcut the callback stuff.
enter_receivership(Module, NodeDef, only_stop) ->
    receive
        {stop, WsName} ->
            erlang:apply(Module, handle_stop, [NodeDef, WsName]);
        {incoming, _Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_incoming'),
            enter_receivership(Module, NodeDef2, only_stop);
        {outgoing, _Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_outgoing'),
            enter_receivership(Module, NodeDef2, only_stop)
    end;
enter_receivership(Module, NodeDef, websocket_events_and_stop) ->
    receive
        {stop, WsName} ->
            NodeDef2 = erlang:apply(Module, handle_stop, [NodeDef, WsName]),
            {ok, NodePid} = maps:find('_node_pid_', NodeDef2),
            websocket_event_exchange:unsubscribe(WsName, NodePid);
        {ws_event, Details} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_websocket'),
            NodeDef3 = erlang:apply(Module, handle_ws_event, [NodeDef2, Details]),
            enter_receivership(Module, NodeDef3, websocket_events_and_stop)
    end;
enter_receivership(Module, NodeDef, incoming_and_outgoing) ->
    receive
        {stop, _WsName} ->
            ok;
        {incoming, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_incoming'),
            NodeDef3 = erlang:apply(Module, handle_incoming, [NodeDef2, Msg]),
            enter_receivership(Module, NodeDef3, incoming_and_outgoing);
        {outgoing, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_outgoing'),
            NodeDef3 = erlang:apply(Module, handle_outgoing, [NodeDef2, Msg]),
            enter_receivership(Module, NodeDef3, incoming_and_outgoing)
    end;
%%
%% link call nodes need a third type of message and that is the response
%% from a link out node that is in return mode. But a link call node does
%% not require an outgoing message type so ignore that.
enter_receivership(Module, NodeDef, link_call_node) ->
    receive
        {stop, _WsName} ->
            ok;
        {incoming, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_incoming'),
            NodeDef3 = erlang:apply(Module, handle_incoming, [NodeDef2, Msg]),
            enter_receivership(Module, NodeDef3, link_call_node);
        {outgoing, _Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_outgoing'),
            enter_receivership(Module, NodeDef2, link_call_node);
        {link_return, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_link_return'),
            NodeDef3 = erlang:apply(Module, handle_link_return, [NodeDef2, Msg]),
            enter_receivership(Module, NodeDef3, link_call_node)
    end;
enter_receivership(Module, NodeDef, only_incoming) ->
    receive
        {stop, _WsName} ->
            ok;
        {incoming, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_incoming'),
            NodeDef3 = erlang:apply(Module, handle_incoming, [NodeDef2, Msg]),
            enter_receivership(Module, NodeDef3, only_incoming);
        {outgoing, _Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_outgoing'),
            enter_receivership(Module, NodeDef2, only_incoming)
    end;
enter_receivership(Module, NodeDef, Type) ->
    throw(
        io_lib:format(
            "Umatched receivership type '~p' for ~p ~p~n",
            [Type, Module, NodeDef]
        )
    ).
