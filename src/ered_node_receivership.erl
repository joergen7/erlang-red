-module(ered_node_receivership).

-export([enter_receivership/3]).

-import(ered_nodes, [
    post_completed_msg/2,
    this_should_not_happen/2
]).

increment_message_counter(NodeDef, CntName) ->
    {ok, V} = maps:find(CntName, NodeDef),
    maps:put(CntName, V + 1, NodeDef).

%%
%% this is called if a message type was received by a node that did not
%% claim to receive such messages
bad_routing(NodeDef, Type, Msg) ->
    this_should_not_happen(
        NodeDef,
        io_lib:format(
            "Node received unhandled type ~p Node: ~p Msg: ~p\n",
            [Type, NodeDef, Msg]
        )
    ).

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

%% used by the complete node to receive messages from nodes that have
%% completed. Ensure that it doesn't receive its own messages thus creating
%% and endless loop.
enter_receivership(Module, NodeDef, completed_messages) ->
    receive
        {stop, _WsName} ->
            ok;
        {completed_msg, FromNodeDef, Msg} ->
            {NodeDef2, Msg2} = erlang:apply(
                Module,
                handle_completed_msg,
                [NodeDef, FromNodeDef, Msg]
            ),
            %% completed node marks the message as having been seen - to
            %% avoid endless loops - so therefore send the modified message
            %% here.
            %%
            %% Completed Nodes can also be listened to by a complete
            %% node, hence this post_completed_msg is here.
            post_completed_msg(NodeDef2, Msg2),
            enter_receivership(Module, NodeDef2, completed_messages)
    end;
%% used by the ignore node, this is really a zombie node.
enter_receivership(Module, NodeDef, nothing) ->
    receive
        {stop, _WsName} ->
            ok;
        {incoming, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_incoming'),
            bad_routing(NodeDef2, incoming, Msg),
            enter_receivership(Module, NodeDef2, nothing);
        {outgoing, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_outgoing'),
            bad_routing(NodeDef2, outgoing, Msg),
            enter_receivership(Module, NodeDef2, nothing)
    end;
%% used by catch nodes to receive exceptions from other nodes.
enter_receivership(Module, NodeDef, only_exception) ->
    receive
        {stop, _WsName} ->
            ok;
        {exception, From, Msg, ErrMsg} ->
            %% From is the NodeDef of the source node and Msg is the
            %% Msg object it received. ErrMsg is a string explaining the
            %% situation.
            NodeDef2 = increment_message_counter(NodeDef, '_mc_exception'),
            {NodeDef3, Msg2} = erlang:apply(
                Module,
                handle_exception,
                [NodeDef2, From, Msg, ErrMsg]
            ),
            post_completed_msg(NodeDef3, Msg2),
            enter_receivership(Module, NodeDef3, only_exception);
        {incoming, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_incoming'),
            bad_routing(NodeDef2, incoming, Msg),
            enter_receivership(Module, NodeDef2, only_exception);
        {outgoing, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_outgoing'),
            bad_routing(NodeDef2, outgoing, Msg),
            enter_receivership(Module, NodeDef2, only_exception)
    end;
%% the disabled and enabled messages are specifically for the debug node.
%% Active here is an attribute on the NodeDef that is used to turn the
%% debug node on and off. But it can also be used for other nodes if
%% necessary.
enter_receivership(Module, NodeDef, only_incoming_with_active) ->
    receive
        {stop, _WsName} ->
            ok;
        {disable, _WsName} ->
            NodeDef2 = maps:put(active, false, NodeDef),
            enter_receivership(Module, NodeDef2, only_incoming_with_active);
        {enable, _WsName} ->
            NodeDef2 = maps:put(active, true, NodeDef),
            enter_receivership(Module, NodeDef2, only_incoming_with_active);
        {incoming, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_incoming'),
            {NodeDef3, Msg2} = erlang:apply(
                Module, handle_incoming, [NodeDef2, Msg]
            ),
            post_completed_msg(NodeDef3, Msg2),
            enter_receivership(Module, NodeDef3, only_incoming_with_active);
        {outgoing, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_outgoing'),
            bad_routing(NodeDef2, outgoing, Msg),
            enter_receivership(Module, NodeDef2, only_incoming_with_active)
    end;
%% assert values fails if it never recevied a message
enter_receivership(Module, NodeDef, stop_and_incoming) ->
    receive
        {stop, WsName} ->
            erlang:apply(Module, handle_stop, [NodeDef, WsName]),
            ok;
        {incoming, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_incoming'),
            {NodeDef3, Msg2} = erlang:apply(
                Module, handle_incoming, [NodeDef2, Msg]
            ),
            post_completed_msg(NodeDef3, Msg2),
            enter_receivership(Module, NodeDef3, stop_and_incoming);
        {outgoing, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_outgoing'),
            bad_routing(NodeDef2, outgoing, Msg),
            enter_receivership(Module, NodeDef2, stop_and_incoming)
    end;
%% this is used  by the assert success node, since it does nothing with a
%% message (i.e. it has no output ports), it only needs the stop notification
%% so shortcut the callback stuff.
enter_receivership(Module, NodeDef, only_stop) ->
    receive
        {stop, WsName} ->
            erlang:apply(Module, handle_stop, [NodeDef, WsName]),
            ok;
        {incoming, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_incoming'),
            bad_routing(NodeDef2, incoming, Msg),
            enter_receivership(Module, NodeDef2, only_stop);
        {outgoing, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_outgoing'),
            bad_routing(NodeDef2, outgoing, Msg),
            enter_receivership(Module, NodeDef2, only_stop)
    end;
enter_receivership(Module, NodeDef, websocket_events_and_stop) ->
    receive
        {stop, WsName} ->
            NodeDef2 = erlang:apply(Module, handle_stop, [NodeDef, WsName]),
            {ok, NodePid} = maps:find('_node_pid_', NodeDef2),
            ered_ws_event_exchange:unsubscribe(WsName, NodePid),
            ok;
        {ws_event, Details} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_websocket'),
            NodeDef3 = erlang:apply(
                Module,
                handle_ws_event,
                [NodeDef2, Details]
            ),
            enter_receivership(Module, NodeDef3, websocket_events_and_stop)
    end;
enter_receivership(Module, NodeDef, incoming_and_outgoing) ->
    receive
        {stop, _WsName} ->
            ok;
        {incoming, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_incoming'),
            {NodeDef3, Msg2} = erlang:apply(
                Module, handle_incoming, [NodeDef2, Msg]
            ),
            post_completed_msg(NodeDef3, Msg2),
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
            {NodeDef3, Msg2} = erlang:apply(
                Module, handle_incoming, [NodeDef2, Msg]
            ),
            %% in fact this will return a msg object to ensure nothing
            %% is posted - this is caught by the complete node.
            post_completed_msg(NodeDef3, Msg2),
            enter_receivership(Module, NodeDef3, link_call_node);
        {outgoing, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_outgoing'),
            bad_routing(NodeDef2, outgoing, Msg),
            enter_receivership(Module, NodeDef2, link_call_node);
        {link_return, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_link_return'),
            {NodeDef3, Msg2} = erlang:apply(
                Module, handle_link_return, [NodeDef2, Msg]
            ),
            post_completed_msg(NodeDef3, Msg2),
            enter_receivership(Module, NodeDef3, link_call_node)
    end;
%%
%% incoming only nodes are things such debug or data stores node that receives
%% messages and store them. These nodes don't perform any computation on the
%% messages.
enter_receivership(Module, NodeDef, only_incoming) ->
    receive
        {stop, _WsName} ->
            ok;
        {incoming, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_incoming'),
            {NodeDef3, Msg2} = erlang:apply(
                Module, handle_incoming, [NodeDef2, Msg]
            ),
            post_completed_msg(NodeDef3, Msg2),
            enter_receivership(Module, NodeDef3, only_incoming);
        {outgoing, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_outgoing'),
            bad_routing(NodeDef2, outgoing, Msg),
            enter_receivership(Module, NodeDef2, only_incoming)
    end;
%%
%% outgoing only nodes are things like inject, http in and mqtt in - these
%% nodes are sources of messages but never receive these within the flow
%% rather they receive data from a third-party source and pass that data
%% into the flow.
enter_receivership(Module, NodeDef, only_outgoing) ->
    receive
        {stop, _WsName} ->
            ok;
        {incoming, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_incoming'),
            bad_routing(NodeDef2, incoming, Msg),
            enter_receivership(Module, NodeDef2, only_outgoing);
        {outgoing, Msg} ->
            NodeDef2 = increment_message_counter(NodeDef, '_mc_outgoing'),
            {NodeDef3, Msg2} = erlang:apply(
                Module, handle_outgoing, [NodeDef2, Msg]
            ),
            post_completed_msg(NodeDef3, Msg2),
            enter_receivership(Module, NodeDef3, only_outgoing)
    end;
enter_receivership(Module, NodeDef, Type) ->
    throw(
        io_lib:format(
            "Umatched receivership type '~p' for ~p ~p~n",
            [Type, Module, NodeDef]
        )
    ).
