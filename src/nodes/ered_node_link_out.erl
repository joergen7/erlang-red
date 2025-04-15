-module(ered_node_link_out).

-export([node_link_out/1]).
-export([handle_incoming/2]).

-import(node_receivership, [enter_receivership/3]).

send_to_link_call({ok, NodeId}, Msg) ->
    NodePid = ered_nodes:nodeid_to_pid(nodered:ws(Msg), NodeId),
    case whereis(NodePid) of
        undefined ->
            ok;
        _ ->
            NodePid ! {link_return, Msg}
    end.

last_value([], _) ->
    empty;
last_value([H | []], Rest) ->
    {ok, H, lists:reverse(Rest)};
last_value([H | Ary], Rest) ->
    last_value(Ary, [H | Rest]).

handle_incoming(NodeDef, Msg) ->
    case maps:find(mode, NodeDef) of
        {ok, <<"link">>} ->
            case maps:find(links, NodeDef) of
                {ok, Links} ->
                    %% this are all link in nodes and they have no incoming
                    %% wires so we can send them their messages using the
                    %% "incoming" message type this is what send_msg_on does.
                    ered_nodes:send_msg_on(Links, Msg);
                _ ->
                    ignore
            end;
        {ok, <<"return">>} ->
            %% careful, what this does is take the last entry and respond
            %% to that. link calls can be nested hence this logic
            case maps:find('_linkSource', Msg) of
                {ok, Ary} ->
                    case last_value(Ary, []) of
                        empty ->
                            ignore;
                        {ok, LinkBack, NewAry} ->
                            send_to_link_call(
                                maps:find(node, LinkBack),
                                maps:put('_linkSource', NewAry, Msg)
                            )
                    end;
                _ ->
                    ignore
            end;
        {ok, Mode} ->
            ErrMsg = ered_nodes:jstr("Unknown Mode: '~s'", [Mode]),
            ered_nodes:this_should_not_happen(
                NodeDef,
                io_lib:format("~p ~p\n", [ErrMsg, Msg])
            ),

            nodered:debug(
                nodered:ws(Msg),
                nodered:debug_string(NodeDef, ErrMsg),
                notice
            ),

            nodered:node_status(nodered:ws(Msg), NodeDef, ErrMsg, "red", "dot");
        _ ->
            ignore
    end,
    NodeDef.

node_link_out(NodeDef) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, only_incoming).
