-module(ered_node_link_call).

-export([node_link_call/2]).
-export([handle_incoming/2]).
-export([handle_link_return/2]).

-import(ered_node_receivership, [enter_receivership/3]).
-import(ered_nodes, [
    generate_id/1,
    jstr/2,
    send_msg_on/2,
    send_msg_to_connected_nodes/2,
    this_should_not_happen/2
]).
-import(nodered, [
    debug/3,
    debug_string/2,
    node_status/5,
    unsupported/3,
    ws_from/1
]).

update_linksource(NodeDef, Msg) ->
    {ok, IdStr} = maps:find(id, NodeDef),
    LinkBack = #{id => generate_id(32), node => IdStr},

    case maps:find('_linkSource', Msg) of
        {ok, Ary} ->
            maps:put('_linkSource', Ary ++ [LinkBack], Msg);
        _ ->
            maps:put('_linkSource', [LinkBack], Msg)
    end.

%%
%% send message to the link in node that this is linked to provided the node
%% isn't in dynamic mode.
handle_incoming(NodeDef, Msg) ->
    case maps:find(linkType, NodeDef) of
        {ok, <<"dynamic">>} ->
            %%
            %% TODO implement this somehow. Two types, by name and by id.
            %% TODO by id is simple, name is harder.
            %%
            unsupported(NodeDef, Msg, jstr("dynamic link calls", []));
        {ok, <<"static">>} ->
            case maps:find(links, NodeDef) of
                {ok, Links} ->
                    send_msg_on(Links, update_linksource(NodeDef, Msg));
                _ ->
                    ignore
            end;
        {ok, LinkType} ->
            ErrMsg = jstr("Unknown LinkType: '~s'", [LinkType]),
            this_should_not_happen(
                NodeDef,
                io_lib:format("~p ~p\n", [ErrMsg, Msg])
            ),
            debug(ws_from(Msg), debug_string(NodeDef, ErrMsg), notice),
            node_status(
                ws_from(Msg), NodeDef, "unknown linkType", "red", "dot"
            );
        _ ->
            ignore
    end,
    NodeDef.

%%
%% This comes from a link out node in return mode, this means we pass
%% the message on to all the nodes connected to us, i.e. the 'wires' attribute.
handle_link_return(NodeDef, Msg) ->
    send_msg_to_connected_nodes(NodeDef, Msg),
    NodeDef.

node_link_call(NodeDef, _WsName) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, link_call_node).
