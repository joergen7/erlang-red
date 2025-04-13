-module(node_link_call).

-export([node_link_call/1]).
-export([handle_incoming/2]).
-export([handle_link_return/2]).

-import(node_receivership, [enter_receivership/3]).

update_linksource(NodeDef,Msg) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    LinkBack = #{ id => nodes:generate_id(32), node => IdStr },

    case maps:find('_linkSource',Msg) of
        {ok, Ary} ->
            maps:put('_linkSource', Ary ++ [LinkBack], Msg);
         _ ->
            maps:put('_linkSource',[LinkBack],Msg)
    end.


%%
%% send message to the link in node that this is linked to provided the node
%% isn't in dynamic mode.
handle_incoming(NodeDef,Msg) ->
    case maps:find(linkType,NodeDef) of
        {ok, <<"dynamic">>} ->
            %%
            %% TODO implement this somehow.
            %%
            ignore;

        {ok, <<"static">>} ->
            case maps:find(links,NodeDef) of
                {ok, Links} ->
                    nodes:send_msg_on(Links,update_linksource(NodeDef,Msg));
                _ ->
                    ignore
            end;

        {ok, LinkType} ->
            ErrMsg = nodes:jstr("Unknown LinkType: '~s'",[LinkType]),
            nodes:this_should_not_happen(
              NodeDef,
              io_lib:format("~p ~p\n",[ErrMsg,Msg])
            ),
            nodered:debug(nodered:ws(Msg),
                          nodered:debug_string(NodeDef, ErrMsg), notice),
            nodered:node_status(nodered:ws(Msg),
                                NodeDef, "unknown linkType", "red", "dot");
        _ ->
            ignore
    end,
    NodeDef.

%%
%% This comes from a link out node in return mode, this means we pass
%% the message on to all the nodes connected to us, i.e. the 'wires' attribute.
handle_link_return(NodeDef,Msg) ->
    nodes:send_msg_to_connected_nodes(NodeDef,Msg),
    NodeDef.


node_link_call(NodeDef) ->
    nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, link_call_node).
