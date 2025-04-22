-module(ered_node_debug).

-export([node_debug/2]).
-export([handle_event/2]).
-export([handle_incoming/2]).

-import(ered_node_receivership, [enter_receivership/3]).

-import(ered_nodered_comm, [
    debug/3,
    node_status/5,
    unsupported/3,
    ws_from/1
]).
-import(ered_nodes, [
    get_prop_value_from_map/2,
    get_prop_value_from_map/3,
    jstr/2
]).

-import(ered_msg_handling, [
    retrieve_prop_value/2
]).

%%
%% Debug nodes have no outgoing wires.
%%

to_binary_if_not_binary(Obj) when is_binary(Obj) ->
    Obj;
to_binary_if_not_binary(Obj) when is_list(Obj) ->
    list_to_binary(Obj);
to_binary_if_not_binary(Obj) ->
    Obj.

%%
%%
%% erlfmt:ignore equals and arrows should line up here.
send_to_sidebar(NodeDef,Msg) ->
    Type     = get_prop_value_from_map(type,  NodeDef),
    IdStr    = get_prop_value_from_map(id,    NodeDef),
    ZStr     = get_prop_value_from_map(z,     NodeDef),
    NameStr  = get_prop_value_from_map(name,  NodeDef, Type),
    TopicStr = get_prop_value_from_map(topic, Msg,     ""),

    %% format is important here.
    %% Triggery for large files and I don't know what. Using format
    %% of "object" as opposed to "Object" (capital-o) causes less
    %% breakage. Definitely something to investigate.
    %% See info for test id: c4690c0a085d6ef5 for more details.
    Data = #{
             id       => IdStr,
             z        => ZStr,
             path     => ZStr,
             name     => NameStr,
             topic    => to_binary_if_not_binary(TopicStr),
             msg      => Msg,
             format   => <<"object">>
            },

    debug(ws_from(Msg), Data, normal).

%%
%%
handle_status_setting({ok, true}, {ok, <<"msg">>}, NodeDef, Msg) ->
    {ok, PropName} = maps:find(statusVal, NodeDef),
    Val = retrieve_prop_value(PropName, Msg),
    node_status(ws_from(Msg), NodeDef, Val, "grey", "dot");
handle_status_setting({ok, true}, {ok, <<"counter">>}, NodeDef, Msg) ->
    Cnt = get_prop_value_from_map('_mc_incoming', NodeDef),
    node_status(ws_from(Msg), NodeDef, Cnt, "blue", "ring");
handle_status_setting({ok, false}, _, _, _) ->
    ok;
handle_status_setting({ok, true}, {ok, StatusType}, NodeDef, Msg) ->
    unsupported(NodeDef, Msg, jstr("StatusType: ~p", [StatusType])).

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_incoming(NodeDef, Msg) ->
    case maps:find(console, NodeDef) of
        {ok, true} ->
            NodeName = get_prop_value_from_map(
                name,
                NodeDef,
                "undefined"
            ),
            io:format("DEBUG [~s]: ~p\n", [NodeName, Msg]);
        _ ->
            ignore
    end,

    case maps:find(tosidebar, NodeDef) of
        {ok, true} ->
            case maps:find(active, NodeDef) of
                {ok, true} ->
                    send_to_sidebar(NodeDef, Msg);
                _ ->
                    not_active_no_output
            end;
        _ ->
            not_to_sidebar
    end,

    handle_status_setting(
        maps:find(tostatus, NodeDef),
        maps:find(statusType, NodeDef),
        NodeDef,
        Msg
    ),

    {NodeDef, Msg}.

node_debug(NodeDef, _WsName) ->
    enter_receivership(?MODULE, NodeDef, only_incoming_with_active).
