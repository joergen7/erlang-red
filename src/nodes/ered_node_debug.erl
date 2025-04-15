-module(ered_node_debug).

-export([node_debug/1]).
-export([handle_incoming/2]).

-import(node_receivership, [enter_receivership/3]).

%%
%% Debug nodes have no outgoing wires.
%%

to_binary_if_not_binary(Obj) when is_binary(Obj) ->
    Obj;
to_binary_if_not_binary(Obj) when is_list(Obj) ->
    list_to_binary(Obj);
to_binary_if_not_binary(Obj) ->
    Obj.

handle_status_setting({ok, true}, {ok, <<"counter">>}, NodeDef, Msg) ->
    Cnt = ered_nodes:get_prop_value_from_map('_mc_incoming', NodeDef),

    nodered:node_status(
        nodered:ws(Msg),
        NodeDef,
        io_lib:format("~p", [Cnt]),
        "blue",
        "ring"
    );
handle_status_setting(_, _, _, _) ->
    ok.

%% erlfmt:ignore equals and arrows should line up here.
handle_incoming(NodeDef,Msg) ->
    case maps:find(console,NodeDef) of
        {ok,true} ->
            NodeName = ered_nodes:get_prop_value_from_map(
                         name,
                         NodeDef,
                         "undefined"
                        ),
            io:format("DEBUG [~s]: ~p\n", [NodeName, Msg]);
        _ ->
            ignore
    end,

    case maps:find(active,NodeDef) of
        {ok, true} ->
            Type     = ered_nodes:get_prop_value_from_map(type,  NodeDef),
            IdStr    = ered_nodes:get_prop_value_from_map(id,    NodeDef),
            ZStr     = ered_nodes:get_prop_value_from_map(z,     NodeDef),
            NameStr  = ered_nodes:get_prop_value_from_map(name,  NodeDef, Type),
            TopicStr = ered_nodes:get_prop_value_from_map(topic, Msg,     ""),

            Data = #{
                     id       => IdStr,
                     z        => ZStr,
                     '_alias' => IdStr,
                     path     => ZStr,
                     name     => NameStr,
                     topic    => to_binary_if_not_binary(TopicStr),
                     msg      => Msg,
                     format   => <<"Object">>
            },

            nodered:debug(nodered:ws(Msg), Data, normal);
        _ ->
            not_active_no_output
    end,

    handle_status_setting(
      maps:find(tostatus, NodeDef),
      maps:find(statusType, NodeDef),
      NodeDef,
      Msg
     ),

    NodeDef.

node_debug(NodeDef) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, only_incoming).
