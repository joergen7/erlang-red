-module(node_debug).

-export([node_debug/1]).
-export([handle_incoming/2]).

%%
%% Debug nodes have no outgoing wires.
%%

to_binary_if_not_binary(Obj) when is_binary(Obj) ->
    Obj;
to_binary_if_not_binary(Obj) when is_list(Obj) ->
    list_to_binary(Obj);
to_binary_if_not_binary(Obj) ->
    Obj.

handle_status_setting({ok,true},{ok,<<"counter">>},NodeDef,_Msg) ->
    Cnt = nodes:get_prop_value_from_map('_mc_incoming',NodeDef),
    nodes:status(NodeDef, io_lib:format("~p",[Cnt]), "blue", "ring");

handle_status_setting(_,_,_,_) -> ok.

handle_incoming(NodeDef,Msg) ->
    NodeName = nodes:get_prop_value_from_map(name,NodeDef,"undefined"),
    io:format("DEBUG [~s]: ~p\n", [NodeName, Msg]),

    case whereis(websocket_pid) of
        undefined ->
            ok;
        _ ->
            TypeStr  = nodes:get_prop_value_from_map(type,NodeDef),
            IdStr    = nodes:get_prop_value_from_map(id,NodeDef),
            ZStr     = nodes:get_prop_value_from_map(z,NodeDef),
            NameStr  = nodes:get_prop_value_from_map(name,NodeDef,TypeStr),
            TopicStr = nodes:get_prop_value_from_map(topic,Msg,""),

            Data = #{
                     id       => IdStr,
                     z        => ZStr,
                     '_alias' => IdStr,
                     path     => ZStr,
                     name     => NameStr,
                     topic    => to_binary_if_not_binary(TopicStr),
                     msg      => jiffy:encode(Msg),
                     format   => <<"Object">>
            },

            websocket_pid ! { debug, Data },

            handle_status_setting( maps:find(tostatus,NodeDef),
                                   maps:find(statusType,NodeDef),
                                   NodeDef,
                                   Msg )
    end.

node_debug(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE,NodeDef).
