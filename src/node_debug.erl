-module(node_debug).
-export([node_debug/1]).


%%
%% Debug nodes have no outgoing wires.
%%

to_binary_if_not_binary(Obj) ->
    case is_binary(Obj) of
        true ->
            Obj;
        _ ->
            case is_list(Obj) of
                true ->
                    list_to_binary(Obj);
                _ ->
                    Obj
            end
    end.

receive_loop(NodeDef) ->
    receive
        {incoming, Msg} ->

            NodeName = nodes:get_prop_value_from_map(name,NodeDef,"undefined"),
            io:format("DEBUG [~s]: ~p\n", [NodeName, Msg]),

            case whereis(websocket_pid) of
                undefined ->
                    ok;
                _ ->
                    {ok, IdStr} = maps:find(id,NodeDef),
                    {ok, ZStr} = maps:find(z,NodeDef),
                    IdStr = nodes:get_prop_value_from_map(id,NodeDef),
                    ZStr = nodes:get_prop_value_from_map(z,NodeDef),
                    NameStr = nodes:get_prop_value_from_map(name,NodeDef),
                    TopicStr = nodes:get_prop_value_from_map(topic,Msg,""),

                    Data = #{
                             id => IdStr,
                             z => ZStr,
                             '_alias' => IdStr,
                             path => ZStr,
                             name => NameStr,
                             topic => to_binary_if_not_binary(TopicStr),
                             msg => jiffy:encode(Msg),
                             format => <<"Object">>
                    },

                    websocket_pid ! { data, jiffy:encode([#{ topic => debug,
                                                          data => Data } ])}
            end,

            receive_loop(NodeDef);

        stop ->
            ok

    end.

node_debug(NodeDef) ->
    io:format("debug node init\n"),
    receive_loop(NodeDef).
