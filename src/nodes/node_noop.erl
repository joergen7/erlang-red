-module(node_noop).

%%
%% No Operation node that is used for all unknown types. It represents
%% a deadend for a message, it stops here.

-export([node_noop/1]).
-export([handle_incoming/2]).
-export([handle_outgoing/2]).

handle_incoming(NodeDef,Msg) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),

    %%
    %% This output is for eunit testing
    nodes:this_should_not_happen(
      NodeDef,
      io_lib:format("Noop Node (incoming). Nothing Done for [~p](~p) ~p\n",
                    [TypeStr,IdStr,Msg])
     ),

    %%
    %% this output is for Node-RED frontend when doing unit testing
    %% inside node red.
    IdStr       = nodes:get_prop_value_from_map(id,NodeDef),
    ZStr        = nodes:get_prop_value_from_map(z,NodeDef),

    Data = #{
             id       => IdStr,
             z        => ZStr,
             '_alias' => IdStr,
             path     => ZStr,
             name     => TypeStr,
             topic    => <<"">>,
             msg      => <<"node type is not implemented">>,
             format   => <<"string">>
    },
    nodered:debug(Data,warning),

    %%
    %% again for node red, show a status value for the corresponding node.
    nodered:node_status(NodeDef, "type undefined", "grey", "dot").


handle_outgoing(NodeDef,Msg) ->
    {ok, IdStr} = maps:find(id,NodeDef),
    {ok, TypeStr} = maps:find(type,NodeDef),

    %%
    %% This output is for eunit testing
    nodes:this_should_not_happen(
      NodeDef,
      io_lib:format("Noop Node (outgoing) Nothing Done for [~p](~p) ~p\n",
                    [TypeStr,IdStr,Msg])
     ),

    %%
    %% this output is for Node-RED frontend when doing unit testing
    %% inside node red.
    IdStr       = nodes:get_prop_value_from_map(id,NodeDef),
    ZStr        = nodes:get_prop_value_from_map(z,NodeDef),

    Data = #{
             id       => IdStr,
             z        => ZStr,
             '_alias' => IdStr,
             path     => ZStr,
             name     => TypeStr,
             topic    => <<"">>,
             msg      => <<"node type is not implemented - button press">>,
             format   => <<"string">>
    },
    nodered:debug(Data,warning),

    %%
    %% again for node red, show a status value for the corresponding node.
    nodered:node_status(NodeDef, "type undefined", "grey", "dot").

node_noop(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE,NodeDef).
