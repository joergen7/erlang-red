-module(ered_node_noop).

%%
%% No Operation node that is used for all unknown types. It represents
%% a deadend for a message, it stops here.

-export([node_noop/1]).
-export([handle_incoming/2]).
-export([handle_outgoing/2]).

-import(ered_node_receivership, [enter_receivership/3]).

%% erlfmt:ignore equals and arrows should line up here.
create_data_for_debug(NodeDef,TypeStr) ->
    IdStr   = ered_nodes:get_prop_value_from_map(id,   NodeDef),
    ZStr    = ered_nodes:get_prop_value_from_map(z,    NodeDef),
    NameStr = ered_nodes:get_prop_value_from_map(name, NodeDef, TypeStr),

    #{
       id       => IdStr,
       z        => ZStr,
       '_alias' => IdStr,
       path     => ZStr,
       name     => NameStr,
       topic    => <<"">>,
       msg      => ered_nodes:jstr("type '~s' is not implemented", [TypeStr]),
       format   => <<"string">>
    }.

handle_incoming(NodeDef, Msg) ->
    {ok, IdStr} = maps:find(id, NodeDef),
    {ok, TypeStr} = maps:find(type, NodeDef),

    %%
    %% This output is for eunit testing. This does in fact does end
    %% up in the Node-RED frontend as a notice, but that's only
    %% because I changed the code after writing the initial comment.
    ered_nodes:this_should_not_happen(
        NodeDef,
        io_lib:format(
            "Noop Node (incoming). Nothing Done for [~p](~p) ~p\n",
            [TypeStr, IdStr, Msg]
        )
    ),

    %%
    %% this output is for Node-RED frontend when doing unit testing
    %% inside node red.
    nodered:debug(
        nodered:ws(Msg),
        create_data_for_debug(NodeDef, TypeStr),
        warning
    ),

    %%
    %% again for node red, show a status value for the corresponding node.
    nodered:node_status(
        nodered:ws(Msg),
        NodeDef,
        "type not implemented",
        "grey",
        "dot"
    ),

    NodeDef.

handle_outgoing(NodeDef, Msg) ->
    {ok, IdStr} = maps:find(id, NodeDef),
    {ok, TypeStr} = maps:find(type, NodeDef),

    %%
    %% This output is for eunit testing. This does in fact does end
    %% up in the Node-RED frontend as a notice, but that's only
    %% because I changed the code after writing the initial comment.
    ered_nodes:this_should_not_happen(
        NodeDef,
        io_lib:format(
            "Noop Node (outgoing) Nothing Done for [~p](~p) ~p\n",
            [TypeStr, IdStr, Msg]
        )
    ),

    %%
    %% this output is for Node-RED frontend when doing unit testing
    %% inside node red.
    nodered:debug(
        nodered:ws(Msg),
        create_data_for_debug(NodeDef, TypeStr),
        warning
    ),

    %%
    %% again for node red, show a status value for the corresponding node.
    nodered:node_status(
        nodered:ws(Msg),
        NodeDef,
        "type not implemented",
        "grey",
        "dot"
    ),

    NodeDef.

node_noop(NodeDef) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, incoming_and_outgoing).
