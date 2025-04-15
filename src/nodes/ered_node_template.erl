-module(ered_node_template).

-export([node_template/1]).
-export([handle_incoming/2]).

-import(node_receivership, [enter_receivership/3]).

%%
%% junctions are decorative elements that are "transparent" - they just
%% pass through the messages that they receive (having cloned the messages)
%%
%% Attributes of importance:
%%
%%    "field": "payload",
%%    "fieldType": "msg",
%%    "format": "handlebars", <<---- ignored since this is for the frontend
%%    "syntax": "plain", <<---- (can be "mustache template")
%%    "output": "str",   <<---- (can be "parse the content as json")
%%    "template": "This is the payload: {{payload}} !",
%%

doit(Prop, <<"msg">>, Template, <<"plain">>, <<"str">>, Msg) ->
    Msg2 = maps:put(binary_to_atom(Prop), Template, Msg),
    {ok, Msg2};
doit(_, _, _, _, _, _) ->
    unsupported.

handle_incoming(NodeDef, Msg) ->
    {ok, Prop} = maps:find(field, NodeDef),
    {ok, PropType} = maps:find(fieldType, NodeDef),
    {ok, Syntax} = maps:find(syntax, NodeDef),
    {ok, Template} = maps:find(template, NodeDef),
    {ok, Output} = maps:find(output, NodeDef),

    case doit(Prop, PropType, Template, Syntax, Output, Msg) of
        {ok, Msg2} ->
            ered_nodes:send_msg_to_connected_nodes(NodeDef, Msg2);
        unsupported ->
            ErrMsg = ered_nodes:jstr(
                "Unsupported configuration: ~p",
                [NodeDef]
            ),
            nodered:debug(
                nodered:ws(Msg),
                nodered:debug_string(NodeDef, ErrMsg),
                notice
            ),
            ered_nodes:send_msg_to_connected_nodes(NodeDef, Msg);
        Bad ->
            ered_nodes:this_should_not_happen(
                NodeDef,
                io_lib:format(
                    "Bad happened ~p with [~p] and [~p]\n",
                    [Bad, NodeDef, Msg]
                )
            ),
            ErrMsg = ered_nodes:jstr(
                "Bad happened: ~p ~p ~p",
                [Bad, NodeDef, Msg]
            ),
            nodered:debug(
                nodered:ws(Msg),
                nodered:debug_string(NodeDef, ErrMsg),
                error
            )
    end,
    NodeDef.

node_template(NodeDef) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, only_incoming).
