-module(ered_node_template).

-export([node_template/2]).
-export([handle_event/2]).
-export([handle_incoming/2]).

%%
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

-import(ered_node_receivership, [enter_receivership/3]).

-import(ered_nodered_comm, [
    debug/3,
    debug_string/2,
    ws_from/1,
    unsupported/3
]).

-import(ered_nodes, [
    jstr/2,
    send_msg_to_connected_nodes/2,
    this_should_not_happen/2
]).

doit(Prop, <<"msg">>, Template, <<"plain">>, <<"str">>, Msg) ->
    Msg2 = maps:put(binary_to_atom(Prop), Template, Msg),
    {ok, Msg2};
doit(_, _, _, _, _, _) ->
    unsupported.

%%
%%
handle_event(_, NodeDef) ->
    NodeDef.

%%
%%
handle_incoming(NodeDef, Msg) ->
    {ok, Prop} = maps:find(field, NodeDef),
    {ok, PropType} = maps:find(fieldType, NodeDef),
    {ok, Syntax} = maps:find(syntax, NodeDef),
    {ok, Template} = maps:find(template, NodeDef),
    {ok, Output} = maps:find(output, NodeDef),

    case doit(Prop, PropType, Template, Syntax, Output, Msg) of
        {ok, Msg2} ->
            send_msg_to_connected_nodes(NodeDef, Msg2),
            {NodeDef, Msg2};
        unsupported ->
            ErrMsg = jstr("Unsupported configuration: ~p", [NodeDef]),
            unsupported(NodeDef, Msg, ErrMsg),
            send_msg_to_connected_nodes(NodeDef, Msg),
            {NodeDef, Msg}
    end.

node_template(NodeDef, _WsName) ->
    enter_receivership(?MODULE, NodeDef, only_incoming).
