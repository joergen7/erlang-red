-module(ered_node_catch).

-export([node_catch/2]).
-export([handle_exception/4]).

%%
%% Catch node receives exceptions and passes these onto its connections.
%%
%% To adds an 'error' object to the Msg containing the following:
%%
%%          message: "Error: ENOENT: no such file or directory, open 'dfsafsdf'"
%%          source: object
%%              id: "39cedd1c01be1c7b"
%%              type: "file in"
%%              name: undefined
%%              count: 1            <--- not sure what this is TODO
%%          stack: "Error: ENOENT: no such file or directory, open 'dfsafsdf'"
%%
%% TODO Not sure about the stack
%%

-import(ered_node_receivership, [enter_receivership/3]).
-import(ered_nodes, [
    get_prop_value_from_map/2,
    jstr/1,
    send_msg_to_connected_nodes/2
]).

handle_exception(NodeDef, FromDef, Msg, ErrMsg) ->
    ErrObj = #{
        message => ErrMsg,
        source => #{
            id => get_prop_value_from_map(id, FromDef),
            type => get_prop_value_from_map(type, FromDef),
            name => jstr(get_prop_value_from_map(name, FromDef)),
            count => get_prop_value_from_map('_mc_exception', NodeDef)
        },
        stack => ErrMsg
    },
    Msg2 = maps:put(error, ErrObj, Msg),
    send_msg_to_connected_nodes(NodeDef, Msg2),
    {NodeDef, Msg2}.

node_catch(NodeDef, _WsName) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, only_exception).
