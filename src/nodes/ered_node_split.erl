-module(ered_node_split).

-export([node_split/2]).
-export([handle_incoming/2]).

-import(ered_node_receivership, [enter_receivership/3]).

-import(ered_nodes, [
    jstr/2,
    send_msg_to_connected_nodes/2
]).

%%
%% Split node takes an array, string or buffer and for each item, it generates
%% a new message with a new msg. It also adds a parts attribute to the
%% message to identify this message as being part of a collection that the
%% join node can group back together again.
%%
%% Most interesting attributes:
%%
%%     "splt": "\\n",
%%     "spltType": "str",
%%     "arraySplt": 1,
%%     "arraySpltType": "len",
%%     "stream": false,
%%     "addname": "",
%%     "property": "payload",
%%
%% (Note: the misspelling 'splt' is desired)
%%
%% This node decides on the type of payload what to do. I.e. if the payload
%% is an array, then the array configuraiton is taken and everything else
%% is ignored. Similar for string & buffer.
%%
%% Also this acts only on properties defined on the msg object, flow, global
%% are not accessible.
%%

handle_incoming(NodeDef, Msg) ->
    send_msg_to_connected_nodes(NodeDef, Msg),
    NodeDef.

node_split(NodeDef, _WsName) ->
    ered_nodes:node_init(NodeDef),
    enter_receivership(?MODULE, NodeDef, only_incoming).
