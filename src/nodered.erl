-module(nodered).

%%
%% Module for sending various websocket messages to Node-RED frontend
%%
%% Does nothing if the websocket_pid isn't defined.
%%
-export([node_status/4]).
-export([debug/1]).
-export([debug/2]).
-export([unittest_result/2]).

send_on_if_ws(Msg) ->
    case whereis(websocket_pid) of
        undefined ->
            ok;
        _ ->
            websocket_pid ! Msg
    end.

node_status(NodeDef,Txt,Clr,Shp) ->
    {ok, NodeId } = maps:find(id,NodeDef),
    send_on_if_ws( { status, NodeId, Txt, Clr, Shp } ).

debug(Data) ->
    send_on_if_ws({ debug, Data }).

debug(Data,error) ->
    send_on_if_ws({ error_debug, Data });

debug(Data,warning) ->
    send_on_if_ws({ warning_debug, Data }).

unittest_result(FlowId,failed) ->
    send_on_if_ws({unittest_results, FlowId, <<"failed">>});

unittest_result(FlowId,success) ->
    send_on_if_ws({unittest_results, FlowId, <<"success">>}).
