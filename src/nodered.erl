-module(nodered).

%%
%% Module for sending various websocket messages to Node-RED frontend
%%
%% Does nothing if the websocket_pid isn't defined.
%%
-export([node_status/4]).
-export([node_status/5]).
-export([debug/3]).
-export([unittest_result/3]).
-export([get_websocket_name/0]).
-export([websocket_name_from_request/1]).

-export([create_outgoing_msg/1]).

-export([debug_string/3]).
-export([debug_string/2]).
-export([ws/1]).

send_on_if_ws(Msg) ->
    case whereis(websocket_pid) of
        undefined ->
            io:format("WARNING[nodered](wspd): not sending ~p~n",[Msg]),
            ok;
        _ ->
            websocket_pid ! Msg
    end.

send_on_if_ws(none,Msg) ->
    io:format("WARNING[nodered](none): not sending ~p~n",[Msg]),
    ok;

send_on_if_ws(WsName,Msg) ->
    case whereis(WsName) of
        undefined ->
            io:format("WARNING[nodered](Wsname): not sending ~p ~p~n",[WsName,Msg]),
            ok;
        _ ->
            WsName ! Msg
    end.

node_status(NodeDef,Txt,Clr,Shp) ->
    {ok, NodeId } = maps:find(id,NodeDef),
    send_on_if_ws( { status, NodeId, Txt, Clr, Shp } ).

node_status(WsName, NodeDef,Txt,Clr,Shp) ->
    {ok, NodeId } = maps:find(id,NodeDef),
    send_on_if_ws(WsName, { status, NodeId, Txt, Clr, Shp }).

debug(WsName,Data,error) ->
    send_on_if_ws(WsName,{ error_debug, Data });

debug(WsName,Data,warning) ->
    send_on_if_ws(WsName,{ warning_debug, Data });

debug(WsName,Data,notice) ->
    send_on_if_ws(WsName,{ notice_debug, Data });

debug(WsName,Data,normal) ->
    send_on_if_ws(WsName,{ debug, Data }).

unittest_result(WsName,FlowId,failed) ->
    send_on_if_ws(WsName,{unittest_results, FlowId, <<"failed">>});

unittest_result(WsName,FlowId,success) ->
    send_on_if_ws(WsName,{unittest_results, FlowId, <<"success">>}).

%%
%%
get_websocket_name() ->
    binary_to_atom(list_to_binary(io_lib:format("ws~s",[nodes:generate_id(6)]))).

%%
%% Obtain the WsName from the HTTP Req or empty atom
websocket_name_from_request(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),

    case lists:keyfind(<<"wsname">>, 1, Cookies) of
        {<<"wsname">>,WsName} ->
            binary_to_atom(WsName);
        _ ->
            none
    end.

ws(Msg) ->
    case maps:find('_ws',Msg) of
        {ok,Val} ->
            Val;
        _ ->
            none
    end.

create_outgoing_msg(WsName) ->
    {outgoing, #{'_msgid' => nodes:generate_id(), '_ws' => WsName }}.


%%
%% helpers
debug_string(NodeDef,Msg) ->
    IdStr = nodes:get_prop_value_from_map(id,NodeDef),
    ZStr  = nodes:get_prop_value_from_map(z,NodeDef),
    debug_string(IdStr,ZStr,Msg).

debug_string(NodeId,TabId,Msg) ->
    #{
      id     => NodeId,
      z      => TabId,
      path   => TabId,
      name   => <<"Unit Test Notice">>,
      topic  => <<"">>,
      msg    => Msg,
      format => <<"string">>
    }.
