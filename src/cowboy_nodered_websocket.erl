-module(cowboy_nodered_websocket).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).
-export([ws_send/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_init([{stats_interval, SInterval}]) ->
    ws_send(self(), SInterval),
    register(websocket_pid, self()),
    erlang:start_timer(
      1000,
      websocket_pid,
      jiffy:encode([#{ topic => <<"notification/runtime-state">>,
                       data => #{ state => start,
                                  deploy => true
                                }}])
    ),
    {ok, [{stats_interval, SInterval}]}.

websocket_info({timeout, _Ref, Msg}, [{stats_interval, SInterval}]) ->
    ws_send(self(), SInterval),
    {reply, {text, Msg}, [{stats_interval, SInterval}]};

websocket_info({data, Msg}, [{stats_interval, SInterval}]) ->
    io:format("sending on socket ~p~n",[Msg]),
    {reply, {text, Msg}, [{stats_interval, SInterval}]};

websocket_info({debug, Data}, [{stats_interval, SInterval}]) ->
    io:format("sending debug on socket ~p~n",[Data]),
    Msg = jiffy:encode([#{ topic => debug, data => Data } ]),
    {reply, {text, Msg}, [{stats_interval, SInterval}]};


websocket_info(_Info, State) ->
    {ok, State}.

ws_send(Pid, SInterval) ->
    Millis = erlang:system_time(millisecond),
    Data_jsonb = jiffy:encode([#{ topic => hb, data => Millis }]),
    erlang:start_timer(SInterval, Pid, Data_jsonb).

terminate(_Reason, _Req, _State) ->
    ok.
