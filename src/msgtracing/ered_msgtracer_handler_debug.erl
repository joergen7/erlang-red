-module(ered_msgtracer_handler_debug).

-behaviour(gen_event).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-import(ered_msgtracer_helpers, [
    send_off_debug/2
]).

init(Args) ->
    {ok, Args}.

handle_event({incoming, NodeDef, _Pid, Msg}, State) ->
    case lists:member(
           maps:get(<<"id">>, NodeDef),
           maps:get(<<"nodeids">>, State)
          ) of
        true ->
            send_off_debug(NodeDef, Msg);
        _ ->
            ignore
    end,
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
