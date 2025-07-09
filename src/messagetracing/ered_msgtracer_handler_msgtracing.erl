-module(ered_msgtracer_handler_msgtracing).

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
    do_msgtrace_for_node/3
]).

init(Args) ->
    {ok, Args}.

handle_event(
  {incoming,
   #{ <<"id">> := NodeId } = NodeDef,
   Pid,
   #{ '_ws' := WsName }
}, #{ '_ws' := WsName, <<"nodeids">> := NodeIds } = State) ->
    case lists:member(NodeId, NodeIds) of
        true ->
            do_msgtrace_for_node(NodeDef, Pid, State);
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
