-module(node_change).

-export([node_change/1]).
-export([handle_incoming/2]).

%%
%% Inject node should have at least one outgoing wire
%%

handle_rules([],Msg) ->
    Msg;

handle_rules([Rule|MoreRules],Msg) ->
    {ok, RuleType} = maps:find(t,Rule),
    handle_rules(MoreRules, handle_rule(RuleType, Rule, Msg)).


handle_rule(<<"set">>,Rule,Msg) ->
    %%
    %% pt can be many things (flow,global,...) we only support
    %% msg - at least until this comment gets removed.
    %%
    case maps:find(pt,Rule) of
        {ok, <<"msg">>} ->
            {ok, Prop} = maps:find(p,Rule),
            {ok, Value} = maps:find(to,Rule),
            io:format("Setting ~p to ~p on ~p\n",[Prop,Value,Msg]),
            %%
            %% TODO there is also a tot which is the type - ignore
            %%
            maps:put(binary_to_atom(Prop),Value,Msg);
        _ ->
            Msg
    end;

handle_rule(<<"delete">>,Rule,Msg) ->
    case maps:find(pt,Rule) of
        {ok, <<"msg">>} ->
            {ok, Prop} = maps:find(p,Rule),
            io:format("Removing ~p from Msg\n",[Prop]),
            maps:remove(binary_to_atom(Prop),Msg);
        _ ->
            Msg
    end;

handle_rule(<<"move">>,_Rule,Msg) ->
    Msg;

handle_rule(<<"change">>,_Rule,Msg) ->
    Msg.


handle_incoming(NodeDef,Msg) ->
    io:format("change node altering Msg\n"),
    {ok, Rules} = maps:find(rules,NodeDef),
    nodes:send_msg_to_connected_nodes(NodeDef, handle_rules(Rules,Msg)).

node_change(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE,NodeDef).
