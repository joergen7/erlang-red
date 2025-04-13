-module(node_change).

-export([node_change/1]).
-export([handle_incoming/2]).

-import(node_receivership, [enter_receivership/3]).

%%
%% Inject node should have at least one outgoing wire
%%

do_move( {ok,<<"msg">>}, {ok,<<"msg">>}, {ok,FromProp}, {ok,ToProp}, Msg ) ->

    case maps:find(binary_to_atom(FromProp),Msg) of
        {ok, Val} ->
            maps:put(binary_to_atom(ToProp), Val,
                     maps:remove(binary_to_atom(FromProp),Msg));
        _ ->
            Msg
    end;

do_move(_,_,_,_,Msg) ->
    Msg.

%%
%% first type check ...
do_change({ok,<<"msg">>}, {ok,<<"str">>}, {ok,<<"str">>}, Rule, Msg) ->
    do_change_str(maps:find(p,Rule),
                  maps:find(from,Rule),
                  maps:find(to,Rule),
                  Msg);

do_change(_,_,_,_,Msg) ->
    Msg.

%%
%%
do_change_str({ok,Prop}, {ok,FromStr}, {ok,ToStr}, Msg ) ->
    case maps:find(binary_to_atom(Prop), Msg) of
        {ok,Val} ->
            maps:put(binary_to_atom(Prop),
                     list_to_binary(
                       lists:flatten(string:replace(binary_to_list(Val),
                                                    binary_to_list(FromStr),
                                                    binary_to_list(ToStr), all))),
                     Msg);
        _ ->
            Msg
    end.

%%
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
            io:format("Setting ~p to ~p\n",[Prop,Value]),
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

handle_rule(<<"move">>,Rule,Msg) ->
    io:format("Rule ~p~n",[Rule]),
    do_move(maps:find(pt,Rule),
            maps:find(tot,Rule),
            maps:find(p,Rule),
            maps:find(to,Rule),
            Msg);

handle_rule(<<"change">>,Rule,Msg) ->
    do_change(maps:find(pt,Rule),
              maps:find(fromt,Rule),
              maps:find(tot,Rule),
              Rule,
              Msg);

handle_rule(_,_Rule,Msg) ->
    Msg.

handle_incoming(NodeDef,Msg) ->
    io:format("change node altering Msg\n"),
    {ok, Rules} = maps:find(rules,NodeDef),
    nodes:send_msg_to_connected_nodes(NodeDef, handle_rules(Rules,Msg)),
    NodeDef.

node_change(NodeDef) ->
    nodes:node_init(NodeDef),
    enter_receivership(?MODULE,NodeDef,only_incoming).
