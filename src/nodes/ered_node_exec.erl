-module(ered_node_exec).

-behaviour(ered_node).

-export([start/2]).
-export([handle_msg/2]).
-export([handle_event/2]).

%%
%% Exec node for executing external commands and piping the output
%% into the flows.
%%
%% [
%%     {
%%         "id": "4bff73814f9b5a17",
%%         "type": "exec",
%%         "z": "b98d0b05a760ad79",
%%         "command": "sleep 1000",
%%         "addpay": "",
%%         "append": "",
%%         "useSpawn": "true",
%%         "timer": "",  <--- timeout in seconds, kill process after this many seconds
%%         "winHide": false,
%%         "oldrc": false,
%%         "name": "",
%%         "x": 531,
%%         "y": 313.5,
%%         "wires": [
%%             [],
%%             [],
%%             []
%%         ]
%%     }
%% ]
%%

%%
%% TODO investigate how thsi interacts with a supervisor node supervising
%% TODO this node. I don't think it is supported at the moment.
%%

-import(ered_nodered_comm, [
    node_status/5,
    node_status_clear/2,
    post_exception_or_debug/3,
    unsupported/3,
    ws_from/1
]).
-import(ered_nodes, [
    get_prop_value_from_map/3,
    jstr/2,
    send_msg_to_connected_nodes/2
]).
-import(ered_messages, [
    convert_to_num/1,
    convert_units_to_milliseconds/2,
    to_bool/1
]).

-define(PIDSTATUS(PID),
    node_status(
        ws_from(Msg),
        NodeDef,
        jstr("pid:~s", [PID]),
        "blue",
        "dot"
    )
).

-define(STATUSKILLED,
    node_status(ws_from(Msg), NodeDef, "Killed", "red", "dot")
).

start(NodeDef, _WsName) ->
    ered_node:start(maps:put('_process_list', [], NodeDef), ?MODULE).

%%
%%

handle_event(_, NodeDef) ->
    NodeDef.

handle_msg({exec_process_died, Msg}, NodeDef) ->
    ProcessList = maps:get('_process_list', NodeDef),
    MachPid = maps:get(<<"pid">>, maps:get(<<"payload">>, Msg)),
    StatusCode = maps:get(<<"code">>, maps:get(<<"payload">>, Msg)),

    node_status_clear(ws_from(Msg), NodeDef),

    StatusCode > 0 andalso ?STATUSKILLED,

    %% trigger a post completed message
    {handled,
        maps:put(
            '_process_list',
            lists:keydelete(MachPid, 1, ProcessList),
            NodeDef
        ),
        Msg};
handle_msg({incoming, Msg}, NodeDef) ->
    case maps:find(<<"kill">>, Msg) of
        {ok, Signal} ->
            %% kill a command
            %% Signal is something like SIGINT, SIGTERM, ...
            Tuple =
                case maps:find(<<"pid">>, Msg) of
                    {ok, MachPid} ->
                        %% this returns false if key is not found.
                        lists:keyfind(
                            convert_to_num(MachPid),
                            1,
                            maps:get('_process_list', NodeDef)
                        );
                    _ ->
                        %% If Pid is not specified and there is more than
                        %% one process running, then ignore the kill request.
                        case maps:get('_process_list', NodeDef) of
                            [H | []] ->
                                H;
                            _ ->
                                false
                        end
                end,
            case Tuple of
                false ->
                    {handled, NodeDef, Msg};
                {_MachPid, ExecPid} ->
                    gen_server:call(ExecPid, {kill_command, Signal}),
                    {handled, NodeDef, Msg}
            end;
        _ ->
            %% execute a command
            case to_bool(maps:get(<<"useSpawn">>, NodeDef)) of
                true ->
                    {handled, start_command_running(Msg, NodeDef),
                        dont_send_complete_msg};
                false ->
                    unsupported(NodeDef, Msg, "exec mode"),
                    {handled, NodeDef, dont_send_complete_msg}
            end
    end;
handle_msg(_, NodeDef) ->
    {unhandled, NodeDef}.

%%
%%
start_command_running(Msg, NodeDef) ->
    start_command_running(maps:get(<<"command">>, NodeDef), Msg, NodeDef).

start_command_running(<<>>, Msg, NodeDef) ->
    ErrMsg = jstr(
        "TypeError: The argument 'file' cannot be empty. Received ''", []
    ),
    post_exception_or_debug(NodeDef, Msg, ErrMsg),
    NodeDef;
start_command_running(Cmd, Msg, NodeDef) ->
    Wires = maps:get(<<"wires">>, NodeDef),

    Opts = #{
        spawn => to_bool(maps:get(<<"useSpawn">>, NodeDef)),
        append => maps:get(<<"append">>, NodeDef),
        timeout => convert_to_num(
            get_prop_value_from_map(<<"timer">>, NodeDef, <<"-1">>)
        ),
        addpayload => maps:get(<<"addpay">>, NodeDef)
    },

    case maps:get(<<"append">>, NodeDef) of
        <<"">> ->
            ok;
        _ ->
            unsupported(NodeDef, Msg, "append value, will be ignored")
    end,

    case maps:get(<<"addpay">>, NodeDef) of
        <<"">> ->
            ok;
        _ ->
            unsupported(NodeDef, Msg, "add payload value, will be ignored")
    end,

    {ok, ExecPid} = ered_exec_manager:start(self(), Wires, Cmd, Msg, Opts),

    ProcessList = maps:get('_process_list', NodeDef),

    MachPid = gen_server:call(ExecPid, run_command),

    ?PIDSTATUS(integer_to_binary(MachPid)),

    maps:put('_process_list', [{MachPid, ExecPid}] ++ ProcessList, NodeDef).
