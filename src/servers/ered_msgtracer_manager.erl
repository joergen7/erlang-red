-module(ered_msgtracer_manager).

%%
%% Message trace manager.
%%
%% Respnsible for passing on messages to the correct handlers if any are
%% defined. Using the gen_event for this because the events are the messages
%% received by nodes. In that sense there is a single type of event and that is
%% a node received a message.
%%
%% Handlers are defined either for a subset of nodes or all nodes. That is for
%% the handler to decide, this managers just sends all registered handlers
%% the incoming messages when they come in.
%%
%% the four possible handlers (at time of writing) are defined in the
%% `src/msgtracing` directory and cover the four possibilities: msgtrace for
%% subset or for all nodes, debug messages for a subset or all nodes. Both
%% types of tracing can be activated at the same time,
%%

-behaviour(gen_event).

-export([
    start_link/0,
    stop/0,
    node_received_msg/3,
    remove_handler/1,
    add_handler/2
]).

-export([
    init/1,
    handle_event/2,
    handle_call/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% Event Manager API
start_link() ->
    % Register with the module name locally
    gen_event:start_link({local, ?MODULE}).

stop() ->
    gen_event:stop(?MODULE).

node_received_msg(NodeDef, Pid, Msg) ->
    gen_event:notify(?MODULE, {incoming, NodeDef, Pid, Msg}).

remove_handler(Module) ->
    gen_event:delete_handler(?MODULE, Module, []).

add_handler(Module, NodeDetails) ->
    gen_event:add_handler(?MODULE, Module, NodeDetails).

% gen_event behaviour callbacks - for the event manger internals.
init(_Args) ->
    {ok, []}.

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
