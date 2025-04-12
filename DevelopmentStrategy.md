Development Strategy - Flow Driven Development
---

Node-RED being a flow based visual programming environment, there is no escaping the need to have flows to test functionality. Test flows provide a development guide to what needs implementation, what is broken and what was broken by new code.

But creating flows by using a text editor would be error prone and, in a word, insane! There is only one possibility: use the Node-RED flow editor to create and test flows - visually.

What I describe here is how to create flows, how to test them from within the Node-RED flow editor and how to implement node functionality in Erlang.

*Terminology*

Node-RED consists of two parts: 

- the flow editor that runs in the browser (jQuery + Javascript), and 
- the backend which executes flows (NodeJS).

The flow editor communicates with backend via a set of [API calls](https://flowhub.org/f/15cc9fb0e94d56cd). To make the flow editor work with a different backend, these API calls need to be emulated. That is what this [cowboy code](src/http) does. Note: *cowboy* is the name of the Erlang [web framework](https://ninenines.eu/docs/en/cowboy/2.13/guide/), not the style of coding!

It is the flow editor codebase that is [included here](node-red-frontend) in the project. The Node-RED NodeJS backend has been discarded and will be replaced by Erlang :)

***Step 1: Flows are Json arrays, understanding flow definitions***

The flow definition used by Node-RED is a single array of objects stored as Json files. There is no nesting of objects, there is no arrays of arrays: just one array containing all the nodes, tabs and groups the define a flow as objects.

Example flow:

![flow](.images/flow-example.png)

That [flow](https://flowhub.org/f/7bac2d969ad2969f) (or view directly in [Node-RED](https://cdn.flowhub.org/?fhid=7bac2d969ad2969f&tk=&t=0#flow/7bac2d969ad2969f)) is represented by this Json:

```
[
    {
        "id": "7bac2d969ad2969f",
        "type": "tab",
        "label": "Hello World - part 1: Input & Output",
        "disabled": false,
        "info": "--- info text removed ---",
        "env": []
    },
    {
        "id": "14f2336960b44447",
        "type": "group",
        "z": "7bac2d969ad2969f",
        "name": "hello world",
        "style": {
            "label": true
        },
        "nodes": [
            "2753c156ed96d617",
            "501a10d8cc90b363"
        ],
        "x": 383,
        "y": 346,
        "w": 436,
        "h": 82
    },
    {
        "id": "2753c156ed96d617",
        "type": "inject",
        "z": "7bac2d969ad2969f",
        "g": "14f2336960b44447",
        "name": "inject",
        "props": [
            {
                "p": "payload"
            }
        ],
        "repeat": "",
        "crontab": "",
        "once": false,
        "onceDelay": 0.1,
        "topic": "",
        "payload": "hello world",
        "payloadType": "str",
        "x": 479,
        "y": 387,
        "wires": [
            [
                "501a10d8cc90b363"
            ]
        ]
    },
    {
        "id": "501a10d8cc90b363",
        "type": "debug",
        "z": "7bac2d969ad2969f",
        "g": "14f2336960b44447",
        "name": "output",
        "active": true,
        "tosidebar": true,
        "console": false,
        "tostatus": false,
        "complete": "payload",
        "targetType": "msg",
        "statusVal": "",
        "statusType": "auto",
        "x": 723,
        "y": 387,
        "wires": []
    }
]
```

Nodes have a type, in this case, `tab`, `group`, `inject` and `debug`. Types are not name-spaced, so types can be mirrored by malicious packages. Node-RED has a large collection of [node packages](https://flows.nodered.org/search?type=node) and there are checks in place to ensure universal type uniqueness.

Each node has a list of `wires` which represent the connections downstream from it. Each wire is a node id which should receive a message object once the node has completed its computation.

Other common attributes are:

- `z`  is the id of the flow tab in which the node is defined. The flow editor has many tabs in which flows many be defined. These flows can be interlinked using link nodes.
- `g` is the id of the group to which a node belongs - this might not be defined if the node does not belong to a group.
- `d` (not shown) is set to true if the node is disabled, in this example no node is disabled.
- `name` is the name of the node shown in the flow editor.
- `x` and`y` representing the location of the node within the flow editor. This makes it possible to view flows in [3D](https://flowhub.org/f/15cc9fb0e94d56cd/3d).

These are the most common, common attributes. In addition, each node defines its own set of node specific attributes, i.e., its configuration.

For example, the debug node from above:

```
        "active": true,
        "tosidebar": true,
        "console": false,
        "tostatus": false,
        "complete": "payload",
        "targetType": "msg",
        "statusVal": "",
        "statusType": "auto",
```

In this case, these attributes mean the following:

- `active true`: display messages in the flow editor
- `tosidebarbar true` : display messages in the sidebar within the flow editor
- `console false`: don't dump messages in the console window of the running server process
- ... I won't continue!

This functionality is partly implemented in [Erlang code](src/nodes/node_debug.erl). The same goes for the [inject node](src/nodes/node_inject.erl). Which also makes clear the naming convention of the code base: `node_<nodetype>.erl`.

For nodes that aren't implemented in Erlang, there is the [noop](src/nodes/node_noop.erl) node.

***Step 2: Accessing node details***

The flow editor makes easy to view the insides of nodes by using the export functionality (shortcut  via `ctrl-e` or `cmd-e`):

![img](.images/viewing-node-attributes.gif)

Individual nodes or the entire flow can be examined using that functionality.

It is important to implement nodes as they come, there must **not** be any Erlang specific attributes for nodes. All flows should be compatible with NodeJS based Node-RED and Erlang based Node-RED. Attributes value that  make no sense (for example `x` and `y`) can be safely **ignored** by the Erlang implementation.

How to know what attributes actually do?

I do this by trial & error: modify the configuration, try it out and make an guestimation. Also nodes are well documented within Node-RED, especially the core nodes:

![accessing documentation](.images/accessing-node-documentation.gif)

Also the [NodeJS](https://github.com/node-red/node-red/tree/master/packages/node_modules/%40node-red/nodes) source code of the nodes is well documented and understandable.

***Step 3: Creating test flows***

The flow editor included here has been extended to include easy test case creation. I did this to simplify my development process and because I wanted to avoid browser-terminal-browser-terminal-browser... context switching. 

To do this, I added a "Create Test Case" button on the export panel from above:

![creating test cases](.images/creating-test-cases.gif)

The process is then:

- create a new flow tab, name it with something pertaining to what the test does
- create the flow to test the functionality. For that, at least one inject node is required. The [unit-testing engine](src/servers/unittest_engine.erl) triggers all *active* inject nodes to start the test. This is done automagically.
- add "Assert" nodes that ensure that certain parts of a flow are either reached or not reached. "Assert True" should be placed at points in the flow that will be reached, while "Assert False" should be added to spots that won't be reached during flow execution - regardless which inject is triggered in a flow test case.
- once the test has been created, using the export panel (`{cmd|ctrl}-e`), click the 'Create Test Case' button. This will create a file in the `[priv/testflows](priv/testflows)` directory. If the test is modified, use the same button will overwrite the existing file. If this is undesirable, copy the flow to a new flows tab and create a new test case.
- switch to the testing sidebar panel and refresh the test list. The new test will be shown there.
- select the test and click the test one button. Note because I've checked the 'Clear debug panel on every test run?', the debug panel is cleared before the test is executed. This leaves then only the errors and output generated by the test in the debug panel.
- switching to the debug panel allows for finding those nodes that caused errors. For those with keen eyes, the grey status is a special "Erlang-RED only" feature. It represents debug message that are informational. In this case it's everything that *should not have happened*, i.e., missing functionality.
- the test can then be corrected and re-run using the same steps.

Note: deploying a flow isn't necessary. Also the deploy button does nothing in Erlang-RED: flows **cannot be executed** using the current implementation of the flow editor. The current flow editor is designed only for creating and executing tests.

***Step 4: debugging test cases***

The testing panel has a couple of other features that make it simple to debug and fix test cases.

![loading tests cases](.images/loading-test-cases.gif)

What happens:

- open the Testing panel, refresh the test list. This loads all tests into the testing panel but *not* into the flow editor
- check the clear the debug panel option and click on "Test all"
- all tests are executed on the server and the results are streamed to the flow editor. Tests run in parallel, so it super quick just to test everything over and over again.
- each test in the testing panel is checked with a success or failure indicator (green-check or red-x-mark).
- double-clicking on a test will either a) jump to the flow tab with the test or b) import the test into the flow editor.
- using the debug panel, its possible to jump to exact nodes that are failing. Fixing a test is just editing the flow and exporting it again (as described above)

**Note**: Because of my lack of Erlang skills is the Erlang server a single browser application. Meaning the web-socket implementation breaks when **two or more** browsers connect. This causes issues with messages to the browsers. Best not to open a second browser window to tab.

***Step 5: Coding Erlang***

I use [rebar3](http://rebar3.org/docs/getting-started/) to organise the build process, so the structure of the codebase is as expected by rebar. However, I do not claim to have done everything right - it works and seems to work, that's my main aim.

Any suggestions on how the code is could be be improved would be greatly appreciated.

So far, the code base is this:

```
.editorconfig      # Spaces not tabs
Makefile           # I prefer using make rules to using rebar
README.md
include
node-red-frontend  # Node-RED flow editor codebase
priv
 |---> testflows   # all flow test cases in form of .json files
rebar.config
rebar.lock
src
 |---> http/       # cowboy/http server code
 |---> nodes/      # individual nodes and their functionality
 |---> servers/    # gen_servers that I haved tried to create
 |---> nodes.erl   # helper module for managing processes<-->node
 |---> flows.erl   # helper module for parsing .json flow files
 |---> erlang-red.erl # the main starter
test
 |---> flow_file_test.erl # eunit test that runs all flow test cases
``` 

Each node implements "a kind of behaviour" (I haven't codified this):

```
-module(node_debug).

-export([node_debug/1]).
-export([handle_incoming/2]).

%%
%% Debug nodes have no outgoing wires.
%%

to_binary_if_not_binary(Obj) when is_binary(Obj) ->
    Obj;
to_binary_if_not_binary(Obj) when is_list(Obj) ->
    list_to_binary(Obj);
to_binary_if_not_binary(Obj) ->
    Obj.

%% if tostatus is sent, send a status update to the flow
%% editor. this message is then shown underneath the 
%% corresponding debug node.
handle_status_setting({ok,true},{ok,<<"counter">>},NodeDef,_Msg) ->
    Cnt = nodes:get_prop_value_from_map('_mc_incoming',NodeDef),
    nodered:node_status(NodeDef, io_lib:format("~p",[Cnt]), "blue", "ring");

handle_status_setting(_,_,_,_) -> ok.

%% The handler that is called when a Msg object is sent to
%% this node
handle_incoming(NodeDef,Msg) ->
    NodeName = nodes:get_prop_value_from_map(name,NodeDef,"undefined"),
    io:format("DEBUG [~s]: ~p\n", [NodeName, Msg]),

    TypeStr  = nodes:get_prop_value_from_map(type,NodeDef),
    IdStr    = nodes:get_prop_value_from_map(id,NodeDef),
    ZStr     = nodes:get_prop_value_from_map(z,NodeDef),
    NameStr  = nodes:get_prop_value_from_map(name,NodeDef,TypeStr),
    TopicStr = nodes:get_prop_value_from_map(topic,Msg,""),

    Data = #{
             id       => IdStr,
             z        => ZStr,
             '_alias' => IdStr,
             path     => ZStr,
             name     => NameStr,
             topic    => to_binary_if_not_binary(TopicStr),
             msg      => jiffy:encode(Msg),
             format   => <<"Object">>
    },
    %% send the data to the debug panel in the flow editor
    nodered:debug(Data),

		%% handle the "tostatus" attribute
    handle_status_setting( maps:find(tostatus,NodeDef),
                           maps:find(statusType,NodeDef),
                           NodeDef,
                           Msg ),
    NodeDef.

%% initialise the process. NodeDef is a map which is one-to-one
%% the hash object contained in the .json of the flow.
node_debug(NodeDef) ->
    nodes:node_init(NodeDef),
    nodes:enter_receivership(?MODULE, NodeDef, only_incoming).
```

That's basically the [debug node in Erlang](https://github.com/gorenje/erlang-red/blob/main/src/nodes/node_debug.erl) but with extra comments :)

New nodes have to be added to a lookup table [over here](https://github.com/gorenje/erlang-red/blob/b13bc798d2ad7bfbcd902069f178fe19ce66d27e/src/nodes.erl#L301-L323).

I use docker as development environment, I don't know whether natively using Erlang on a machine will also work but it should!

That's my development process at the moment, from flow.json to Erlang code in five easy steps! :)

