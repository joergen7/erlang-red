Milestones
---

*Milestone Four - m4*

1. Supervisor Node

    One of the most important concepts of Erlang is the [supervisor behaviour](https://www.erlang.org/doc/system/sup_princ.html). It defines an elegant approach for monitoring, restarting and stopping processes that are unhealthy. It thus allows for complex yet stable architectures to be created in Erlang. Erlang-Red now has its very own supervisor node to provide the same behaviour but *visually*.

    The supervisor node is fully feature compatible to the supervisor behaviour and the node provides a simple mechanism of selecting nodes to be supervised and also ordered. Supervisors have a specific ordering of processes and this is also supported by the supervisor node by being able to sort nodes, each node being a process.

2. Added Pencil to Flow-editor

    ![pencil](.images/pencil-in-info.png)

    Might seem silly but it is important for documentation. Test flows can now be viewed over at Red-Erik.org (hosted via [flowhub.org](https://flowhub.org)), so that test flows can now be better documented via "pink link technology", for example, [flow one](https://flows.red-erik.org/f/6b22ce2a482c2d6d), [flow two](https://flows.red-erik.org/f/e447b0048a5983b5) and [flow three](https://flows.red-erik.org/f/3afa3b2ec00a5e3d).

    Pink links highlight nodes and groups in the flow diagram making it simpler to explain flows within the flow description. These pink links are a html stanzas, e.g.:  ```<a class='ahl-group-only' data-ids='0bf41347dde34db0'>name</a>``` - which are a *real* pain to write by hand. Node ids had to be copied, the class name typed out, node ids found from the flow editor ... blah blah :(

    Also important to note is that flow documentations is found via the flow tab:

    ![flow documentation](.images/erlang-red-flow-documentation.gif)

    This means that flow documentation lives with the flow code and is displayed both in Erlang-Red (within the flow editor) *and* at flows.red-erik.org. (All acknowledgement to the [Node-RED team](https://github.com/node-red/node-red/graphs/contributors) for putting this in the editor, I am just piggy-backing their work and extending it to my needs.)

    Back to the pencil: I now select a node or group from the info panel and click on the pencil and I have my HTML stanza:

    ![pencil function](.images/erlang-red-using-the-pencil.gif)

    The nice thing is that the info panel is usable while editing the documentation for a flow, so the pencil can always be used to create the HTML stanza for the pink links.

3. FlowHubPull node: dynamic flow code loading

    [FlowHub.org](https://flowhub.org) is my attempt at creating a visual code hosting platform. Part of that is defining interdependencies between flows. For me a flow is the flow tab (for others a "Flow" are all flow tabs). I like to think in flow tabs and creating functionality that fits into a [single flow tab](https://flowhub.org/f/c520d9da20ad7f1d).

    Flows can be linked via the link nodes and I do this very often. But what is missing is the loading of flows into Erlang-Red dynamically. So for example, [this test flow](https://flows.red-erik.org/f/048a36525238b2e7) first triggers the flow hub pull node to load another flow into Erlang-Red. It then triggers the test which contains a link to the [other flow](https://flows.red-erik.org/f/641ddaab2819c61d).

    The FlowHub pull node is currently limited to the flows contained within the Erlang-Red [test-suite](https://github.com/gorenje/erlang-red/tree/main/priv/testflows) but there isn't any reason - other than laziness - why this cannot be extended to include other sources of flow code.

4. Remove catch around function node

    With the introduction of a supervisor node, I removed the [exception handling](https://github.com/gorenje/erlang-red/commit/5617f3fcbdd49d36ad95adee2a471719377370c1#diff-b812aee125e472bd96c84632968be04bfe9dc0cc8afee1186994fbeae9cb9e86R115) around the function node - I actually had to do this to get the function node to fail (was using a divide by zero error to test the supervisor node).

    But it made me realise that Erlang is a lot different to NodeJS when it comes to exception handling. Erlang processes are lightweight and designed to fail, so let them fail becomes the intention. NodeJS on the other hand wants to handle all exceptions and not let the system get unstable, so there is much use of the try-catch-final pattern within NodeJS code.

    Erlang heals systems by restarting processes if they fail. It is assumed the system is stable until something fails. Perhaps the complete inverse of what NodeJS does.

    By removing the catch block on the function node, I'm moving away from the NodeJS way of doing things and moving towards a more Erlang-like approach.
    
    Ironically the problem with catching errors in something as generic as the function node (which takes straight up Erlang code and executes the code) is that: how to deal with the exceptions? All I was doing was pushing it to the debug panel ... but I could do that far more descriptively by using a catch node connected to a debug node.
    
    Its only a minor change yet  it represents a major step forward for Erlang-Red because it allows Erlang folks to think in terms of Erlang when using Erlang-Red.
    
    Additionally Erlang-Red now has two nodes for error handling: catch node for catching exceptions and the supervisor node for restarting failing processes. As they say in German: *twice holds better*!
    
5. Erlang, Elixir, BEAM?

    Tried but [failed](https://github.com/gorenje/erlang-red/issues/15) to get Erlang-Red compiling in an Elixir environment. It does so but then Erlang-Red won't compile in an Erlang environment. I decided to focus on the Erlang path - after all its Erlang-Red not [BEAM-Red](https://github.com/gorenje/erlang-red/discussions/14). My experience is not up to getting Erlang-Red to compile for both Elixir and Erlang, perhaps someone else will make in-roads there.


*Milestone Three - m3*

1. Elixir code constructively includable.

    Since Elixir also runs on the BEAM VM and there are some libraries that aren't available in Erlang, I made an effort to integrate third party Elixir libraries - in a structured manner - into the codebase of Erlang-RED. Many thanks to [@filmor](https://github.com/filmor) for the tip of using their [exerl](https://github.com/filmor/exerl) plugin. Works like a charm!

    This effort lead to  two new nodes using Elixir codebases: the [markdown node](src/nodes/ered_node_markdown.erl) uses [earmark](https://github.com/pragdave/earmark) and the [csv node](src/nodes/ered_node_csv.erl) uses [nimble_csv](https://github.com/dashbitco/nimble_csv).  Nimble might be removed again since there are pure Erlang CSV libraries - we'll see.

2. Erlang JSONata has moved out

	  Moved the codebase for the JSONata parser out into its own [repository](https://github.com/gorenje/erlang-red-jsonata). I wasn't actively extending it and moving it out will allow others to extend and add to the JSONata functionality supported in Erlang-RED. It's always sad when the children move out but its for the best!

3. Function node in Erlang!

    One of the more important nodes in Node-RED is the function node because it allows for creating Javascript code, i.e., dropping down to high-code instead of low-code. This can be helpful for adding functionality to a flow that is not covered by any available node. What the function node offers is syntax highlighting and error highlighting in code. This makes it a mini editor inside of Node-RED.

    An initial emulation of this this for Erlang has been made so that Erlang-RED flows can now also speak Erlang. The implementation is still rudimentary but it shows what needs to be done to get this happening. A function provides much power and the Erlang one is no different - i.e. it's a window into the server hosting Erlang-RED. So a sandbox needs to be created and much thought into how to ensure that nothing breaks or gets maliciously damaged.

    **Use at own risk!** is the conclusion.

4. Add FlowHub.org nodes for managing flow test cases

     [FlowHub.org](https://flowhub.org) is my attempt to make visual programming truly visual. One of the first things I did was to create a visualisation of Node-RED outside of Node-RED, i.e., flows [displayed in webpages](https://github.com/gorenje/node-red-flowviewer-js). From that initial effort, FlowHub.org was created to support visual version controlling of flow code - both inside of Node-RED/Erlang-RED and also externally via a website.

     Adding FlowHub to Erlang-RED now allows me to maintain the test flow [repository](https://github.com/gorenje/erlang-red-flow-testsuite) and easily import tests into either Node-RED or Erlang-RED - with the same consistent interface. As an aside: FlowHub is also coded in Node-RED, i.e., dog-fooding all the way down to the turtles.

5. [Red-Erik.org](https://red-erik.org) utilises multiple flows

    The initial release of Red-Erik was based on a single, now its a multiple flow monster! Why is this important? Because it ensures that the link nodes work across multiple flows - this is their main purposes. Also it made me think about how I could get Erlang-RED to execute multiple flows. In doing so, it became clear that executing and designing flows are very much different activities. When the flows get executed, they basically just become an Erlang architecture of processes, nothing remains of the original flows. Each process only knows where to send its messages to - when it receives a message. There is no overall structure of the flow in memory. This is great because there is no overhead to maintain such a structure.

*Milestone Two - m2*

1. JSONata parser and evaluator

    [JSONata](https://jsonata.org) is a transformation language for JSON objects that is heavily used in Node-RED. Within Node-RED it provides a vital function by offering functionality for manipulating the msg object (i.e., the data flow) without having to code Javascript code. Because of this and also wanting to be 100% compatible with NodeRED, I created a [JSONata parser](https://github.com/gorenje/erlang-red/blob/b788c04d3a2b694da908d864e5d554434e4453ea/src/jsonata_parser.erl) in Erlang and implemented basic functionality that JSONata provides. The parser is defined in [yecc](https://www.erlang.org/doc/apps/parsetools/yecc.html) and can be easily extended in either the [evaluator](https://github.com/gorenje/erlang-red/blob/b788c04d3a2b694da908d864e5d554434e4453ea/src/jsonata/jsonata_evaluator.erl#L46-L101) or in the [yecc definition](https://github.com/gorenje/erlang-red/blob/b788c04d3a2b694da908d864e5d554434e4453ea/src/jsonata_parser.yrl#L205-L485). Having JSONata support is a major step forward for the Erlang-RED!

2. External Connectivity: network connectivity via HTTP-in and MQTT nodes, command connectivity via the exec node

     Having spent the initial part of the project developing the routing and flow control nodes, this milestone has three new nodes for accessing the external world.

     The [HTTP in](https://github.com/gorenje/erlang-red/blob/b788c04d3a2b694da908d864e5d554434e4453ea/src/nodes/ered_node_http_in.erl) is my personal favourite because with it, it is possible to do [static http routing](https://ered.fly.dev/node-red?tstid=f346d45c81f595e5) within Erlang-RED. Even better, combined with the dynamic linking of the [link nodes](https://github.com/gorenje/erlang-red/blob/b788c04d3a2b694da908d864e5d554434e4453ea/src/nodes/ered_node_link_call.erl#L59-L86), it is even possible to do [dynamic web routing](https://ered.fly.dev/node-red?tstid=c562c43e69bcf0af).

     MQTT [nodes](https://github.com/gorenje/erlang-red/blob/b0092cf01bdc2333e03dab60485dd018faaae9f8/src/nodes/ered_node_mqtt_in.erl) provide message bus connectivity. The mqtt nodes are particularly useful for connectivity between Erlang-RED and NodeRED: it is now trivial to have instances of Erlang-RED communicate with NodeRED.

     Exec node can be used to shell-out to a new process. Finally it is possible to read `cat /etc/passwd` using Erlang-RED - have hours of fun and games with family and friends by hacking each other installation of Erlang-RED ;)

     With these three nodes done, it is possible to create applications for routing MQTT traffic or HTTP traffic to MQTT or vice versa.

3. 100+ Visual test flows for ensuring compatibility

    There are just over 100 visual flow tests to ensure that Erlang-RED nodes are compatible with the current Node-RED functionality. These "test" flows can be utilised by other projects (e.g. [Py-RED](https://github.com/mdkrieg/py-red)) to also ensure compatibility or by NodeRED itself for regression testing.

    Creating these flows has also given me insights into the specifics of existing nodes. It also lead to [pull requests](https://discourse.nodered.org/t/complete-split-is-the-value-wrong/96650) for the original NodeRED.

    In the long term, I will push these tests to a separate repository so that they are independent of the project.

4. Internal architecture for utilising third party Erlang libraries

    With the implementation of [mqtt nodes](https://github.com/gorenje/erlang-red/blob/b788c04d3a2b694da908d864e5d554434e4453ea/src/nodes/ered_node_mqtt_in.erl) using the [emqtt library](https://github.com/emqx/emqtt) it became clear that each library will need a [manager](https://github.com/gorenje/erlang-red/blob/b0092cf01bdc2333e03dab60485dd018faaae9f8/src/managers/ered_mqtt_manager.erl) for handling communication between the node and the library.

    This pattern was also implemented for the [exec node](https://github.com/gorenje/erlang-red/blob/b788c04d3a2b694da908d864e5d554434e4453ea/src/nodes/ered_node_exec.erl) that utilises the [erlexec](https://github.com/saleyn/erlexec) library and which has a [manager](https://github.com/gorenje/erlang-red/blob/b0092cf01bdc2333e03dab60485dd018faaae9f8/src/managers/ered_exec_manager.erl) to coordinate the message passing between erlexec and the exec node.

    What I learnt was that libraries generate messages and if these are sent directly to the node, then the [node behaviour](https://github.com/gorenje/erlang-red/blob/b788c04d3a2b694da908d864e5d554434e4453ea/src/ered_node.erl#L70-L169) would become chaos. I am torn between having [generic messages](https://github.com/gorenje/erlang-red/blob/b0092cf01bdc2333e03dab60485dd018faaae9f8/src/ered_node.erl#L118-L128) and passing them off the individual nodes and being strict on the set of message that a node can receive.

    But having a manager is actually the tie-breaker, being able to remove library specific code to the manger while the node concentrates on "node specific" functionality is optimal. Done right, it should be possible to replace third-party libraries without modification to the node code. What is node specific functionality? Something like ensure that its [status](https://github.com/gorenje/erlang-red/blob/b0092cf01bdc2333e03dab60485dd018faaae9f8/src/nodes/ered_node_delay.erl#L70-L80) is displayed correctly - in this case the messages waiting at a delay node.

    Division of responsibility is simple when nodes worry about their status and managers about external resources.

5. https://red-erik.org the very first Erlang-RED application

    Red-Erik.org represents an initial headless installation of Erlang-RED. The flow being [executed](https://github.com/gorenje/erlang-red/blob/main/priv/testflows/flow.499288ab4007ac6a.json) consists of a http-in node listening to the '/' path, two template nodes the contain the contents of the page and a http-response node that sends back the content when requested.

    ![img](.images/red-erik-flow.png)

    It is oxymoronic to speak of a "flow executing" since what actually happens is that the flow represents a blueprint for an Erlang architecture. When the [docker image](https://github.com/gorenje/erlang-red/blob/b0092cf01bdc2333e03dab60485dd018faaae9f8/Dockerfile.heroku) spins up, the Erlang processes are created (four - one the http in, two for two template nodes and one for the http response node) and then messages are sent.

    The processes aren't wired together, all that happens is that the http-in node will send a message when a request from a browser is received. That message goes to the first template node that sends it on to the second template node after modifying the message to contain its payload. Using a [mustache](https://mustache.github.io/)  template, the second template inserts the payload into a html layout. That is then sent to the http respond node that is connected to the second template. The http respond node sends the response to the client.

    So the flow specification is exactly that: a specification for a bunch Erlang processes that are completely independent of one another.


*Milestone One - m1*

1. Working integration of Erlang and Node-RED flow editor

	  Static flow editor frontend [codebase](https://github.com/gorenje/erlang-red/tree/m1/priv/node-red-frontend) communicates with Erlang via [http endpoints](https://github.com/gorenje/erlang-red/blob/m1/src/servers/ered_webserver.erl). Flow editor codebase is retrieved from a running Node-RED instance using a well defined [script](https://github.com/gorenje/erlang-red/blob/m1/priv/node-red-frontend/retrieve.sh). A clear path to updating the flow editor is thus assured: updates to the live instance are reflected in the static version in the project.

2. Erlang codebase

	  The basic shape of an Erlang project has been followed. Initially project codebase was unstructured but [many](https://erlangforums.com/t/erlang-red-erlang-interpreter-for-node-red-flow-code-visual-flow-based-programming/4678) at the Erlang forum helped to improve that structure and form it in to proper Erlang codebase. Using rebar3 for release and build management, eunit for testing and `ered_` as project prefix.

3. Erlang Architecture stabilised - Supervisors and Processes all the way down.

	  Main [gen_server components](https://github.com/gorenje/erlang-red/tree/m1/src/servers) are placed under [supervision](https://github.com/gorenje/erlang-red/blob/m1/src/erlang_red_sup.erl) to ensure stability. Nodes are [gen_server processes](https://github.com/gorenje/erlang-red/blob/m1/src/ered_node.erl) but are not placed under  supervision be will be in the long term. The ideas behind the architecture have been [documented](https://github.com/gorenje/erlang-red/blob/m1/Architecture.md).

4. Initial collection of *implemented* nodes

	  An [initial](https://github.com/gorenje/erlang-red/tree/m1/src/nodes) collection of nodes have been implemented, they are not feature complete but provide the basis for initial testing. The intention is to create as many nodes as possible to ensure that the underlying architecture supports these nodes. Feature completeness will hopefully not affect the architecture in the same way as initial creation. Using the testing panel within the flow editor, it is possible to check what works and what does not.


5. Flow Driven Development Process

	  A collection of approximately sixty [flow tests](https://github.com/gorenje/erlang-red/tree/m1/priv/testflows) have be created to support development. The idea is to develop these flows in conjunction with a working Node-RED installation and then transport these flows here. Erlang-RED Nodes are then developed until the functionality is implemented, that is, until the test passes. Test flows can also be marked as pending (using the [flow environment](https://github.com/gorenje/erlang-red/blob/m1/priv/testflows/flow.6ff45e2a0ce77393.json#L10-L12) so that future functionality can be staked out. This development process has also be [documented](https://github.com/gorenje/erlang-red/blob/m1/DevelopmentStrategy.md).


---

Milestones are spots in the life of the codebase where everything comes together and there is stability. Milestones are not planned, they are discovered once reached. Milestones have no relation to versions.
