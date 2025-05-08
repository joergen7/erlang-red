Milestones
---
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
