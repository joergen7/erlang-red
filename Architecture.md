# Architecture - Flow Based Programming in Erlang

How to represent a set of nodes that represent a flow in Erlang?  As processes.

How to represent the graph that is a flow in Erlang? You don't.

How to represent the state of a flow in Erlang? As ever-duplicating immutable maps.

## Background

To understand this project, it is important to know what Flow Based Programming (FBP) is and how Node-RED implements this concept. Node-RED is in fact not truly FBP but it is heavily influenced by FBP.

This project will look to the ideas of Node-RED not FBP if there happens to be a conflict (e.g., statefulness or not statefulness).

Why use Node-RED as inspiration? Because Node-RED has a well defined set of nodes and a clearly defined method for combining those nodes, representing their connections and their respective purposes.

Taking Node-RED and duplicating its functionality in Erlang provides focus since many questions have been answered: 

- What nodes should there be? 
- Should nodes have multiple inputs? 
- How to represent a complete flow with its nodes in Json? 
- What colours should the nodes be? 
- How to create a visual representation and editor for flows?
- How to handle server-client communication?

There is plenty else to do besides deciding on node colours!

In addition, it makes it simple to create unit tests since the expected behaviour is known - it's what Node-RED does. These unit tests can then be used to check the workings of Node-RED itself, it becomes a win-win situation: a set of unit tests that define the functionality of Node-RED can be used to create compute engines for flows in different programming languages, while this same set can be used as regression testing for Node-RED itself.

## Why do this in the first place?

Because programming using a text editor and keyboard is like claiming that punch cards should be revived. Programming hasn't changed since the invention of the keyboard. 

The hidden connections within a textual codebase are like understanding the plot of a book by reading five random pages. Taking a visual approach makes that plot come out that much more - after all, a picture is worth a thousand words.

We have the technology to step up our tooling for programming, just as the keyboard was a step up from punchcards. We have VR/AR, mouse and big screen colour monitors, yet we maintain a desire for green-screen mouse-less clients with mechanical keyboards. And when that does not work, we query AI for the answers.

But also: I really enjoy *visual* FBP and perhaps others might also enjoy the experience of aligning rectangles and calling it refactoring!

## Structure

Flows represent a collection of interconnected nodes. A collection of flows is a complete project. 

```
Project --> Flows --> Nodes Types --> Node Instances
```

Nodes:

- are stateless (this is an ideal not a must), state should be maintained as much as possible in the messages being passed through flows
- do exactly one thing and one thing really well, functionality of each node is limited to one feature
- all have the same interface of receiving messages and for passing on messages
- have zero or one input ports and zero or many output  ports
- can connect to multiple input ports and multiple output ports, just not to itself.

If this sounds similar to Unix pipelines, that's because FBP influenced Unix pipelines and Node-RED is influenced by pipe lines (and probably Yahoo!Pipes).

Connections between nodes are pipes: sometimes something will flow through these pipes and most of the time, nothing flows. This implies that nodes are triggered by a message.

A flow in the flow editor is a static representation of a system. Once messages start to flow through the flow, the system comes to life.

Messages represent the data upon which a flow acts. Messages start at one end of a flow and end up at the other end, moving along a well-defined pathways. What happens in-between is up to the nodes that make up the pathway travelled.

Messages:

- contain state and are manipulated and modified by nodes
- are cloned before being passed to a node, each node has their own unique copy of a message
- should ideally remain small since each pipe represents a cloning of the message object
- has a key-value API and is a key-value store.  A message is a represented by: map (Erlang), hash/object (Javascript), hashmap (Java).
- has no functionality, a message is not an object in the sense of an instance of a class. All functionality is *applied to* the message from nodes not by *functions defined* on the message.

## Erlang

Supervisors, processes and message passing - perfect for FBP.

Tail recursion, function clause matching, pattern matching and immutable data structures - perfect for the implicit stateless nature of FBP.

Erlang is just as niche, Node-RED is niche and Flow Based Programming is niche -  much niche makes more niche!

*Tech Architecture*

When I started out, I wanted to create one process per node per flow per user. This has become the case - parallel connections to the same Erlang-RED server will each have a completely isolated set of processes to execute their flows, even for the same flow.

What happens when a person presses the deploy button in the flow editor:

- request goes to the [flow deploy handler](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/http/ered_http_nodered_flow_deploy_handler.erl#L51-L78)
- which pushes that request to the [compute engine](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/servers/ered_compute_engine.erl#L71-L94) supervised gen_server
- the compute engine calls the [create pid for node](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/ered_nodes.erl#L155-L178) function that spins up a gen_server process for each node found in the flow data
- the *create pid for node* function takes the node representation as a map finds the correct [module](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/ered_nodes.erl#L262-L298) that represents the [nodes type](https://github.com/gorenje/erlang-red/tree/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/nodes)
- the *create pid for node* function also generates a [name](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/ered_nodes.erl#L103-L110) for the node which gives the node its uniqueness across all connections.
- a nodes module has a [start](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/nodes/ered_node_assert_debug.erl#L25-L26) function that creates an unsupervised gen_server which takes [messages](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/ered_node.erl#L72-L117) from other nodes and [events](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/ered_node.erl#L119-L137) from the system. Both messages and events are async but conceptually they differ hence the events are sent using `!` while messages are sent using `gen_server:cast(Pid,Message)`.
- The pid, once available, is stored in [pg](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/ered_node.erl#L55) to create a name to pid mapping - this is done by the [ered_node](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/ered_node.erl) behaviour. This allows other nodes to send this node messages.
- Once all Pids have been created, the compute engine [responds](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/servers/ered_compute_engine.erl#L94) and the request is [ended](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/http/ered_http_nodered_flow_deploy_handler.erl#L78).

On the server, i.e., Erlang codebase:

- There is no graph construct to provide an overview of the flow in its entirety, instead the flow editor provides that diagram. 
- there are only independent processes that each know where to send [their messages to](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/ered_nodes.erl#L195-L208) once they have completed their computation
- no supervision of node processes, if they fail, then a deploy will revive them. This will change if this software ever becomes production ready. The point is that it is possible, just not yet needed.

What happens when a person clicks the button on the inject node:

- request hits the [inject handler](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/http/ered_http_nodered_inject_node_button_handler.erl#L39-L55) which retrieves the process id of the corresponding inject. this is the inject node process for that web-socket connection. A web-socket is the representation of the person sitting at the terminal and pressing the inject button.
- The system (in this case a slight inconsistency since this should a `!` call) tells the inject node to [generate an outgoing](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/http/ered_http_nodered_inject_node_button_handler.erl#L49) message. Outgoing messages are node generated messages that become incoming messages for all nodes connected to the inject node.
- the inject node [receives](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/nodes/ered_node_inject.erl#L159-L161) the message from the [ered node behaviour](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/ered_node.erl#L95-L97) and [creates a message map](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/nodes/ered_node_inject.erl#L141-L155) and passes that on to its [connections](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/nodes/ered_node_inject.erl#L150).
- meanwhile the original request has been [replied to](https://github.com/gorenje/erlang-red/blob/main/src/http/ered_http_nodered_inject_node_button_handler.erl#L55).

Inject nodes can trigger flow execution from the flow editor frontend. Hence they are used to test flows and ensure that they work when real data is sent to the flow.

But wait, what happens with results and debug messages from the server?

Most of the communication between the server (Erlang codebase) and client (flow editor) is done via a [websocket connection](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/http/ered_http_nodered_websocket.erl). The web-socket id is set as a [cookie](https://raw.githubusercontent.com/gorenje/erlang-red/refs/heads/main/priv/node-red-frontend/red/red.js)[1]  in the browser and is sent with each request to the server - so the server knows to whom they are talking to. The web-socket id becomes the [named process](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/ered_nodered_comm.erl#L43) with which nodes send their messages to the flow editor.

Finally, this architecture is focussed - initially - on the creation of test flows to represent the existing functionality of Node-RED. The flow editor is optimised for that purpose. Long term flow execution is possible but is definitely not advisable at this stage (at time of writing).


[1]: Sorry code too long but it's just this:

```javascript
RED.comms.subscribe("cookie/set-wsname", function(topic,msg) {
            setCookie = (cname,cvalue) => {
                const d = new Date();
                const exdays = 1;
                d.setTime(d.getTime() + (exdays * 24 * 60 * 60 * 1000));
                let expires = "expires="+ d.toUTCString();
                document.cookie = cname+"="+cvalue+";"+expires+";path=/";
            }
            setCookie("wsname",msg.name)
        })
```

RED.comms being the Node-RED abstraction of a web-socket connection in the browser.