Erlang-RED - A Node-RED backend written in Erlang
=====

An experiment to see how far is it possible to build an Erlang backend for the Node-RED flow editor that seemlessly executes existing Node-RED flows.

The idea is not to provide Erlang nodes for NodeRED rather the idea is to replace the NodeJS backend of Node-RED with an Erlang equivalent.

The goal is to offer the advanages of visual flow based programming using a programming language that is designed for message passing and concurrency from the ground up - that's why Erlang was selected.


Implementation Ramblings
----

The idea is to convert Node-RED flows into Erlang processes to which message are sent.

All this does is take an [example flow](https://cdn.flowhub.org/?t=0&fhid=ea246f68766c8630&tk=#flow/ea246f68766c8630) exported as [Json](priv/flow.json), parse the Json and create processes for each node found. It then sends a message to an inject node that then passes that message onto its connected nodes.


It’s an extremely simple demo but the underlying workflow is this:

1. take a Node-RED flows.json file containing nodes and their connections

2. parse the JSON and create for each node a process.

3. each process is assigned a different function depending on the node type (Node-RED is low-code so there are nodes for doing switch (i.e. case & if), change (add/deleting entries in hashes) and debug (for printing contents of messages). So each type has a function and multiple nodes of the same time are each assigned an Erlang process.

4. generate a message and send to a random process. In the demo this happens to be an “inject” node that is responsible (in the Node-RED world) for generating messages. That process that alters the message before passing it on.

The demo is based on simple Node-RED flows that have been exported as Json and then interpreted - no modification of the exported Jsons has made.

Flows
----

All Node-RED flows upon which this is tested are located in [priv/](priv/testflows) and can be viewed in Node-RED using the [serverless instance of Node-RED](https://cdn.flowhub.org).

Use the import functionality and paste the included .json into the dialog.

~~Eventually this repository will contain a serverless version of Node-RED that will interact with a Erlang backend - but that's future works.~~ This is now the [case](node-red-frontend/).

Why?
---

[Node-RED](https://nodered.org) is a amazing[*] tool for creating flows that describe concurrent processing, it is just a shame the NodeJS is single threaded. So why not use something that is multi-process from the ground up? That way concurrency is guaranteed.

Also Erlang isn't the most understandable of programming language - unless one ~~was born~~ has fallen into in a [cauldron](https://en.wikipedia.org/wiki/Obelix) of Prolog and Lisp!

So won't it be great to have the simplicity of low-code visual flow based programming and the performance (and concurrency) of Erlang?

[*] = amazing in the sense of extendability (the codebase is Javascript both front and back), understandability (Node-RED [terminology](https://blog.openmindmap.org/blog/pipes-wires-nodes) is straightforward) and usability (many would disagree but once a rectangle becomes a unit of computation and a line becomes a pathway for data - it's simple!).

Build
-----

    $ rebar3 compile

Test
-----

    $ rebar3 eunit

Run
---

    $ rebar3 shell --apps erlang_red

Open the Node-RED visual flow editor in a browser:

    $ open -a Firefox http://localhost:9090/node-red

Docker
---

I use docker to develop this so for me, the following works:

    prompt$ git clone git@github.com:gorenje/erlang-red.git
    prompt$ docker run -it -v $(pwd)/erlang-red:/code -p 9090:8080 -w /code --rm erlang bash
    docker> rebar3 shell --apps erlang_red

Then from the docker host machine, open a browser:

    prompt$ open -a Firefox http://localhost:9090/node-red

That should display the Node-RED visual editor.

Example
---

![img](.images/erlang-red.gif)

What the gif shows is executing a [simple flow](https://flowhub.org/f/ea246f68766c8630) using Erlang as a backend. The flow demonstrates the difference in the switch node of 'check all' or 'stop at first match'.

All nodes are are pids - that is shown on the left in the terminal window.

Obviously this example is extremely trivial but it does lay the groundwork for expansion. I, for one, would rather code Erlang visually, low-code than in my Emacs ;)


Testing
---

To create unit tests for this, Node-RED frontend has been extended with a
"Create Test Case" button on the export dialog:

![img](.images/create-test-case.png)

Then flow is then stored in the [testflows](priv/testflows) dir and will be picked up the next time `make eunit-test` is called. In this way it is possible to create unit tests visually.

Assert Nodes
---

To better support testing of flows, two new nodes have been created:

![img](.images/assert-nodes.png)

"Assert Failed" node cases unit tests to fail if a message reaches it, regardless of any message values. It's basically the same as a `assert(false)` call. The intention is to ensure that specific parts of a flow aren't reached.

The second node (in green) is a equivalent to a change node except it contains test on attributes of the message object. Possible tests include 'equal', 'match', 'unset' and their inverses. Here the intention is that a message passes through is tested for specific values else the unit test fails.

These nodes are necessary since there is no other way to test whether flow is working or not.

Also remember these flow tests are designed to ensure the Erlang backend is correctly implementing node functionality. The purpose of these nodes is *not* to ensure a flow is correct, rather that the *functionality* of implemented nodes works and continues to work correctly.

Contributing
---

All contributions should be printed out on sheets of A4 paper (not US-Letter), copied in triplicate, one copy should filed away and forgotten. Another can be snail mailed to me at c/o GitHub (only one copy). The other copies should be filed away in some dusty filing cabinet at the bottom of a dark basement, preferably before removing the stairs that lead to the basement.

Alternatively just create a pull request, whichever is simpler.

Questions and Answers at either the [Erlang Forum](https://erlangforums.com/t/erlang-red-visual-flow-based-programming-for-erlang/4656) or the [Node-RED Forum](https://discourse.nodered.org/t/erlang-red-erlang-backed-node-red/96458).

Acknowledgement
---

[Nick](https://github.com/knolleary) and [Dave](https://github.com/dceejay) for bring Node-RED to live - amazing quality and flexibility and the entire [Node-RED community](https://discourse.nodered.org/).
