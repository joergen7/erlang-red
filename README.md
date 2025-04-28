Erlang-RED - A Node-RED backend written in Erlang
=====

An experiment to see how far is it possible to build an Erlang backend for the Node-RED flow editor that seemlessly executes existing Node-RED flows.

The idea is not to provide Erlang nodes for NodeRED rather the idea is to replace the NodeJS backend of Node-RED with an Erlang equivalent.

The goal is to offer the advantages of visual flow based programming using a programming language that is designed for message passing and concurrency from the ground up - that's why Erlang was selected.



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

Development Strategy
---

A kind of HOWTO describes my development process, best described as [flow driven development](DevelopmentStrategy.md).

The description is aimed at folks who have never used Node-RED but who would like to contribute. Feel free to [send me](erlang.red@flowhub.org) questions relating to the project and its development.

Architecture
----

The architecture, along with a background to Flow Based Programming and Node-RED has been [written](Architecture.md).

The codebase as it stands, has many interdependence because of testing flows: [assert nodes](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/nodes/ered_node_assert_status.erl#L109-L131) need to know about [web-socket communication](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/servers/ered_ws_event_exchange.erl), or because of the nature of nodes: the [complete](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/nodes/ered_node_complete.erl#L74-L89) node needs to know of [completed messages](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/exchanges/ered_message_exchange.erl#L36-L39).

While the [exception node](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/nodes/ered_node_catch.erl#L40) needs to know about exceptions when they [happen](https://github.com/gorenje/erlang-red/blob/e2bd2f324ddc1bbe611dff29216246b80151ffc0/src/exchanges/ered_message_exchange.erl#L46-L56).

So an architectural diagram of this interconnectedness would be probably be just as confusing the code itself.

Build
-----

    $ rebar3 compile

Test
-----

    $ rebar3 eunit

Development
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

Release
---

A [release](https://github.com/gorenje/erlang-red/blob/8072c15ffbcb9444f3939f49039cf1b93f486820/rebar.config#L11-L21) can be bundled together:

    $ rebar3 release

All static frontend code (for the Node-RED flow editor) and the test flow files in `priv/testflows` are bundled into the release.

Cowboy server will listen on port 8080.

Fly.io
---

A sample Dockerfile `Dockerfile.fly` is provided to allow for easy launching of an instance as a fly application.
The provided shell script (`fly_er.sh`) sets some common expected parameters for the launch.
Advanced users may wish to examine the `fly launch` line therein and adjust for their requirements.

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

The second node (in green) is an equivalent to a change node except it contains test on attributes of the message object. Possible tests include 'equal', 'match', 'unset' and their inverses. Here the intention is that a message passes through is tested for specific values else the unit test fails.

These nodes are necessary since there is no other way to test whether flow is working or not.

Also remember these flow tests are designed to ensure the Erlang backend is correctly implementing node functionality. The purpose of these nodes is *not* to ensure a flow is correct, rather that the *functionality* of implemented nodes works and continues to work correctly.

Visual Unit Testing
---

My plan is to create test flows that represent specific NodeRED functionality that needs to be implemented by Erlang-RED. This provides regression testing and todos for the implementation.

I created a shortcut storing and creating these flows but I was still using the terminal to execute tests `make eunit-test` became painful. Instead I pulled this testing into Node-RED, as the gif demonstrates:

![img](.images/unit-testing-inside-nodered.gif)

What the gif shows is my list of unit tests, which at the press of a button, can all be tested. Notifications for each test shows the result. In addition, the tree list shows which tests failed/succeed (red 'x' or green check). Also tests can be executed individually so that fixes may be applied.

The best bit though is that all errors are pushed to the debug panel and from there I get directly to the node causing the error. Unit testing is now completely integrated into Erlang-RED.

My intention is to create many small flows that represent functionality that needs to be implemented by Erlang-RED. These unit tests shows the compatibility to Node-RED and less the correctness of the Erlang code.


Contributing
---

Contributions very much welcome in the form of Erlang code or as Node-RED test-flows, ideally with the Erlang implementation.

Each test flow should test exactly one feature and use the assert nodes to check correctness of the test. Tests can also be pending to indicate that the corresponding Erlang functionality is still missing.

Questions and Answers at either the [Erlang Forum](https://erlangforums.com/t/erlang-red-erlang-interpreter-for-node-red-flow-code-visual-flow-based-programming/4678) or the [Node-RED Forum](https://discourse.nodered.org/t/erlang-red-erlang-backed-node-red/96458).

Acknowledgement
---

[Nick](https://github.com/knolleary) and [Dave](https://github.com/dceejay) for bring Node-RED to live - amazing quality and flexibility and the entire [Node-RED community](https://discourse.nodered.org/).

Much thanks to

- [@mwmiller](https://github.com/mwmiller) for providing a fly server for running a [live version](https://ered.fly.dev/node-red) of Erlang-RED,
- [@joaohf](https://erlangforums.com/u/joaohf/summary) many tips on coding Erlang and structuring an Erlang project, and
- [@Maria-12648430](https://erlangforums.com/u/maria-12648430/summary) for debugging my initial attempt to create a gen_server for nodes.

Disclaimer
---

No Artificial Intelligence was harmed in the creation of this codebase. This codebase is old skool search engine (ddg), stackoverflow, blog posts and RTFM technology.
