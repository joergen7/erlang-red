Node-RED to Erlang
=====

A Proof of Work in Progress.

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

All Node-RED flows upon which this is tested are located in [priv/](priv/) and can be viewed in Node-RED using the [serverless instance of Node-RED](https://cdn.flowhub.org).

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

Acknowledgement
---

[Nick](https://github.com/knolleary) and [Dave](https://github.com/dceejay) for bring Node-RED to live - amazing quality and flexibility and the entire [Node-RED community](https://discourse.nodered.org/).
