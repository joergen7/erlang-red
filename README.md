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

The demo is based on a simple Node-RED flow (see README @ repo) that has been exported as Json and then interpreted - no modification of the exported Json was made.

Flows
----

All Node-RED flows upon which this is tested are located in `[priv/](priv/)` and can be viewed in Node-RED using the [serverless instance of Node-RED](https://cdn.flowhub.org).

Use the import functionality and paste the included .json into the dialog.

Eventually this repository will contain a serverless version of Node-RED that will interact with a Erlang backend - but that's future works.

Build
-----

    $ rebar3 escriptize

Test
-----

    $ rebar3 eunit

Run
---

    $ rebar3 shell
    1> test:main().
