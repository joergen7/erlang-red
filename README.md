Node-RED to Erlang
=====

A Proof of Work in Progress.

The idea is to convert Node-RED flows into Erlang processes to which message are sent.

All this does is take an [example flow](https://cdn.flowhub.org/?t=0&fhid=ea246f68766c8630&tk=#flow/ea246f68766c8630) exported as [Json](priv/flow.json), parse the Json and create processes for each node found. It then sends a message to an inject node that then passes that message onto its connected nodes.

Build
-----

    $ rebar3 escriptize

Run
---

    $ rebar3 shell
    1> test:main().
