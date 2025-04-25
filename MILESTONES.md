Milestones
---

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
