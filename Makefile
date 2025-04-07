build-docker-container:
	docker build -f Dockerfile.erlang -t erlang-shell .

start-docker-shell: build-docker-container
	docker run -it -v /mnt/github/erlang-red:/code -p 9090:8080 -w /code --rm erlang-shell bash

##
## The following are done inside the docker container
##
compile:
	rebar3 compile

shell:
	rebar3 shell

app-start:
	rebar3 shell --apps erlang_red

eunit-test:
	rebar3 eunit
