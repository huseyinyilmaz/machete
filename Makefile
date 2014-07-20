ERL ?= erl
APP := machete

REBAR = ./rebar
DIALYZER = dialyzer
MNESIA_DIR = /tmp/mnesia
NODE_NAME = machete@127.0.0.1

.PHONY: deps

compile: deps
	@./rebar compile
deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

start: compile
	erl -pa apps/*/ebin deps/*/ebin \
	    -i  lib/*/include deps/*/include \
	    -config rel/files/sys.config \
	    -sync log all \
	    -lager handlers '[{lager_console_backend, debug}]' \
	    -mnesia dir '"$(MNESIA_DIR)"' \
	    -name $(NODE_NAME) \
	    -s lager \
	    -s reloader \
	    -s machete \

