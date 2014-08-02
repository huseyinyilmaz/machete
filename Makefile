ERL ?= erl
APP := machete

REBAR = ./rebar
DIALYZER = dialyzer
MNESIA_DIR = /tmp/mnesia
MNESIA_DIR2 = /tmp/mnesia2
MNESIA_DIR3 = /tmp/mnesia3
MNESIA_DIR4 = /tmp/mnesia4

NODE_NAME = machete@127.0.0.1
NODE_NAME2 = machete2@127.0.0.1
NODE_NAME3 = machete3@127.0.0.1
NODE_NAME4 = machete4@127.0.0.1

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
            -s mnesia \
	    -s machete \

start2: compile
	erl -pa apps/*/ebin deps/*/ebin \
	    -i  lib/*/include deps/*/include \
	    -config rel/files/sys.config \
	    -sync log all \
	    -lager handlers '[{lager_console_backend, debug}]' \
	    -mnesia dir '"$(MNESIA_DIR2)"' \
	    -name $(NODE_NAME2) \
	    -s lager \
	    -s reloader \
            -s mnesia \
	    -s machete \

start3: compile
	erl -pa apps/*/ebin deps/*/ebin \
	    -i  lib/*/include deps/*/include \
	    -config rel/files/sys.config \
	    -sync log all \
	    -lager handlers '[{lager_console_backend, debug}]' \
	    -mnesia dir '"$(MNESIA_DIR3)"' \
	    -name $(NODE_NAME3) \
	    -s lager \
	    -s reloader \
            -s mnesia \
	    -s machete \

start4: compile
	erl -pa apps/*/ebin deps/*/ebin \
	    -i  lib/*/include deps/*/include \
	    -config rel/files/sys.config \
	    -sync log all \
	    -lager handlers '[{lager_console_backend, debug}]' \
	    -mnesia dir '"$(MNESIA_DIR4)"' \
	    -name $(NODE_NAME4) \
	    -s lager \
	    -s reloader \
            -s mnesia \
	    -s machete \

release: clean deps compile
	@$(REBAR) generate
	cd rel; tar -czvf machete.tar.gz machete
	rm -rf rel/machete
	mv rel/machete.tar.gz .
	echo "machete.tar.gz is created in current directory."

build-static-plt:
	@$(DIALYZER) --build_plt --output_plt .erlang_dialyzer.plt \
		--apps erts kernel stdlib ssl crypto mnesia eunit tools os_mon runtime_tools xmerl inets deps/*

build-plt: compile
	@$(DIALYZER) --build_plt --output_plt .dialyzer.plt \
		--apps erts kernel stdlib ssl crypto \
                       mnesia eunit tools os_mon runtime_tools \
                       xmerl inets public_key webtool deps/*

dialyze:
	@$(DIALYZER) --apps apps/* --plt .dialyzer.plt\
		-Werror_handling \
		-Wrace_conditions -Wunmatched_returns
