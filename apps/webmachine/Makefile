ERL          ?= erl
APP          := webmachine

all: 
	@(./rebar compile)

clean:
	@(./rebar clean)

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

test: all
	scripts/run_tests.escript ebin | tee test.log

