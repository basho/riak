ERL          ?= erl
EBIN_DIRS    := $(wildcard webmachine/ebin)
APP          := webmachine_demo

all: erl ebin/$(APP).app

erl:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

clean: 
	@echo "removing:"
	@rm -fv ebin/*.beam ebin/*.app

ebin/$(APP).app: src/$(APP).app
	@cp -v src/$(APP).app $@
