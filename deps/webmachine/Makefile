ERL          ?= erl
EBIN_DIRS    := $(wildcard deps/*/ebin)
APP          := webmachine

all: mochi erl ebin/$(APP).app

mochi:
	@(cd deps/mochiweb;$(MAKE))

erl:
	@$(ERL) -pa $(EBIN_DIRS) -noinput +B \
	  -eval 'case make:all() of up_to_date -> halt(0); error -> halt(1) end.'

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'

clean: 
	@echo "removing:"
	@rm -fv ebin/*.beam ebin/*.app

ebin/$(APP).app: src/$(APP).app
	@cp -v src/$(APP).app $@
