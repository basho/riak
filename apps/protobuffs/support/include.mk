## -*- makefile -*-

ERL := erl
ERLC := $(ERL)c

INCLUDE_DIRS := ../include $(wildcard ../deps/*/include)
EBIN_DIRS := $(wildcard ../deps/*/ebin)
ERLC_FLAGS := +debug_info -W $(INCLUDE_DIRS:../%=-I ../%) $(EBIN_DIRS:%=-pa %)

ifdef debug
  ERLC_FLAGS += -Ddebug
endif

EBIN_DIR := ../ebin
DOC_DIR  := ../doc
EMULATOR := beam

ERL_TEMPLATE := $(wildcard *.et)
ERL_SOURCES  := $(wildcard *.erl)
ERL_HEADERS  := $(wildcard *.hrl) $(wildcard ../include/*.hrl)
ERL_OBJECTS  := $(ERL_SOURCES:%.erl=$(EBIN_DIR)/%.beam)
ERL_TEMPLATES := $(ERL_TEMPLATE:%.et=$(EBIN_DIR)/%.beam)
ERL_OBJECTS_LOCAL := $(ERL_SOURCES:%.erl=./%.$(EMULATOR))
APP_FILES := $(wildcard *.app)
EBIN_FILES = $(ERL_OBJECTS) $(APP_FILES:%.app=../ebin/%.app) $(ERL_TEMPLATES)
MODULES = $(ERL_SOURCES:%.erl=%)

../ebin/%.app: %.app
	cp $< $@

$(EBIN_DIR)/%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o $(EBIN_DIR) $<

$(EBIN_DIR)/%.$(EMULATOR): %.et
	$(ERL) -noshell -pa ../../elib/erltl/ebin/ -eval "erltl:compile(atom_to_list('$<'), [{outdir, \"../ebin\"}, report_errors, report_warnings, nowarn_unused_vars])." -s init stop

./%.$(EMULATOR): %.erl
	$(ERLC) $(ERLC_FLAGS) -o . $<

$(DOC_DIR)/%.html: %.erl
	$(ERL) -noshell -run edoc file $< -run init stop
	mv *.html $(DOC_DIR)

