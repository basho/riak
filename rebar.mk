
REBAR_URL ?= http://github.com/downloads/basho/rebar/rebar

REBAR_GLOBAL ?= $(shell which rebar)
REBAR_LOCAL  ?= $(shell which ./rebar)
REBAR_TARGET ?= $(dir $(shell which escript))

ifneq ($(strip $(REBAR_LOCAL)), )
REBAR ?= $(REBAR_LOCAL)
else ifneq ($(strip $(REBAR_GLOBAL)), )
REBAR ?= $(REBAR_GLOBAL)
else
REBAR ?= rebar
$(warning Rebar not installed or available. Try 'make rebar-info')
endif

ifneq ($(strip $(shell which wget)), )
REBAR_FETCH ?= wget --no-check-certificate -q -O - $(REBAR_URL)
else
REBAR_FETCH ?= curl -s -f $(REBAR_URL)
endif


rebar-info:
	@echo "Rebar needs to be either on your path or present in the current" \
	      "working directory:\n" \
	      "* 'make rebar-install' will download and install it into your Erlang path (RECOMMENDED)\n" \
	      "* 'make rebar-get' will download it to your current working directory\n" \


rebar-install:
	$(REBAR_FETCH) > $(REBAR_TARGET)/rebar
	chmod a+x $(REBAR_TARGET)/rebar

rebar-get:
	$(REBAR_FETCH) > ./rebar
	chmod u+x ./rebar

