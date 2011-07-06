
REBAR_URL ?= http://github.com/downloads/basho/rebar/rebar

REBAR_GLOBAL ?= $(shell which rebar)
REBAR_LOCAL  ?= $(shell which ./rebar)
REBAR_TARGET ?= $(dir $(shell which escript))

# Check for local rebar (./rebar), then globally installed and default to
# /usr/local/bin/rebar
ifneq ($(strip $(REBAR_LOCAL)), )
REBAR ?= $(REBAR_LOCAL)
else ifneq ($(strip $(REBAR_GLOBAL)), )
REBAR ?= $(REBAR_GLOBAL)
else
REBAR ?= /usr/local/bin/rebar
$(warning Rebar not installed or available. Try 'make rebar-info')
endif

# Try to use wget and fall back to curl
ifneq ($(strip $(shell which wget)), )
REBAR_FETCH ?= wget --no-check-certificate -q -O - $(REBAR_URL)
else
REBAR_FETCH ?= curl -s -f $(REBAR_URL)
endif

# Check for missing/empty REBAR_TARGET; fallback to /usr/local/bin in that
# situation
ifeq ($(strip $(REBAR_TARGET)),)
REBAR_TARGET = /usr/local/bin
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

