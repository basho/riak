include ../support/include.mk

APPLICATION=skel
DOC_OPTS={dir,\"../doc\"}
TEST_PLT=$(TEST_DIR)/dialyzer_plt

all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)

edoc:
	$(ERL) -noshell -pa ../ebin \
		-eval "edoc:application($(APPLICATION), \".\", [$(DOC_OPTS)])" \
		-s init stop

test: $(EBIN_FILES)
	mkdir -p $(TEST_DIR);
	@../support/run_tests.escript $(EBIN_DIR) | tee $(TEST_DIR)/test.log

$(TEST_PLT):
	mkdir -p $(TEST_DIR)
	cp $(DIALYZER_PLT) $(TEST_PLT)
	dialyzer --plt $(TEST_PLT) --add_to_plt -r ../deps/*/ebin

clean_plt:
	rm $(TEST_PLT)

dialyzer: $(TEST_PLT)
	dialyzer --src --plt $(TEST_PLT) -DNOTEST -DDIALYZER -c ../src | tee $(TEST_DIR)/dialyzer.log