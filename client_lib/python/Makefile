foo:
	@echo 'Usage: make {docs|test}'

docs:
	mkdir -p docs
	pydoc -w riak
	mv riak.html docs

test:
	python unit_tests.py
