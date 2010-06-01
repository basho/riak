
all: jar

compile:
	@mkdir -p build
	@javac -d build `find src -name "*.java"`

jar: compile
	@mkdir -p dist
	@jar cvf dist/riak_jmx.jar -C build .
	@cp dist/riak_jmx.jar ../priv

clean:
	@rm -rf build dist
	@rm -rf ../priv/riak_jmx.jar


