
all: jar

compile:
	@mkdir -p build
	@javac -d build `find src -name "*.java"`

jar: compile
	@mkdir -p dist
	@jar cvf dist/jiak_client.jar -C build .

clean:
	@rm -rf build dist
