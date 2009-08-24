
all: jar

compile:
	@mkdir -p build
	@javac -d build `find src -name "*.java"`

jar: compile
	@mkdir -p dist
	@jar cvf dist/jiak_client.jar -C build .

clean:
	@rm -rf build dist

javadoc: compile
	@javadoc -public -sourcepath src com.basho.riak -classpath build -d javadoc -noqualifier java.lang:java.io:java.util:org.json
