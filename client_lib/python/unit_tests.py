#!/usr/bin/env python

import riak
try: 
        import json
except ImportError: 
        import simplejson as json
import random

HOST = 'localhost'
PORT = 8098
VERBOSE = True

# BEGIN UNIT TESTS

def testIsAlive():
	client = riak.RiakClient(HOST, PORT)
	assert(client.isAlive())

def testStoreAndGet():
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket('bucket')
        rand = randint()
	obj = bucket.newObject('foo', rand)
	obj.store()
	obj = bucket.get('foo')
	assert(obj.exists())
	assert(obj.getData() == rand)

def testBinaryStoreAndGet():
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket('bucket')
	# Store as binary, retrieve as binary, then compare...
        rand = str(randint())
	obj = bucket.newBinary('foo1', rand)
	obj.store()
	obj = bucket.getBinary('foo1')
	assert(obj.exists())
	assert(obj.getData() == rand)
	# Store as JSON, retrieve as binary, JSON-decode, then compare...
	data = [randint(), randint(), randint()]
	obj = bucket.newObject('foo2', data)
	obj.store()
	obj = bucket.getBinary('foo2')
	assert(data == json.loads(obj.getData()))

def testMissingObject():
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket('bucket')
	obj = bucket.get("missing")
	assert(not obj.exists())
	assert(obj.getData() == None)

def testDelete():
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket('bucket')
	rand = randint()
	obj = bucket.newObject('foo', rand)
	obj.store()
	obj = bucket.get('foo')
	assert(obj.exists())
	obj.delete()
	obj.reload()
	assert(not obj.exists())

def testSetBucketProperties():
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket('bucket')
	# Test setting allow mult...
	bucket.setAllowMultiples(True)
	assert(bucket.getAllowMultiples())
	# Test setting nval...
	bucket.setNVal(3)
	assert(bucket.getNVal() == 3)
	# Test setting multiple properties...
	bucket.setProperties({"allow_mult":False, "n_val":2})
	assert(not bucket.getAllowMultiples())
	assert(bucket.getNVal() == 2)

def testSiblings():
	# Set up the bucket, clear any existing object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket('multiBucket')
	bucket.setAllowMultiples(True)
	obj = bucket.get('foo')
	obj.delete()
	# Store the same object multiple times...
	for i in range(5):
		client = riak.RiakClient(HOST, PORT)
		bucket = client.bucket('multiBucket')
		obj = bucket.newObject('foo', randint())
		obj.store()
	# Make sure the object has 5 siblings...
	assert(obj.hasSiblings())
	assert(obj.getSiblingCount() == 5)
	# Test getSibling()/getSiblings()...
	siblings = obj.getSiblings()
	obj3 = obj.getSibling(3)
	assert(siblings[3].getData() == obj3.getData())
	# Resolve the conflict, and then do a get...
	obj3 = obj.getSibling(3)
	obj3.store()
	obj.reload()
	assert(obj.getData() == obj3.getData())
	# Clean up for next test...
	obj.delete()

def testJavascriptSourceMap():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.newObject("foo", 2).store()
	# Run the map...
	result = client \
            .add("bucket", "foo") \
            .map("function (v) { return [JSON.parse(v.values[0].data)]; }") \
            .run()
	assert(result == [2])

def testJavascriptNamedMap():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.newObject("foo", 2).store()
	# Run the map...
	result = client \
            .add("bucket", "foo") \
            .map("Riak.mapValuesJson") \
            .run()
	assert(result == [2])

def testJavascriptSourceMapReduce():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.newObject("foo", 2).store()
	bucket.newObject("bar", 3).store()
	bucket.newObject("baz", 4).store()
	# Run the map...
	result = client \
            .add("bucket", "foo") \
            .add("bucket", "bar") \
            .add("bucket", "baz") \
            .map("function (v) { return [1]; }") \
            .reduce("function(v) { return v.length; } ") \
            .run()
	assert(result == 3)

def testJavascriptNamedMapReduce():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.newObject("foo", 2).store()
	bucket.newObject("bar", 3).store()
	bucket.newObject("baz", 4).store()
	# Run the map...
	result = client \
            .add("bucket", "foo") \
            .add("bucket", "bar") \
            .add("bucket", "baz") \
            .map("Riak.mapValuesJson") \
            .reduce("Riak.reduceSum") \
            .run()
	assert(result == [9])

def testJavascriptBucketMapReduce():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket_" . randint())
	bucket.newObject("foo", 2).store()
	bucket.newObject("bar", 3).store()
	bucket.newObject("baz", 4).store()
	# Run the map...
	result = client \
            .add(bucket.name) \
            .map("Riak.mapValuesJson") \
            .reduce("Riak.reduceSum") \
            .run()
	assert(result == [9])

def testJavascriptArgMapReduce():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.newObject("foo", 2).store()
	# Run the map...
	result = client \
            .add("bucket", "foo", 5) \
            .add("bucket", "foo", 10) \
            .add("bucket", "foo", 15) \
            .add("bucket", "foo", -15) \
            .add("bucket", "foo", -5) \
            .map("function(v, arg) { return [arg]; }") \
            .reduce("Riak.reduceSum") \
            .run()
	assert(result == [10])

def testErlangMapReduce():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.newObject("foo", 2).store()
	bucket.newObject("bar", 2).store()
	bucket.newObject("baz", 4).store()
	# Run the map...
	result = client \
            .add("bucket", "foo") \
            .add("bucket", "bar") \
            .add("bucket", "baz") \
            .map(["riak_mapreduce", "map_object_value"]) \
            .reduce(["riak_mapreduce", "reduce_set_union"]) \
            .run()
	assert(len(result) == 2)

def testMapReduceFromObject():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.newObject("foo", 2).store()
	obj = bucket.get("foo")
	result = obj.map("Riak.mapValuesJson").run()
	assert(result == [2])

def testStoreAndGetLinks():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.newObject("foo", 2) \
            .addLink(bucket.newObject("foo1")) \
            .addLink(bucket.newObject("foo2"), "tag") \
            .addLink(bucket.newObject("foo3"), "tag2!@#%^&*)") \
            .store()
	obj = bucket.get("foo")
	links = obj.getLinks()
	assert(len(links) == 3)

def testLinkWalking():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.newObject("foo", 2) \
            .addLink(bucket.newObject("foo1", "test1").store()) \
            .addLink(bucket.newObject("foo2", "test2").store(), "tag") \
            .addLink(bucket.newObject("foo3", "test3").store(), "tag2!@#%^&*)") \
            .store()
	obj = bucket.get("foo")
	results = obj.link("bucket").run()
	assert(len(results) == 3)
	results = obj.link("bucket", "tag").run()
	assert(len(results) == 1)

test_pass = 0
test_fail = 0

def test(function):
	global test_pass, test_fail
	try:
		apply(function, [])
		test_pass+=1
		print "  [.] TEST PASSED: " + function.__name__
        except:
		test_fail+=1
		print "  [X] TEST FAILED: " + function.__name__
		if (VERBOSE): raise
			

def test_summary():
	global test_pass, test_fail
	if (test_fail == 0):
		print "\nSUCCESS: Passed all " + str(test_pass) + " tests.\n"
	else:
		test_total = test_pass + test_fail
		print "\nFAILURE: Failed " + str(test_fail) + " of " + str(test_total) + " tests!"

def randint():
        return random.randint(1, 999999)


# CALL THE UNIT TESTS

print("Starting Unit Tests\n---\n")
test(testIsAlive)
test(testStoreAndGet)
test(testBinaryStoreAndGet)
test(testMissingObject)
test(testDelete)
test(testSetBucketProperties)
test(testSiblings)
test(testJavascriptSourceMap)
test(testJavascriptNamedMap)
test(testJavascriptSourceMapReduce)
test(testJavascriptNamedMapReduce)
test(testJavascriptArgMapReduce)
test(testErlangMapReduce)
test(testMapReduceFromObject)
test(testStoreAndGetLinks)
test(testLinkWalking)
test_summary()
