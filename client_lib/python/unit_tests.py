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

def test_is_alive():
	client = riak.RiakClient(HOST, PORT)
	assert(client.is_alive())

def test_store_and_get():
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket('bucket')
        rand = randint()
	obj = bucket.new('foo', rand)
	obj.store()
	obj = bucket.get('foo')
	assert(obj.exists())
        assert(obj.get_bucket().get_name() == 'bucket')
        assert(obj.get_key() == 'foo')
	assert(obj.get_data() == rand)

def test_binary_store_and_get():
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket('bucket')
	# Store as binary, retrieve as binary, then compare...
        rand = str(randint())
	obj = bucket.new_binary('foo1', rand)
	obj.store()
	obj = bucket.get_binary('foo1')
	assert(obj.exists())
	assert(obj.get_data() == rand)
	# Store as JSON, retrieve as binary, JSON-decode, then compare...
	data = [randint(), randint(), randint()]
	obj = bucket.new('foo2', data)
	obj.store()
	obj = bucket.get_binary('foo2')
	assert(data == json.loads(obj.get_data()))

def test_missing_object():
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket('bucket')
	obj = bucket.get("missing")
	assert(not obj.exists())
	assert(obj.get_data() == None)

def test_delete():
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket('bucket')
	rand = randint()
	obj = bucket.new('foo', rand)
	obj.store()
	obj = bucket.get('foo')
	assert(obj.exists())
	obj.delete()
	obj.reload()
	assert(not obj.exists())

def test_set_bucket_properties():
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket('bucket')
	# Test setting allow mult...
	bucket.set_allow_multiples(True)
	assert(bucket.get_allow_multiples())
	# Test setting nval...
	bucket.set_n_val(3)
	assert(bucket.get_n_val() == 3)
	# Test setting multiple properties...
	bucket.set_properties({"allow_mult":False, "n_val":2})
	assert(not bucket.get_allow_multiples())
	assert(bucket.get_n_val() == 2)

def test_siblings():
	# Set up the bucket, clear any existing object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket('multiBucket')
	bucket.set_allow_multiples(True)
	obj = bucket.get('foo')
	obj.delete()
	# Store the same object multiple times...
	for i in range(5):
		client = riak.RiakClient(HOST, PORT)
		bucket = client.bucket('multiBucket')
		obj = bucket.new('foo', randint())
		obj.store()
	# Make sure the object has 5 siblings...
	assert(obj.has_siblings())
	assert(obj.get_sibling_count() == 5)
	# Test get_sibling()/get_siblings()...
	siblings = obj.get_siblings()
	obj3 = obj.get_sibling(3)
	assert(siblings[3].get_data() == obj3.get_data())
	# Resolve the conflict, and then do a get...
	obj3 = obj.get_sibling(3)
	obj3.store()
	obj.reload()
	assert(obj.get_data() == obj3.get_data())
	# Clean up for next test...
	obj.delete()

def test_javascript_source_map():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.new("foo", 2).store()
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
	bucket.new("foo", 2).store()
	# Run the map...
	result = client \
            .add("bucket", "foo") \
            .map("Riak.mapValuesJson") \
            .run()
	assert(result == [2])

def test_javascript_source_mapReduce():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.new("foo", 2).store()
	bucket.new("bar", 3).store()
	bucket.new("baz", 4).store()
	# Run the map...
	result = client \
            .add("bucket", "foo") \
            .add("bucket", "bar") \
            .add("bucket", "baz") \
            .map("function (v) { return [1]; }") \
            .reduce("function(v) { return v.length; } ") \
            .run()
	assert(result == 3)

def test_javascript_named_map_reduce():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.new("foo", 2).store()
	bucket.new("bar", 3).store()
	bucket.new("baz", 4).store()
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
	bucket.new("foo", 2).store()
	bucket.new("bar", 3).store()
	bucket.new("baz", 4).store()
	# Run the map...
	result = client \
            .add(bucket.name) \
            .map("Riak.mapValuesJson") \
            .reduce("Riak.reduceSum") \
            .run()
	assert(result == [9])

def test_javascript_arg_map_reduce():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.new("foo", 2).store()
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

def test_erlang_map_reduce():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.new("foo", 2).store()
	bucket.new("bar", 2).store()
	bucket.new("baz", 4).store()
	# Run the map...
	result = client \
            .add("bucket", "foo") \
            .add("bucket", "bar") \
            .add("bucket", "baz") \
            .map(["riak_mapreduce", "map_object_value"]) \
            .reduce(["riak_mapreduce", "reduce_set_union"]) \
            .run()
	assert(len(result) == 2)

def test_map_reduce_from_object():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.new("foo", 2).store()
	obj = bucket.get("foo")
	result = obj.map("Riak.mapValuesJson").run()
	assert(result == [2])

def test_store_and_get_links():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.new("foo", 2) \
            .add_link(bucket.new("foo1")) \
            .add_link(bucket.new("foo2"), "tag") \
            .add_link(bucket.new("foo3"), "tag2!@#%^&*)") \
            .store()
	obj = bucket.get("foo")
	links = obj.get_links()
	assert(len(links) == 3)

def test_link_walking():
	# Create the object...
	client = riak.RiakClient(HOST, PORT)
	bucket = client.bucket("bucket")
	bucket.new("foo", 2) \
            .add_link(bucket.new("foo1", "test1").store()) \
            .add_link(bucket.new("foo2", "test2").store(), "tag") \
            .add_link(bucket.new("foo3", "test3").store(), "tag2!@#%^&*)") \
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
test(test_is_alive)
test(test_store_and_get)
test(test_binary_store_and_get)
test(test_missing_object)
test(test_delete)
test(test_set_bucket_properties)
test(test_siblings)
test(test_javascript_source_map)
test(testJavascriptNamedMap)
test(test_javascript_source_mapReduce)
test(test_javascript_named_map_reduce)
test(test_javascript_arg_map_reduce)
test(test_erlang_map_reduce)
test(test_map_reduce_from_object)
test(test_store_and_get_links)
test(test_link_walking)
test_summary()
