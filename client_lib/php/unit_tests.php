<?php

require_once 'riak.php';

define('HOST', 'localhost');
define('PORT', 8098);
define('VERBOSE', true);


print("Starting Unit Tests\n---\n");

test("testIsAlive");
test("testStoreAndGet");
test("testBinaryStoreAndGet");
test("testMissingObject");
test("testDelete");
test("testSetBucketProperties");
test("testSiblings");

test("testJavascriptSourceMap");
test("testJavascriptNamedMap");
test("testJavascriptSourceMapReduce");
test("testJavascriptNamedMapReduce");
test("testJavascriptArgMapReduce");

test("testErlangFunctionMap");
test("testErlangFunctionMapReduce");
test("testMapReduceFromObject");

test("testStoreAndGetLinks");
test("testLinkWalking");

test_summary();


/* BEGIN UNIT TESTS */

function testIsAlive() {
  $client = new RiakClient(HOST, PORT);
  test_assert($client->isAlive());
}

function testStoreAndGet() {
  $client = new RiakClient(HOST, PORT);
  $bucket = $client->bucket('bucket');
  
  $rand = rand();
  $obj = $bucket->newObject('foo', $rand);
  $obj->store();

  $obj = $bucket->get('foo');
  
  test_assert($obj->exists());
  test_assert($obj->getData() == $rand);
}

function testBinaryStoreAndGet() {
  $client = new RiakClient(HOST, PORT);
  $bucket = $client->bucket('bucket');

  # Store as binary, retrieve as binary, then compare...
  $rand = rand();
  $obj = $bucket->newBinary('foo1', $rand);
  $obj->store();
  $obj = $bucket->getBinary('foo1');
  test_assert($obj->exists());
  test_assert($obj->getData() == $rand);

  # Store as JSON, retrieve as binary, JSON-decode, then compare...
  $data = array(rand(), rand(), rand());
  $obj = $bucket->newObject('foo2', $data);
  $obj->store();
  $obj = $bucket->getBinary('foo2');
  assert($data == json_decode($obj->getData()));
}

function testMissingObject() {
  $client = new RiakClient(HOST, PORT);
  $bucket = $client->bucket('bucket');
  $obj = $bucket->get("missing");
  test_assert(!$obj->exists());
  test_assert($obj->getData() == NULL);
}

function testDelete() {
  $client = new RiakClient(HOST, PORT);
  $bucket = $client->bucket('bucket');
  
  $rand = rand();
  $obj = $bucket->newObject('foo', $rand);
  $obj->store();

  $obj = $bucket->get('foo');
  test_assert($obj->exists());

  $obj->delete();
  $obj->reload();
  test_assert(!$obj->exists());
}

function testSetBucketProperties() {
  $client = new RiakClient(HOST, PORT);
  $bucket = $client->bucket('bucket');

  # Test setting allow mult...
  $bucket->setAllowMultiples(TRUE);
  test_assert($bucket->getAllowMultiples());

  # Test setting nval...
  $bucket->setNVal(3);
  test_assert($bucket->getNVal() == 3);

  # Test setting multiple properties...
  $bucket->setProperties(array("allow_mult"=>FALSE, "n_val"=>2));
  test_assert(!$bucket->getAllowMultiples());
  test_assert($bucket->getNVal() == 2);
}

function testSiblings() {
  # Set up the bucket, clear any existing object...
  $client = new RiakClient(HOST, PORT);
  $bucket = $client->bucket('multiBucket');
  $bucket->setAllowMultiples('true');
  $obj = $bucket->get('foo');
  $obj->delete();

 # Store the same object multiple times...
  for ($i=0; $i<5; $i++) {
    $client = new RiakClient(HOST, PORT);
    $bucket = $client->bucket('multiBucket');
    $obj = $bucket->newObject('foo', rand());
    $obj->store();
  }

  # Make sure the object has 5 siblings...
  test_assert($obj->hasSiblings());
  test_assert($obj->getSiblingCount() == 5);

  # Test getSibling()/getSiblings()...
  $siblings = $obj->getSiblings();
  $obj3 = $obj->getSibling(3);
  assert($siblings[3]->getData() == $obj3->getData());

  # Resolve the conflict, and then do a get...
  $obj3 = $obj->getSibling(3);
  $obj3->store();
  
  $obj->reload();
  assert($obj->getData() == $obj3->getData());
  
  # Clean up for next test...
  $obj->delete();
}

function testJavascriptSourceMap() {
  # Create the object...
  $client = new RiakClient(HOST, PORT);
  $bucket = $client->bucket("bucket");
  $bucket->newObject("foo", 2)->store();

  # Run the map...
  $result = $client->
    add("bucket", "foo")->
    map("function (v) { return [JSON.parse(v.values[0].data)]; }") ->
    run();
  assert($result == array(2));
}

function testJavascriptNamedMap() {
  # Create the object...
  $client = new RiakClient(HOST, PORT);
  $bucket = $client->bucket("bucket");
  $bucket->newObject("foo", 2)->store();

  # Run the map...
  $result = $client->
    add("bucket", "foo")->
    map("Riak.mapValuesJson") ->
    run();
  assert($result == array(2));
}

function testJavascriptSourceMapReduce() {
  # Create the object...
  $client = new RiakClient(HOST, PORT);
  $bucket = $client->bucket("bucket");
  $bucket->newObject("foo", 2)->store();
  $bucket->newObject("bar", 3)->store();
  $bucket->newObject("baz", 4)->store();

  # Run the map...
  $result = $client->
    add("bucket", "foo")->
    add("bucket", "bar")->
    add("bucket", "baz")->
    map("function (v) { return [1]; }") ->
    reduce("function (v) { return v.length; }")->
    run();
  assert($result == 3);
}

function testJavascriptNamedMapReduce() {
  # Create the object...
  $client = new RiakClient(HOST, PORT);
  $bucket = $client->bucket("bucket");
  $bucket->newObject("foo", 2)->store();
  $bucket->newObject("bar", 3)->store();
  $bucket->newObject("baz", 4)->store();

  # Run the map...
  $result = $client->
    add("bucket", "foo")->
    add("bucket", "bar")->
    add("bucket", "baz")->
    map("Riak.mapValuesJson") ->
    reduce("Riak.reduceSum")->
    run();
  assert($result == array(9));
}

function testJavascriptBucketMapReduce() {
  # Create the object...
  $client = new RiakClient(HOST, PORT);
  $bucket = $client->bucket("bucket_" . rand());
  $bucket->newObject("foo", 2)->store();
  $bucket->newObject("bar", 3)->store();
  $bucket->newObject("baz", 4)->store();

  # Run the map...
  $result = $client->
    add($bucket->name)->
    map("Riak.mapValuesJson") ->
    reduce("Riak.reduceSum")->
    run();
  assert($result == array(9));
}

function testJavascriptArgMapReduce() {
  # Create the object...
  $client = new RiakClient(HOST, PORT);
  $bucket = $client->bucket("bucket");
  $bucket->newObject("foo", 2)->store();

  # Run the map...
  $result = $client->
    add("bucket", "foo", 5)->
    add("bucket", "foo", 10)->
    add("bucket", "foo", 15)->
    add("bucket", "foo", -15)->
    add("bucket", "foo", -5)->
    map("function(v, arg) { return [arg]; }")-> 
    reduce("Riak.reduceSum")->
    run();
  assert($result == array(10));
}

function testErlangFunctionMap() {
  assert(false);
}

function testErlangFunctionMapReduce() {
  assert(false);
}

function testMapReduceFromObject() {
  assert(false);
}


function testStoreAndGetLinks() {
  # Create the object...
  $client = new RiakClient(HOST, PORT);
  $bucket = $client->bucket("bucket");
  $bucket->newObject("foo", 2)->
    addLink($bucket->newObject("foo1"))->
    addLink($bucket->newObject("foo2"), "tag")->
    addLink($bucket->newObject("foo3"), "tag2!@#$%^&*")->
    store();

  $obj = $bucket->get("foo");
  $links = $obj->getLinks();
  assert(count($links) == 3);
}

function testLinkWalking() {
  # Create the object...
  $client = new RiakClient(HOST, PORT);
  $bucket = $client->bucket("bucket");
  $bucket->newObject("foo", 2)->
    addLink($bucket->newObject("foo1", "test1")->store())->
    addLink($bucket->newObject("foo2", "test2")->store(), "tag")->
    addLink($bucket->newObject("foo3", "test3")->store(), "tag2!@#$%^&*")->
    store();
  
  $obj = $bucket->get("foo");
  $results = $obj->link("bucket")->run();
  assert(count($results) == 3);

  $results = $obj->link("bucket", "tag")->run();
  assert(count($results) == 1);
}


/* BEGIN UNIT TEST FRAMEWORK */

assert_options(ASSERT_ACTIVE,   true);
assert_options(ASSERT_BAIL,     true);
$test_pass = 0; $test_fail = 0;

function test($method) {
  global $test_pass, $test_fail;
  try {
    $method(); 
    $test_pass++;
    print "  [.] TEST PASSED: $method\n";
  } catch (Exception $e) {
    $test_fail++;
    print "  [X] TEST FAILED: $method\n";
    if (VERBOSE) {
      throw $e;
    }
  }
}

function test_summary() {
  global $test_pass, $test_fail;
  if ($test_fail == 0) {
    print "\nSUCCESS: Passed all $test_pass tests.\n";
  } else {
    $test_total = $test_pass + $test_fail;
    print "\nFAILURE: Failed $test_fail of $test_total tests!";
  }
}

function test_assert($bool) {
  if (!$bool) throw new Exception("Test failed.");
}

/* END UNIT FRAMEWORK */
?>