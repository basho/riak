<?php

require_once 'riak.php';

define('HOST', 'localhost');
define('PORT', 8098);
define('VERBOSE', true);


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

/* function testSiblings() { */
/*   assert(false); */
/* } */

print("Starting Unit Tests\n---\n");
test("testIsAlive");
test("testStoreAndGet");
test("testBinaryStoreAndGet");
test("testMissingObject");
test("testDelete");
test("testSetBucketProperties");
test("testSiblings");
test_summary();

?>