var TEST_BUCKET = "basho_test";
var MR_TEST_BUCKET = 'mr_test';

var currentClient = null;
var currentBucket = null;
var currentObject = null;

/** Start helpers **/
function log(output) {
  if (console !== undefined) {
    console.log(output);
  }
};

function findLink(object, tag, link) {
  var found = false;
  var links = object.getLinks();
  for (var i = 0; i < links.length; i++) {
    if (links[i].tag === tag && links[i].target === link) {
      found = true;
      break;
    }
  }
  return found;
};
/** End helpers **/

function lookupMissingObject() {
  stop();
  currentClient = new RiakClient();
  currentClient.bucket(TEST_BUCKET, function(bucket, req) {
			 currentBucket = bucket;
			 currentBucket.get("td1", function(object, req) {
					     ok(object == null, "Nonexistent object returns null");
					     start(); } ); } );
};

function createMissingObject() {
  stop();
  currentBucket.get_or_new("td1", function(object, req) {
				      ok(object, "Nonexistent object created");
				      start(); } );
};

function storeMissingObject() {
  stop();
  currentBucket.get_or_new("td1", function(object, req) {
			     object.store(function(object, req) {
					    ok(object !== null, "Object saved");
					    ok(object.vclock !== null &&
					       object.vclock.length > 0, "Object vclock set");
					    currentObject = object;
					    start(); } ); } );
};

function updateObject() {
  stop();
  currentObject.store(function(n, req) {
			if (n !== null) {
			  equals(n !== null, true, "Object saved");
			  ok(n.vclock !== null &&
			     n.vclock.length > 0, "Object vclock set");
			  n.body = "Testing";
			  n.contentType = "text/plain";
			  n.store(function(newObj, req) {
				    ok(newObj != null, "Object updated");
				    ok(newObj.vclock !== n.vclock, "Vclock changed");
				    currentObject = newObj;
				    start(); } );
			}
			else {
			  throw("Object not updated!");
			} } );
};

function deleteObject() {
  stop();
  currentObject.remove(function(flag, req) {
			 ok(flag === true, "Object deleted");
			 start(); } );
};

function storeLink() {
  stop();
  var link = '/riak/' + TEST_BUCKET + '/td1';
  currentBucket.get_or_new("td1", function(object, req) {
			     object.store(function(object, req) {
					    ok(object !== null, "Object saved");
					    ok(object.links.length == 1, "New object has 1 link");
					    object.addLink(link, "testlink");
					    object.store(function(newObj, req) {
							   ok(newObj !== null, "Object saved");
							   ok(newObj.vclock !== object.vclock, "Vclock changed");
							   ok(newObj.getLinks().length == 2, "Link was saved");
							   ok(findLink(newObj, "testlink", link), "Link was found in collection");
							   currentObject = newObj;
							   start(); } ); } ); } );
};

function deleteLink() {
  stop();
  currentObject.links = [];
  currentObject.store(function(obj, req) {
			ok(obj !== null, "Object saved");
			equals(obj.links.length, 1, "Links collection reset");
			currentObject = obj;
			start(); } );
};

function readNValue() {
  stop();
  currentClient.bucket(TEST_BUCKET, function(bucket, req) {
			 ok(bucket.nValue() > 0, "N-Value default is set");
			 currentBucket = bucket;
			 start(); } );
}

function readAllowMult() {
  stop();
  currentClient.bucket(TEST_BUCKET, function(bucket, req) {
		       equals(bucket.allowsMultiples(), false, "allowsMultiples defaults to false");
		       currentBucket = bucket;
		       start(); } );
}

function updateNValue() {
  stop();
  currentBucket.nValue(5);
  currentBucket.store(function(bucket, req) {
			equals(bucket.nValue(), 5, "N-Value updated");
		        bucket.nValue(3);
		        bucket.store(function(b, req) {
				     equals(b.nValue(), 3);
				     currentBucket = b;
				     start(); } ); } );
}

function updateAllowMult() {
  stop();
  currentBucket.allowsMultiples(true);
  currentBucket.store(function(bucket, req) {
			equals(bucket.allowsMultiples(), true, "allowsMultiples updated");
		        bucket.allowsMultiples(false);
		        bucket.store(function(b, req) {
				     equals(b.allowsMultiples(), false, "allowsMultiples reset to default");
				     currentBucket = b;
				     start(); } ); } );
}

function updateSmallClock() {
  stop();
  currentBucket.props.small_vclock = 5;
  currentBucket.store(function(b, req) {
		      equals(b.props.small_vclock, 5),
		      currentBucket = b;
		      start(); } );
}

function bucketMap() {
  stop();
  currentClient.bucket(MR_TEST_BUCKET, function(bucket, req) {
			   bucket.map({language: 'javascript',
				       name: 'Riak.mapValuesJson',
				       keep: true}).run(function(flag, results, req) {
							  ok(results !== null, 'Map results not null');
							  equals(results.length, 3, 'Map results contain data');
							  start(); } ); } );
}

function bucketMapReduce() {
  stop();
}

$(document).ready(function() {
		    module("Storage");
		    test("Lookup missing object", 1, lookupMissingObject);
		    test("Create", 1, createMissingObject);
		    test("Store", 2, storeMissingObject);
		    test("Update", 4, updateObject);
		    test("Delete", 1, deleteObject);

		    module("Links");
		    test("Create", 1, createMissingObject);
		    test("Update", 6, storeLink);
		    test("Delete", 2, deleteLink);

		    module("Bucket Properties");
		    test("N-Value", 1, readNValue);
		    test("Update N-Value", 2, updateNValue);
		    test("Allow Multiples", 1, readAllowMult);
		    test("Update Allow Multiples", 2, updateAllowMult);
		    test("Set Small Clock Bucket Property", 1, updateSmallClock);

		    module("Map/Reduce");
		    test("Bucket-level map", 2, bucketMap); } );