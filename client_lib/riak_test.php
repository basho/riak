<?php



# -- CREATE A RIAK CONNECTION --
require_once 'riak.php';
$riak = new RiakClient("localhost", 8098);



# -- STORE AND FETCH AN OBJECT (SERIALIZE AS JSON) --

# Store an object...
$t = new stdClass();
$t->attr1 = 1;
$t->attr2 = 2;
$riak->store("bucket", "key1", $t);

# Fetch the object...
$obj = $riak->fetch("bucket", "key1");
var_dump($obj->get_value());

# Update the object and store, specify W and DW values...
$value = $obj->get_value();
$value->attr3 = 3;
$riak->store("bucket", "key1", $obj, 2, 2);

# Fetch again, specify R value...
$obj = $riak->fetch("bucket", "key1", 2);
print "Value: ";
var_dump($obj->get_value());

# Delete the object...
$riak->delete("bucket", "key1");



# -- STORE AND FETCH A RAW STRING -- 

# Store an object...
$riak->store_binary("bucket", "key2", "value");

# Fetch the object...
$obj = $riak->fetch_binary("bucket", "key2");
print "Value: " . $obj->get_value() . "\n";

# Update the object and store, specify W and DW values...
$obj->set_value("updated value");
$riak->store_binary("bucket", "key2", $obj, 2, 2);

# Fetch again, specify R value
$obj = $riak->fetch_binary("bucket", "key2", 2);
print "Value: " . $obj->get_value() . "\n";

# Delete the object...
$riak->delete("bucket", "key1");

?>