<?php

  /* 
    This file is provided to you under the Apache License,
    Version 2.0 (the "License"); you may not use this file
    except in compliance with the License.  You may obtain
    a copy of the License at
   
      http://www.apache.org/licenses/LICENSE-2.0
   
    Unless required by applicable law or agreed to in writing,
    software distributed under the License is distributed on an
    "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
    KIND, either express or implied.  See the License for the
    specific language governing permissions and limitations
    under the License.    
  */

class JiakClient {
    function JiakClient($jiakIP, $jiakPort) {
        $this->jiakIP = $jiakIP;
        $this->jiakPort = $jiakPort;
        $this->JKP = "/jiak/";
    }
    
    function _do_req($method, $uri, $body = "", $header = array()) {
        $ch = curl_init();
        $url = "http://" . $this->jiakIP . ":" . $this->jiakPort . $uri;
        
        // Disable 100-continue header
        $header[] = 'Expect:';
        
        curl_setopt($ch, CURLOPT_URL, $url);
        curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
        curl_setopt($ch, CURLOPT_HTTPHEADER, $header);
        
        if ($method == "GET") {
            curl_setopt($ch, CURLOPT_HTTPGET, 1);
        
        } else if ($method == "POST") {
            curl_setopt($ch, CURLOPT_POST, 1);
            curl_setopt($ch, CURLOPT_POSTFIELDS, $body);
        
        } else if ($method == "PUT") {
            curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "PUT");
            curl_setopt($ch, CURLOPT_POSTFIELDS, $body);
        
        } else if ($method == "DELETE") {
            curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "DELETE");
        }
        
        $resp = curl_exec($ch);
        $http_code = curl_getinfo($ch, CURLINFO_HTTP_CODE);
        curl_close($ch);
        return array("resp" => $resp, "http_code" => $http_code);
    }
    
    function _expect($status, $data) {
        $http_code = $data['http_code'];
        $resp = $data['resp'];
        if ($http_code != $status) {
            throw new Exception ("_do_req: " . $http_code . ": " . $resp);
        }
        return json_decode($resp, true);
    }
    
    function set_bucket_schema($bucket, $allowed_fields, 
        $required_fields = array(),
        $write_mask = null, $read_mask = null) {
        if ($write_mask == null)
            $write_mask = $allowed_fields;
        if ($read_mask == null)
            $read_mask = $allowed_fields;
        return $this->_expect(204,
            $this->_do_req("PUT", $this->JKP . urlencode($bucket),
                json_encode( array (
                    "schema" => array (
                        "allowed_fields" => $allowed_fields,
                        "required_fields" => $required_fields,
                        "write_mask" => $write_mask,
                        "read_mask" => $read_mask))),
                array("Content-type: application/json")));
    }
    
    function bucket_info($bucket) {
        return $this->_expect(200,
            $this->_do_req("GET", $this->JKP . urlencode($bucket)));
    }
    
    function store_all($ar, $w=2, $dw=2) {
        foreach($ar as $o) {
            $this->store($o, $w, $dw);
        }
    }
    
    function store($obj, $w=2, $dw=2) {
    	$obj_json = $obj->to_json();
        $new_data = $this->_expect(200,
            $this->_do_req("PUT",
                $this->JKP . urlencode($obj->bucket) . "/" .
                urlencode($obj->key) . "?returnbody=true" .
                	"&w=" . $w .
                	"&dw=" . $dw,
                $obj_json,
                array("Content-type: application/json;charset=UTF-8")));
        $obj->update($new_data);
        return $obj;
    }
    
    function fetch($bucket, $key, $r=2) {
        $resp = $this->_do_req("GET",
                    $this->JKP . urlencode($bucket) . "/" . 
                    urlencode($key) .
                    "?r=" . $r);
        if ($resp['http_code'] == 404) {
            return null;
        }
        $data = $this->_expect(200, $resp);
        return $this->_make_object($data);
    }

    function list_bucket($bucket) {
        $o = $this->get_bucket_metadata($bucket, true);
        return $o['keys'];
    }

    function get_bucket_schema($bucket) {
        $o = $this->get_bucket_metadata($bucket, false);
        return $o['schema'];
    }

    function get_bucket_metadata($bucket, $with_keys) {
        $resp = $this->_do_req("GET",
                    $this->JKP . urlencode($bucket) . "/" .
                    ($with_keys===true?"":"?keys=false"));
        if ($resp['http_code'] == 404) {
            return null;
        }
        $data = $this->_expect(200, $resp);
        return $data;
    }
    
    function delete($bucket, $key, $dw=2) {
        $resp = $this->_do_req("DELETE", $this->JKP .
                    urlencode($bucket) . "/" . urlencode($key) .
                    "&dw=" . $dw);
        $http_code = $resp['http_code'];
        if ($http_code == 404) return false;
        else if ($http_code == 204) return true;
        throw Exception("delete: " . $http_code . ": " . $resp['resp']);
    }
    
    function walk($bucket, $key, $spec) {
        $resp = $this->_do_req("GET", $this->JKP . urlencode($bucket) .
            "/" . urlencode($key) . "/" . $this->_convert_walk_spec($spec));
        
        if ($resp['http_code'] == 404) {
            return array();
        }
        
        $data = $this->_expect(200, $resp);
        $objlists = $data['results'];
        return $this->_convert_objlists($objlists);
    }
    
    function _convert_objlists($objlists) {
        $ar = array();
        foreach($objlists as $objlist) {
            foreach($objlist as $obj) {
                $ar[] = $this->_make_object($obj);
            }
        }
        return $ar;
    }
    
    function _make_object($data) {
        $o = new JiakObject($data['bucket'], $data['key']);
        $o->update($data);
        return $o;
    }
    
    function _convert_walk_spec($spec) {
        $s = "";
        foreach($spec as $el) {
            $s .= urlencode($el[0]) . "," . urlencode($el[1]) . "," . $el[2] . "/";
        }
        return $s;
    }
}

class JiakObject {
    function JiakObject($bucket, $key, 
        $links = array(), $obj = array()) {
        $this->bucket = $bucket;
        $this->key = $key;
        $this->links = $links;
        $this->object = $obj;
    }
    
    function update($data) {
        $this->object = $data['object'];
        $this->links = $data['links'];
        $this->vtag = $data['vtag'];
        $this->lastmod = $data['lastmod'];
        $this->vclock = $data['vclock'];
    }
    
    function set($k, $v) {
        $this->object[$k] = $v;
    }
    
    function get($k) {
        return $this->object[$k];
    }
    
    function add_link($bucket, $key, $type) {
        $this->links[] = array($bucket, $key, $type);
    }
    
    function to_json() {
        $json = json_encode($this);
        return $json;
    }
}


/*

    // jiak.php example
    //
    // this example only works if you
    // have a running riak cluster with:
    //
    //  {riak_web_ip, "127.0.0.1"}.
    //  {riak_web_port, 8098}.
    //
    // (add to config/your-config.erlenv
    //
    
    try {
        //
        // setup, connect
        //
    
        $riak_ip = "127.0.0.1";
        $riak_port = 8098;
        
        $demo_bucket = "jiak_example";
        $demo_key = "phptest";

        $JC = new JiakClient($riak_ip, $riak_port);
        
        //
        // simple set_bucket, fetch, store
        //
        
        $JC->set_bucket_schema("jiak_example", array("foo", "bar", "baz"));
        $obj = $JC->fetch($demo_bucket, $demo_key);
        print_r($obj);
        
        if ($obj === null) {
            print ("No object in " . $demo_bucket . " for key '" . $demo_key . 
                "', so I'll create it.\n");
            $jiak_obj = new JiakObject($demo_bucket, $demo_key);
            $jiak_obj->set("foo", "value of foo!");

            // ** note: you will want to use the object returned from a
            //    call to store; this object will include vclock, lastmod, etc.
            $obj = $JC->store($jiak_obj);
        }
        
        print ("Object stored in bucket '" . $demo_bucket . " with key " .
            "'" . $demo_key . "': ");
        $obj = $JC->fetch($demo_bucket, $demo_key);
        print_r($obj);
        
        //
        // links
        //
        
        // clear links
        $obj->links = array();
        
        print ("Creating leaf1, leaf2, leaf3 and linking to them from " .
            $demo_key . "\n\n");
        
        $kleaf1 = "leaf1" . rand(0,100000);
        $kleaf2 = "leaf2" . rand(0,100000);
        $kleaf3 = "leaf3" . rand(0,100000);
        
        $leaf1 = new JiakObject($demo_bucket, $kleaf1);
        $leaf1->set("foo", "foobie");
        
        $leaf2 = new JiakObject($demo_bucket, $kleaf2);
        $leaf2->set("foo", "bletch");
        
        $leaf3 = new JiakObject($demo_bucket, $kleaf3);
        $leaf3->set("foo", "good news & a snack!");
        
        print_r($obj);
        $obj->add_link("jiak_example", $kleaf1, "tag_one");
        $obj->add_link("jiak_example", $kleaf2, "tag_one");
        $obj->add_link("jiak_example", $kleaf3, "tag_other");
        
        $JC->store_all(array($leaf1, $leaf2, $leaf3, $obj));
        
        print ("\n\nNow-linked value of '" . $demo_bucket . "':'" .
            $demo_key . "':\n");
        
        $obj = $JC->fetch($demo_bucket, $demo_key);
        print_r($obj);
        
        //
        // link-walking via "jaywalker"
        //
        
        print ("\nTesting walk with parameters '" . $demo_bucket . "'" .
            ", 'tag_one'...\n");
        $ar = $JC->walk($demo_bucket, $demo_key,
            array( array("jiak_example", "tag_one", 1) ));
        print_r($ar);
        
        print ("Removing objects...\n\n");
        $JC->delete($demo_bucket, $demo_key);
        $JC->delete($demo_bucket, $kleaf1);
        $JC->delete($demo_bucket, $kleaf2);
        $JC->delete($demo_bucket, $kleaf3);
        
        $val = $JC->fetch($demo_bucket, $demo_key);
        if ($val === null) {
            print ($demo_bucket . ":" . $demo_key . " deleted successfully!\n\n");
        }
        
        //
        // dynamic bucket creation via set_bucket_schema
        //
        
        print ("\nTesting set_bucket_schema('jiak_test', ...)\n\n");
        $o = $JC->set_bucket_schema("jiak_test", array("name", "title", "description"));
        print_r($o);
        
        print ("Adding record to jiak_test ...\n");
        $jiak_test = new JiakObject("jiak_test", "test_object");
        $jiak_test->set("name", "Bob");
        $jiak_test->set("title", "SVP Megacorp");
        $jiak_test->set("description", "Bob looks forward to learning Erlang.");
        $JC->store($jiak_test);
        
        print ("\n\nFetching jiak_test object..\n");
        $jto = $JC->fetch("jiak_test", "test_object");
        print_r($jto);
        
        print ("\nDeleting jiak_test object ('test_object')...\n");
        $JC->delete("jiak_test", "test_object");
 
        print ("\nListing keys for bucket '" . $demo_bucket . "'...\n");
        print_r($JC->list_bucket($demo_bucket));

        print ("\nGetting jiak bucket schema for bucket '" . $demo_bucket . "'...\n");
        print_r($JC->get_bucket_schema($demo_bucket));
       
        print ("\nDone!\n\n");
        
    } catch (Exception $ex) {
        print ("\n\nException: \n\n");
        print_r($ex);
    }
*/

?>
