<?php

class JiakClient {
    function JiakClient($jiakIP, $jiakPort) {
        $this->jiakIP = $jiakIP;
        $this->jiakPort = $jiakPort;
        $this->JKP = "/jiak/";
        $this->JWP = "/jaywalker/";
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
    
    function bucket_info($bucket) {
        return $this->_expect(200, $this->_do_req("GET", $this->JKP . $bucket));
    }
    
    function store_all($ar) {
        foreach($ar as $o) {
            $this->store($o);
        }
    }
    
    function store($obj) {
        $new_data = $this->_expect(200,
            $this->_do_req("PUT",
                $this->JKP . $obj->bucket . "/" . $obj->key .
                "?returnbody=true",
                $obj->to_json(),
                array("Content-type: application/json;charset=UTF-8")));
        $obj->update($new_data);
        return $obj;
    }
    
    function fetch($bucket, $key) {
        $resp = $this->_do_req("GET", $this->JKP . $bucket . "/" . $key);
        if ($resp['http_code'] == 404) {
            return null;
        }
        $data = $this->_expect(200, $resp);
        return $this->_make_object($data);
    }
    
    function delete($bucket, $key) {
        $resp = $this->_do_req("DELETE", $this->JKP .
            $bucket . "/" . $key);
        $http_code = $resp['http_code'];
        if ($http_code == 404) return false;
        else if ($http_code == 204) return true;
        throw Exception("delete: " . $http_code . ": " . $resp['resp']);
    }
    
    function walk($bucket, $key, $spec) {
        $resp = $this->_do_req("GET", $this->JKP . $bucket . "/" . $key . "/" .
            $this->_convert_walk_spec($spec));
        
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
        $s = "/";
        foreach($spec as $el) {
            $s .= $el[0] . "," . $el[1] . "," . $el[2] . ",";
        }
        $s = str_replace(",\n", "", $s."\n");
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


/*****

    // jiak.php example
    //
    // this example only works if you
    // have a running riak cluster with:
    //
    //  {riak_web_ip, "127.0.0.1"}.
    //  {riak_web_port, 8999}.
    //
    // (add to config/your-config.erlenv
    //
    
    try {
        $riak_ip = "127.0.0.1";
        $riak_port = 8999;
        
        $demo_bucket = "jiak_example";
        $demo_key = "phptest";
    
        $JC = new JiakClient($riak_ip, $riak_port);
    
        $obj = $JC->fetch($demo_bucket, $demo_key);
        
        if ($obj === null) {
            print ("No object in " . $demo_bucket . " for key '" . $demo_key . 
                "', so I'll create it.\n");
            $jiak_obj = new JiakObject($demo_bucket, $demo_key);
            $jiak_obj->set("foo", "value of foo!");
            
            // ** note: you will want to use the object returned from a
            //    call to store; this object will include vclock, lastmod, etc.
            $jiak_obj = $JC->store($jiak_obj);
        }
        
        print ("Object stored in bucket '" . $demo_bucket . " with key " .
            "'" . $demo_key . "': ");
        $obj = $JC->fetch($demo_bucket, $demo_key);
        print_r($obj);
        
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
        
        $obj->add_link("jiak_example", $kleaf1, "tag_one");
        $obj->add_link("jiak_example", $kleaf2, "tag_one");
        $obj->add_link("jiak_example", $kleaf3, "tag_other");
        
        $JC->store_all(array($leaf1, $leaf2, $leaf3, $obj));
        
        print ("\n\nNow-linked value of '" . $demo_bucket . "':'" .
            $demo_key . "':\n");
        
        $obj = $JC->fetch($demo_bucket, $demo_key);
        print_r($obj);
        
        print ("\nTesting walk with parameters '" . $demo_bucket . "'" .
            ", 'tag_one'...\n");
        $ar = $JC->walk($demo_bucket, $demo_key,
            array( array("jiak_example", "tag_one", 1) ));
        print_r($ar);
        
        print ("Removing objects...");
        $JC->delete($demo_bucket, $demo_key);
        $JC->delete($demo_bucket, $kleaf1);
        $JC->delete($demo_bucket, $kleaf2);
        $JC->delete($demo_bucket, $kleaf3);
        
        $val = $JC->fetch($demo_bucket, $demo_key);
        if ($val === null) {
            print ("\nObject deleted successfully!\n\n");
        }
    } catch (Exception $ex) {
        print ("\n\nException: \n\n");
        print_r($ex);
    }

*****/

?>
