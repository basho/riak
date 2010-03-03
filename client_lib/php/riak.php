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


class RiakClient {
  function RiakClient($host='127.0.0.1', $port=8098, $prefix='riak') {
    $this->host = $host;
    $this->port = $port;
    $this->prefix = $prefix;    
    $this->clientid = 'php_' . base64_encode(rand(1, 1073741824));
    $this->r = 2;
    $this->w = 2;
    $this->dw = 2;
  }

  function getR()     { return $this->r; }
  function setR($r)   { $this->r = $r; }

  function getW()     { return $this->w; }
  function setW($w)   { $this->w = $w; }

  function getDW()    { return $this->dw; }
  function setDW($dw) { $this->dw = $dw; }

  function clientID() { return $this->clientid; }

  function bucket($name) {
    return new RiakBucket($this, $name);
  }

  function isAlive() {
    $url = 'http://' . $this->host . ':' . $this->port . '/ping';
    $response = RiakUtils::httpRequest('GET', $url);
    return ($response != NULL) && ($response[1] == 'OK');
  }
}

class RiakBucket {
  function RiakBucket($client, $name) {
    $this->client = $client;
    $this->name = $name;
    $this->r = NULL;
    $this->w = NULL;
    $this->dw = NULL;
  }

  function getR($r)     { 
    if ($r != NULL) return $r;
    if ($this->r != NULL) return $this->r;
    return $this->client->getR();
  }
  
  function setR($r)   { 
    $this->r = $r; 
  }

  function getW($w)     { 
    if ($w != NULL) return $w;
    if ($this->w != NULL) return $this->w;
    return $this->client->getW();
  }

  function setW($w)   { 
    $this->w = $w; 
  }

  function getDW($dw)    { 
    if ($dw != NULL) return $dw;
    if ($this->dw != NULL) return $this->dw;
    return $this->client->getDW();
  }

  function setDW($dw) { 
    $this->dw = $dw; 
  }

  function newObject($key, $data=NULL) {
    $obj = new RiakObject($this->client, $this, $key);
    $obj->data = $data;
    $obj->content_type = 'text/json';
    $obj->jsonify = TRUE;
    return $obj;
  }

  function newBinary($key, $data, $content_type='text/json') {
    $obj = new RiakObject($this->client, $this, $key);
    $obj->data = $data;
    $obj->content_type = $content_type;
    $obj->jsonify = FALSE;
    return $obj;
  }

  function get($key, $r=NULL) {
    $obj = new RiakObject($this->client, $this, $key);
    $obj->jsonify = TRUE;
    $r = $this->getR($r);
    return $obj->reload($r);
  }

  function getBinary($key, $r=NULL) {
    $obj = new RiakObject($this->client, $this, $key);
    $obj->jsonify = FALSE;
    $r = $this->getR($r);
    return $obj->reload($r);
  }

  function getOrNew($key, $data=NULL) {
    $obj = new RiakObject($this->client, $this, $key);
    $obj->jsonify = TRUE;
    $r = $this->getR($r);
    $obj.reload($r);
    if ($obj->exists()) {
      return $obj;
    } else {
      return $this->new($key, $data);
    }
  }

  function setNVal($nval) {
    return $this->setProperty("n_val", $nval);
  }

  function getNVal() {
    return $this->getProperty("n_val");
  }

  function setAllowMult($bool) {
    return $this->setProperty("allow_mult", $bool);
  }
  function getAllowMult() {
    return "true" == $this->getProperty("allow_mult");
  }

  function setProperty($key, $value) {
    return $this->setProperties(array($key=>$value));
  }

  function getProperty($key) {
    $props = $this->getProperties();
    if (array_key_exists($key, $props)) {
      return $props[$key];
    } else {
      return NULL;
    }
  }

  function setProperties($props) {
    # Construct the URL, Headers, and Content...
    $url = RiakUtils::buildRestPath($this->client, $this);
    $headers = array('Content-Type: application/json');
    $content = json_encode(array("props"=>$props));
    
    # Run the request...
    $response = RiakUtils::httpRequest('PUT', $url, $headers, $content);

    # Handle the response...
    if ($response == NULL) {
      throw Exception("Error setting bucket properties.");
    }
    
    # Check the response value...
    $status = $response[0]['http_code'];
    if ($status != 204) {
      throw Exception("Error setting bucket properties.");
    }
  }


  function getProperties() {
    # Run the request...
    $params = array('props' => 'true', 'keys' => 'false');
    $url = RiakUtils::buildRestPath($this->client, $this, NULL, NULL, $params);
    $response = RiakUtils::httpRequest('GET', $url);

    # Use a RiakObject to interpret the response, we are just interested in the value.
    $obj = new RiakObject($this->client, $this, NULL);
    $obj->populate($response, array(200));
    if (!$obj->exists()) {
      throw Exception("Error getting bucket properties.");
    }
    
    $props = $obj->getData();
    $props = $props["props"];

    return $props;
  }
}

class RiakObject {
  function RiakObject($client, $bucket, $key) {
    $this->client = $client;
    $this->bucket = $bucket;
    $this->key = $key;
    $this->jsonize = TRUE;
    $this->headers = array();
    $this->siblings = NULL;
    $this->exists = FALSE;
  }

  function getData() { return $this->data; }
  function setData($data) { $this->data = $data; }

  function status() {
    return $this->headers['http_code'];
  }

  function exists() {
    return $this->exists;
  }

  function getContentType() { 
    return $this->headers['content-type']; 
  }

  function setContentType($content_type) {
    $this->headers['content-type'] = $content_type;
  }

  function vclock() {
    if (array_key_exists('X-Riak-Vclock', $this->headers)) {
      return $this->headers['X-Riak-Vclock'];
    } else {
      return NULL;
    }
  }

  function store($w=NULL, $dw=NULL) {
    # Use defaults if not specified...
    $w = $this->bucket->getW($w);
    $dw = $this->bucket->getDW($w);

    # Construct the URL...
    $params = array('returnbody' => 'true', 'w' => $w, 'dw' => $dw);
    $url = RiakUtils::buildRestPath($this->client, $this->bucket, $this->key, NULL, $params);
    
    # Construct the headers...
    $headers = array('Content-Type: application/json',
                     'Accept: text/plain, */*; q=0.5',
                     'X-Riak-ClientId: ' . $this->client->clientID());

    # Add the vclock if it exists...
    if ($this->vclock() != NULL) {
      $headers[] = 'X-Riak-Vclock: ' . $this->vclock();
    }

    if ($this->jsonize) {
      $content = json_encode($this->getData());
    } else {
      $content = $this->getData();
    }

    # Run the operation.
    $response = RiakUtils::httpRequest('PUT', $url, $headers, $content);
    $this->populate($response, array(200, 300));
    return $this;
  }
 
  function reload($r=NULL) {
    # Do the request...
    $r = $this->bucket->getR($r);
    $params = array('r' => $r);
    $url = RiakUtils::buildRestPath($this->client, $this->bucket, $this->key, NULL, $params);
    $response = RiakUtils::httpRequest('GET', $url);
    $this->populate($response, array(200, 300, 404));
    
    # If there are siblings, load the data for the first one by default...
    if ($this->hasSiblings()) {
      $obj = $this->getSibling(0);
      $this->setData($obj->getData());
    }

    return $this;
  }

  function clear() {
      $this->headers = array();
      $this->data = NULL;
      $this->exists = FALSE;
      $this->siblings = NULL;
  }

  function populate($response, $expected_statuses) {
    $this->clear();

    # If no response given, then return.    
    if ($response == NULL) {
      return $this;
    }
  
    # Update the object...
    $this->headers = $response[0];
    $this->data = $response[1];
    $status = $this->status();

    # Check if the server is down (status==)
    if ($status == 0) {
      $m = 'Could not contact Riak Server: http://' . $this->client->host . ':' . $this->client->port . '!';
      throw new Exception($m);
    }

    # Verify that we got one of the expected statuses. Otherwise, throw an exception.
    if (!in_array($status, $expected_statuses)) {
      $m = 'Expected status ' . implode(' or ', $expected_statuses) . ', received ' . $status;
      throw new Exception($m);
    }

    # If 404 (Not Found), then clear the object.
    if ($status == 404) {
      $this->clear();
      return $this;
    }

    # If 300 (Siblings), then load the first sibling, and
    # store the rest.
    if ($status == 300) {
      $siblings = explode("\n", trim($this->data));
      array_shift($siblings); # Get rid of 'Siblings:' string.
      $this->siblings = $siblings;
      $this->exists = TRUE;
      return $this;
    }

    # Possibly json_decode...
    if ($status == 200 && $this->jsonize) {
      $this->data = json_decode($this->data, true);
      $this->exists = TRUE;
    }

    return $this;
  }

  function delete($dw=NULL) {
    # Use defaults if not specified...
    $dw = $this->bucket->getDW($dw);

    # Construct the URL...
    $params = array('dw' => $dw);
    $url = RiakUtils::buildRestPath($this->client, $this->bucket, $this->key, NULL, $params);

    # Run the operation...
    $response = RiakUtils::httpRequest('DELETE', $url);    
    $this->populate($response, array(204, 404));
    return $response;
  }

  function hasSiblings() {
    return ($this->getSiblingCount() > 0);
  }

  function getSiblingCount() {
    return count($this->siblings);
  }

  function getSibling($i, $r=NULL) {
    # Use defaults if not specified.
    $r = $this->bucket->getR($r);

    # Run the request...
    $vtag = $this->siblings[$i];
    $params = array('r' => $r, 'vtag' => $vtag);
    $url = RiakUtils::buildRestPath($this->client, $this->bucket, $this->key, NULL, $params);
    $response = RiakUtils::httpRequest('GET', $url);
    
    # Respond with a new object...
    $obj = new RiakObject($this->client, $this->bucket, $this->key);
    $obj->populate($response, array(200));
    return $obj;
  }

  function getSiblings() {
    $a = array();
    for ($i = 0; $i<$this->getSiblingCount(); $i++) {
      $a[] = $this->getSibling($i);
    }
    return $a;
  }
}

/**
 * Private class used to accumulate a CURL response.
 */
class RiakStringIO {
  function RiakStringIO() {
    $this->contents = '';
  }

  function write($ch, $data) {
    $this->contents .= $data;
    return strlen($data);
  }

  function contents() {
    return $this->contents;
  }
}

class RiakUtils {
  /**
   * Construct a REST path from client/bucket/object.
   * @param string $bucket - Name of the bucket.
   * @param string $key - Name of the key.
   * @param array $spec - Linkwalking spec (see the 'walk' function for format).
   * @param array $params - Params of the form array($key=>$value).
   * @returns string Combined path.
   */
  public static function buildRestPath($client, $bucket, $key=NULL, $spec=NULL, $params=NULL) {
    # Build 'http://hostname:port/prefix/bucket'
    $path = 'http://';
    $path.= $client->host . ':' . $client->port;
    $path.= '/' . $client->prefix;
    $path.= '/' . urlencode($bucket->name);
    
    # Add '.../key'
    if (!is_null($key)) {
      $path .= '/' . urlencode($key);
    }

    # Add '.../bucket,tag,acc/bucket,tag,acc'
    if (!is_null($spec)) {
      $s = '';
      foreach($spec as $el) {
	if ($s != '') $s .= '/';
	$s .= urlencode($el[0]) . ',' . urlencode($el[1]) . ',' . $el[2] . '/';
      }
      $path .= '/' . $s;
    }

    # Add query parameters.
    if (!is_null($params)) {
      $s = '';
      foreach ($params as $key => $value) {
	if ($s != '') $s .= '&';
	$s .= urlencode($key) . '=' . urlencode($value);
      }

      $path .= '?' . $s;
    }

    return $path;
  }

  /**
   * Run a Riak Request.
   * @param string $method 'GET', 'PUT', 'POST', or 'DELETE'
   * @param string $url URL of the request to run.
   * @param object $obj The string or object to store.
   * @param array $request_headers Request headers to set, of the form array('Header: Value')
   * @return RiakResponse object
   */
  public static function httpRequest($method, $url, $request_headers = array(), $obj = '') {
    # Set up curl
    $ch = curl_init();
    curl_setopt($ch, CURLOPT_URL, $url);
    curl_setopt($ch, CURLOPT_HTTPHEADER, $request_headers);

    if ($method == 'GET') {
      curl_setopt($ch, CURLOPT_HTTPGET, 1);
    } else if ($method == 'POST') {
      curl_setopt($ch, CURLOPT_POST, 1);
      curl_setopt($ch, CURLOPT_POSTFIELDS, $obj);
    } else if ($method == 'PUT') {
      curl_setopt($ch, CURLOPT_CUSTOMREQUEST, 'PUT');
      curl_setopt($ch, CURLOPT_POSTFIELDS, $obj);
    } else if ($method == 'DELETE') {
      curl_setopt($ch, CURLOPT_CUSTOMREQUEST, 'DELETE');
    }

    # Capture the response headers...
    $response_headers_io = new RiakStringIO();
    curl_setopt($ch, CURLOPT_HEADERFUNCTION, array(&$response_headers_io, 'write'));

    # Capture the response body...
    $response_body_io = new RiakStringIO();
    curl_setopt($ch, CURLOPT_WRITEFUNCTION, array(&$response_body_io, 'write'));

    try {
      # Run the request.
      curl_exec($ch);
      $curl_headers = curl_getinfo($ch);
      curl_close($ch);

      # Merge the headers...
      $parsed_headers = RiakUtils::parseHttpHeaders($response_headers_io->contents());
      $response_headers = array_merge($curl_headers, $parsed_headers);
      
      # Pull out the body...
      $response_body = $response_body_io->contents();
      $array = array();
      $success = mb_parse_str($response_body, $array);

      # Return a new RiakResponse object.
      return array($response_headers, $response_body);
    } catch (Exception $e) {
      curl_close($ch);
      error_log('Error: ' . $e->getMessage());
      return NULL;
    } 
  }

  /**
   * Parse an HTTP Header string.
   * @param  string $header - Header string.
   * @return An array of headers of the form array(array('Header', 'Value'))
   */
  static function parseHttpHeaders($headers) {
    $retVal = array();
    $fields = explode("\r\n", preg_replace('/\x0D\x0A[\x09\x20]+/', ' ', $headers));
    foreach( $fields as $field ) {
      if( preg_match('/([^:]+): (.+)/m', $field, $match) ) {
        $match[1] = preg_replace('/(?<=^|[\x09\x20\x2D])./e', 'strtoupper("\0")', strtolower(trim($match[1])));
        if( isset($retVal[$match[1]]) ) {
          $retVal[$match[1]] = array($retVal[$match[1]], $match[2]);
        } else {
          $retVal[$match[1]] = trim($match[2]);
        }
      }
    }
    return $retVal;
  }
}


?>