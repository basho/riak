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

# Not Yet Implemented:
# - Links/LinkWalking
# - Support for siblings when a bucket has allow_mult = true

class RiakClient {

  /**
   * Create a new RiakClient object.
   * @param string $host - Hostname or IP address.
   * @param int $port - Port number (default 8098)
   * @param string $prefix - Interface prefix (default "raw")
   */
  function RiakClient($host, $port=8098, $prefix="raw") {
    $this->host = $host;
    $this->port = $port;
    $this->prefix = $prefix;    
    $this->clientid = "php_" . base64_encode(rand(1, 1073741824));
  }

  /**
   * Store an object in Riak in JSON format.
   * @param  string $bucket - Name of the bucket.
   * @param  string $key - Name of the key.
   * @param  object $obj - An object to serialize as JSON.
   * @param  array  $links - An array of arrays of the form 
                             array(array("bucket", "key", "tag"), ...)
   * @param  int $w - W-value, wait for W partitions to acknowledge operation.
   * @param  int $dw - DW-value, wait for DW partitions to complete operation.
   * @return RiakResponse object.
   */
  function store($bucket, $key, $obj, $w=2, $dw=2) {
    return $this->_store($bucket, $key, $obj, $w, $dw, TRUE);
  }

  /**
   * Store an object in Riak in binary (string) format.
   * @param  string $bucket - Name of the bucket.
   * @param  string $key - Name of the key.
   * @param  object $obj - An object to serialize as JSON.
   * @param  array  $links - An array of arrays of the form 
                             array(array("bucket", "key", "tag"), ...)
   * @param  int $w - W-value, wait for W partitions to acknowledge operation.
   * @param  int $dw - DW-value, wait for DW partitions to complete operation.
   * @return RiakResponse object.
   */
  function store_binary($bucket, $key, $obj, $w=2, $dw=2) {
    return $this->_store($bucket, $key, $obj, $w, $dw, FALSE);
  }

  /**
   * Store an object in Riak.
   */
  private function _store($bucket, $key, $obj, $w, $dw, $json_encode) {
    # Construct the URL...
    $params = array("returnbody" => "true", "w" => $w, "dw" => $dw);
    $url = $this->_build_path($bucket, $key, NULL, $params);

    # Construct the headers...
    $headers = array("Content-Type: application/json",
		    "X-Riak-ClientId: " . $this->clientid);

    if ($obj instanceof RiakResponse) {
      $headers[] = "X-Riak-Vclock: " . $obj->vclock();
    }

    # Construct the body...
    if ($obj instanceof RiakResponse) {
      $content = $obj->get_value();
    } else {
      $content = $obj;
    }

    # Possibly encode as JSON.
    if ($json_encode) {
      $content = json_encode($content);
    }

    # Run the operation.
    $response = $this->_request("PUT", $url, $headers, $content);
    $response->assert_status(200);
    return $response;
  }

  /**
   * Fetch an object from Riak, decode as JSON.
   * @param  string $bucket - Name of the bucket.
   * @param  string $key - Name of the key.
   * @param  int $r - R-value, wait for R partitions to respond before 
                      completing the operation.
   * @return RiakResponse object.
   */
  function fetch($bucket, $key, $r=2) {
    return $this->_fetch($bucket, $key, $r, TRUE);
  }

  /**
   * Fetch an object from Riak, in binary (string) format.
   * @param  string $bucket - Name of the bucket.
   * @param  string $key - Name of the key.
   * @param  int $r - R-value, wait for R partitions to respond before 
                      completing the operation.
   * @return RiakResponse object.
   */
  function fetch_binary($bucket, $key, $r=2) {
    return $this->_fetch($bucket, $key, $r, FALSE);
  }

  /**
   * Fetch an object from Riak.
   * @param  string $bucket - Name of the bucket.
   * @param  string $key - Name of the key.
   * @param  int $r - R-value, wait for R partitions to respond before 
                      completing the operation.
   * @return RiakResponse object.
   */
  function _fetch($bucket, $key, $r=2, $json_decode) {
    # Do the request...
    $params = array("r" => $r);
    $url = $this->_build_path($bucket, $key, NULL, $params);
    $response = $this->_request("GET", $url);

    # For now, error if this object has siblings...
    if ($response->status() == 300) {
      $m = "Buckets with allow_mult=true not yet implemented!";
      throw new Exception($m);
    }

    # Ensure the proper status...
    $response->assert_status(200, 404);
    
    # NULL value if 404...
    if ($response->status() == 404) {
      $response->set_value(NULL);
    } 

    # Possibly json_decode...
    if ($response->status() == 200 && $json_decode) {
      $response->set_value(json_decode($response->get_value()));
    }

    return $response;
  }

  /** 
   * Delete an object from Riak.
   * @param  string $bucket - Name of the bucket.
   * $param  string $key - Name of the key.
   * $param  int $dw - DW-value, wait for DW partitions to acknowledge 
                       operation before returning.
   * @return RiakResponse object.
   */
  function delete($bucket, $key, $dw=2) {
    $params = array("dw" => $dw);
    $url = $this->_build_path($bucket, $key, NULL, $params);
    $response = $this->_request("DELETE", $url);
    $response->assert_status(204, 404);
    return $response;
  }

  /**
   * Run a linkwalking operation and return results.
   * @param  string $bucket - Name of the bucket.
   * @param  string $key - Name of the key.
   * @param  array $spec - Link spec of the form array(array("bucket", "tag", $acc)) where 
                           $acc is true or false. "bucket" or "tag" can replaced by underscore 
			   wildcard (_).
   * @return RiakResponse object.
   */
  function walk($bucket, $key, $spec) {
    $url = $this->_build_path($bucket, $key, $spec);
    $response = $this->_request("GET", $url);
    $response->assert_status(200, 404);
    return $response;
  }

  /**
   * Return information about a bucket.
   * @param string $bucket - Name of the bucket
   * @param bool $list_props - Return bucket properties
   * @param bool $list_keys - Return the keys in the bucket (expensive!)
   */
  function list_bucket($bucket, $list_props=true, $list_keys=false) {
    $url = $this->_build_path($bucket);
    $response = $this->_request("GET", $url);
    $response->assert_status(200);
    return $response;
  }

  /**
   * Set the properties on a bucket.
   * @param  string $bucket - Name of the bucket
   * @param  array @properties - Array of prop=>value
   * @return Updated bucket properties.
   */
  function set_bucket($bucket, $properties) {
    $url = $this->_build_path($bucket);
    $headers = array("Content-Type: application/json");
    $json_properties = json_encode(array("props"=>$properties));
    $response = $this->_request("PUT", $url, $headers, $json_properties);
    $response->assert_status(204);
    return $response;
  }

  /**
   * Construct a path to Riak.
   * @param string $bucket - Name of the bucket.
   * @param string $key - Name of the key.
   * @param array $spec - Linkwalking spec (see the 'walk' function for format).
   * @param array $params - Params of the form array($key=>$value).
   * @returns string Combined path.
   */
  private function _build_path($bucket, $key=NULL, $spec=NULL, $params=NULL) {
    # Build "http://hostname:port/prefix/bucket"
    $path = "http://";
    $path.= $this->host . ":" . $this->port;
    $path.= "/" . $this->prefix;
    $path.= "/" . urlencode($bucket);
    
    # Add ".../key"
    if (!is_null($key)) {
      $path .= "/" . urlencode($key);
    }

    # Add ".../bucket,tag,acc/bucket,tag,acc"
    if (!is_null($spec)) {
      $s = "";
      foreach($spec as $el) {
	if ($s != "") $s .= "/";
	$s .= urlencode($el[0]) . "," . urlencode($el[1]) . "," . $el[2] . "/";
      }
      $path .= "/" . $s;
    }

    # Add query parameters.
    if (!is_null($params)) {
      $s = "";
      foreach ($params as $key => $value) {
	if ($s != "") $s .= "&";
	$s .= urlencode($key) . "=" . urlencode($value);
      }

      $path .= "?" . $s;
    }

    return $path;
  }

  /**
   * Run a Riak Request.
   * @param string $method "GET", "PUT", "POST", or "DELETE"
   * @param string $url URL of the request to run.
   * @param object $obj The string or object to store.
   * @param array $request_headers Request headers to set, of the form array("Header: Value")
   * @return RiakResponse object
   */
  private function _request($method, $url, $request_headers = array(), $obj = "") {
    # Set up curl
    $ch = curl_init();
    curl_setopt($ch, CURLOPT_URL, $url);
    curl_setopt($ch, CURLOPT_HTTPHEADER, $request_headers);

    if ($method == "GET") {
      curl_setopt($ch, CURLOPT_HTTPGET, 1);
    } else if ($method == "POST") {
      curl_setopt($ch, CURLOPT_POST, 1);
      curl_setopt($ch, CURLOPT_POSTFIELDS, $obj);
    } else if ($method == "PUT") {
      curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "PUT");
      curl_setopt($ch, CURLOPT_POSTFIELDS, $obj);
    } else if ($method == "DELETE") {
      curl_setopt($ch, CURLOPT_CUSTOMREQUEST, "DELETE");
    }

    # Capture the response headers...
    $response_headers_io = new RiakStringIO();
    curl_setopt($ch, CURLOPT_HEADERFUNCTION, array(&$response_headers_io, "write"));

    # Capture the response body...
    $response_body_io = new RiakStringIO();
    curl_setopt($ch, CURLOPT_WRITEFUNCTION, array(&$response_body_io, "write"));

    try {
      # Run the request.
      curl_exec($ch);
      $curl_headers = curl_getinfo($ch);
      curl_close($ch);

      # Merge the headers...
      $parsed_headers = $this->_http_parse_headers($response_headers_io->contents());
      $response_headers = array_merge($curl_headers, $parsed_headers);
      
      # Pull out the body...
      $response_body = $response_body_io->contents();

      # Return a new RiakResponse object.
      return new RiakResponse($response_headers, $response_body);
    } catch (Exception $e) {
      curl_close($ch);
      return NULL;
    } 
  }

  /**
   * Parse an HTTP Header string.
   * @param  string $header - Header string.
   * @return An array of headers of the form array(array("Header", "Value"))
   */
  function _http_parse_headers($header) {
    $retVal = array();
    $fields = explode("\r\n", preg_replace('/\x0D\x0A[\x09\x20]+/', ' ', $header));
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

/**
 * Private class used to accumulate a CURL response.
 */
class RiakStringIO {
  function RiakStringIO() {
    $this->contents = "";
  }

  function write($ch, $data) {
    $this->contents .= $data;
    return strlen($data);
  }

  function contents() {
    return $this->contents;
  }
}

/** 
 * Response from a Riak request.
 */
class RiakResponse {
  function RiakResponse($headers, $value, $json_encode=TRUE) {
    $this->headers = $headers;
    $this->value = $value;
    $this->json_encode = true;
  }

  function status() {
    return $this->headers["http_code"];
  }

  function vclock() {
    return $this->headers["X-Riak-Vclock"];
  }

  function headers() {
    return $this->headers;
  } 

  function get_value() {
    return $this->value;
  }
  
  function set_value($value) {
    $this->value = $value;
  }

  function assert_status() {
    # Return true if our status matches one of the specified statuses.
    # Otherwise, throw an exception.
    $a = func_get_args();
    foreach ($a as $s) {
      if ($this->status() == $s) {
	return $this;
      }
    }

    # Error if we get here.
    $m = "Expected status " . implode(" or ", $a) . ", received " . $this->status();
    throw new Exception($m);
  }
}

?>