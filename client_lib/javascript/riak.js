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

/**
 * Utility functions which don't belong anywhere else
 */
var RiakUtil = function() {
  return {
    /**
     * Base64 encode a number
     * @param num - Number to encode
     * @return string containing base64 encoded number
     */
    base64Encode: function(num) {
      var base64digits = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
        return base64digits[(num >>> 26)] + base64digits[((num >>> 20)&63)] +
               base64digits[((num >>> 14)&63)] + base64digits[((num >>> 8)&63)] +
               base64digits[((num >>> 2)&63)] + base64digits[((num << 4)&63)] + '==';
    },
    /**
     * Trim spaces from beginning/end of text
     * @param text - Text to trimg
     * @return string with leading & trailing spaces removed
     */
    trim: function(text) {
      var tmp = text.replace(/^\s+/, '');
      return tmp.replace(/\s+$/, '');
    }
  };
}();

/**
 * Models an entry in a Riak bucket
 * @param bucketName - Riak bucket name
 * @param key - Object's key
 * @param client - Owning RiakClient
 * @param body - Object's data
 * @param contentType - Mime type associated with data
 * @param vclock - Riak-assigned vclock
 */
function RiakObject(bucketName, key, client, body, contentType, vclock) {
  if (client == null) {
    throw("Cannot construct RiakObject without a client reference");
  }
  this.bucket = bucketName;
  this.key = key;
  this.client = client;
  this.body = body;
  this.contentType = contentType;
  this.vclock = vclock;
};

RiakObject._buildObject = function(req, callback, createEmpty) {
  if (req.readyState != 4) {
    return;
  }
  if (callback !== null) {
    var riakObject = null;
    if (req.status == 200) {
	if (req.status == 200) {
	  var vclock = req.getResponseHeader('X-Riak-Vclock');
	  var contentType = req.getResponseHeader('Content-Type');
	  var links = req.getResponseHeader('Link');
	  var body = req.responseText;
	  riakObject = new RiakObject(this.name, key, this.client, body, contentType, vclock);
    }
    else if (req.status == 404 && createEmpty === true) {
      riakObject = new RiakObject(bucket.name, key, bucket.client);
    }
    callback(riakObject, req);
  }
};

/**
 * Store the object in Riak
 * @param callback - Function to call when op completes
 */
RiakObject.prototype.store = function(callback) {
  if (this.contentType == null) {
    throw('RiakObject missing contentType');
  }
  var req =  this.client._newAjaxRequest('PUT', bucket, key);
  req.setRequestHeader('Content-Type', this.contentType);
  if (this.vclock !== null) {
    req.setRequestHeader('X-Riak-Vclock', this.vclock);
  }
  req.onreadystatechange = function() { RiakObject._buildObject(req, callback, false); };
};

/**
 * Models a Riak bucket
 * @param bucket - Riak bucket name
 * @param client - RiakClient reference
 */
function RiakBucket(bucket, client, props) {
  if (client == null) {
    throw("Cannot construct RiakBucket without client reference");
  }
  this.name = bucket;
  this.client = client;
  if (props == null) {
    this.props = {};
  }
  else {
    this.props = props.props;
  }
};

/**
 * Sets/gets the nValue for this bucket
 * @param n -- New nValue (optional)
 * @return the current nValue
 */
RiakBucket.prototype.nValue = function(n) {
  var retval = this.props.n_val;
  if (n !== undefined) {
    this.props.n_val = n;
    retval = n;
  }
  return retval;
};

/**
 * Enables/disables multiple bucket entries
 * @param flag -- true or false
 * @return the current setting
 */
RiakBucket.prototype.allowsMultiples = function(flag) {
  var retval = this.props.allow_mult;
  if (flag !== undefined) {
    this.props.allow_mult = flag;
    retval = flag;
  }
  return retval;
};

/**
 * Retrieves an object from the bucket
 * @param key - Riak key name
 * @param callback - Function to call when op has completed
 */
RiakBucket.prototype.get_or_new = function(key, callback) {
  var req = this.client._newAjaxRequest('GET', this.name, key);
  var bucket = this;
  req.onreadystatechange = function() { RiakObject._buildObject(req, callback, true); };
  req.send(null);
};

/**
 * Stores bucket
 * @param callback - Function call when op has completed
 */
RiakBucket.prototype.store = function(callback) {
  this.client._storeBucket(this, callback);
};

/** Start RiakBucket internals **/

/** End RiakBucket internals **/


/**
 * Entry point for interacting with Riak
 * @param baseUrl - URL for 'raw' interface (optional, default: '/raw/')
 */
function RiakClient(baseUrl) {
  if (baseUrl == null) {
    baseUrl = '/riak/';
  }
  else {
    if (baseUrl[0] !== '/') {
      baseUrl = '/' + baseUrl;
    }
    if ((baseUrl.slice(-1) !== '/')) {
      baseUrl += '/';
    }
  }
  this.baseUrl = baseUrl;
  this.clientId = "js_" + RiakUtil.base64Encode(Math.floor(Math.random() * 4294967296));
};

/**
 * Fetches a bucket from Riak
 * @param bucket Riak bucket name
 * @param callback Function to call when op completes
 */
RiakClient.prototype.bucket = function(bucket, callback) {
  var req = this._newAjaxRequest('GET', bucket);
  req.setRequestHeader('Content-Type', 'application/json');
  var client = this;
  req.onreadystatechange = function() { client._buildBucket(bucket, req, callback, false); };
  req.send(null);
};

/**
 * Fetches a bucket from Riak or creates a new one if it doesn't exist
 * @param bucket Riak bucket name
 * @param callback Function call when op completes
 */
RiakClient.prototype.bucket_or_new = function(bucket, callback) {
  var req = this._newAjaxRequest('GET', bucket);
  req.setRequestHeader('Content-Type', 'application/json');
  var client = this;
  req.onreadystatechange = function() { client._buildBucket(bucket, req, callback, true); };
  req.send(null);
};

/** Begin RiakClient internal functions **/

RiakClient.prototype._storeBucket = function(bucket, callback) {
  var req = this._newAjaxRequest('PUT', bucket.name);
  req.setRequestHeader('Content-Type', 'application/json');
  var props = {};
  props.props = bucket.props;
  var client = this;
  req.onreadystatechange = function() { client._handleStore(bucket, req, callback); };
  req.send(JSON.stringify(props));
};

RiakClient.prototype._handleStore = function(bucket, req, callback) {
  if (req.readyState != 4) {
    return;
  }
  if (callback !== null) {
    if (req.status == 200) {
      callback(bucket, req);
    }
    else {
      callback(null, req);
    }
  }
};

RiakClient.prototype._buildBucket = function(bucketName, req, callback, createEmpty) {
  var bucket = null;
  if (req.readyState != 4) {
    return;
  }
  if (callback !== null) {
    if (req.status == 200) {
      var body = req.responseText;
      bucket = new RiakBucket(bucketName, this, JSON.parse(body));
    }
    else if (req.status == 404 && createEmpty === true) {
      bucket = new RiakBucket(bucketName, this);
    }
    callback(bucket, req);
  }
};

RiakClient.prototype._buildPath = function(method, bucket, key) {
  var path = this.baseUrl + bucket;
  if (key !== null && key !== undefined) {
    path = path + '/' + key;
  }
  else {
    if (method === 'GET') {
      path = path + '?keys=false';
    }
  }
  console.log('path: ' + path);
  return path;
};

RiakClient.prototype._newAjaxRequest = function(method, bucket, key) {
  var req = null;
  try {
    req = new XMLHttpRequest();
  }
  catch (trymsxml) {
    try {
      req = new ActiveXObject("Msxml2.XMLHTTP");
    }
    catch (trymicrosoft) {
      try {
	req = new ActiveXObject("Microsoft.XMLHTTP");
      }
      catch (failed) {
	throw('Error creating XMHTTPRequest');
      }
    }
  }
  req.open(method, this._buildPath(method, bucket, key), true);
  req.setRequestHeader('X-Riak-ClientId', this.clientId);
  return req;
};

/** End RiakClient internal Functions **/