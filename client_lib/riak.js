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

/* TO DOs: Object metadata, Link walking, Map/Reduce */

/**
 * Utility class for holding functions which don't belong anywhere else
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
    }
  };
}();

/**
 * Models an entry in Riak
 * @param bucket - Riak bucket name
 * @param key - Riak key name
 * @param body - Data to store
 * @param contentType - Content type to assign (optional, default: application/octet-stream)
 * @param vclock - VClock to use (optional, default: null)
 */
function RiakObject(bucket, key, body, contentType, vclock) {
  this.bucket = bucket;
  this.key = key;
  this.body = body;
  this.contentType = contentType;
  this.vclock = vclock;
};

/**
 * "Preps" the object for storage by RiakClient
 * @param baseUrl - Base URL pointing to the Riak 'raw' interface
 * @return Associative map of prepped data
 */
RiakObject.prototype.prep = function(baseUrl) {
  var headers = [];
  if (this.contentType !== null && this.contentType !== undefined) {
    headers['Content-Type'] = this.contentType;
  }
  if (this.vclock !== null && this.vclock !== undefined) {
    headers['X-Riak-Vclock'] = this.vclock;
  }
  var url = baseUrl + this.bucket + '/' + this.key;
  return {'headers': headers, 'url': url, 'body': this.body};
};

/**
 * Entry point for interacting with Riak
 * @param baseUrl - URL for 'raw' interface (optional, default: '/raw/')
 */
function RiakClient(baseUrl) {
  if (baseUrl == null) {
    baseUrl = '/raw/';
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
 * Submits a PUT request to Riak
 * @param obj - Instance of RiakObject to store
 * @param callback - Function to callback when store is complete
 * @param w - W-value, wait for W partitions to ack operation (optional, default: 2)
 * @param dw - DW-value, wait for DW partitions to complete operation (optional, default: 2)
 */
RiakClient.prototype.store = function(obj, callback, w, dw) {
  var preppedData = obj.prep(this.baseUrl);
  var req = this._newAjaxRequest();
  req.open('PUT', preppedData.url, true);
  this._setHeaders(req, preppedData.headers);
  req.onreadystatechange = this._newResponseHandler(req, callback);
  req.send(preppedData.body);
};

/**
 * Submits a DELETE request to Riak
 * @param bucket - Riak bucket name
 * @param key - Riak key name
 * @param callback - Fnction to callback when delete is complete
 * @param dw - DW-value, wait for DW parititons to complte operation (optional, default: 2)
 */
RiakClient.prototype.remove = function(bucket, key, callback, dw) {
  var path = this._buildPath(bucket, key);
  var req = this._newAjaxRequest();
  req.open('DELETE', path, true);
  req.onreadystatechange = this._newReadyHandler(req, callback);
  req.send(null);
};

/**
 * Submits a GET request to Riak
 * @param bucket - Riak bucket name
 * @param key - Riak key name
 * @param callback - Callback funcion when fetch is complete
 * @param r - R-value, wait for R paritions to respond (optional, default: 2)
 */
RiakClient.prototype.fetch = function(bucket, key, callback, r) {
  var path = this._buildPath(bucket, key);
  var req = this._newAjaxRequest();
  req.open('GET', path, true);
  req.onreadystatechange = this._newBuilderResponseHandler(bucket, key, req, callback);
  req.send(null);
};

/**
 * Submits a POST request to Riak
 * @param obj - Instance of RiakObject to update
 * @param callback - Function to call when the update is complete
 * @param w - W-value, wait for W partitions to ack operaiton (optiona, default: 2)
 * @param dw - DW-value, wait for DW partitions to complete operation (optional, default: 2)
 */
RiakClient.prototype.update = function(obj, callback, w, dw) {
  var preppedData = obj.prep(this.baseUrl);
  var req = this._newAjaxRequest();
  req.open('POST', preppedData.url, true);
  this._setHeaders(req, preppedData.headers);
  req.onreadystatechange = this._newBuilderResponseHandler(obj.bucket, obj.key, req, callback);
  req.send(preppedData.body);
}

/** Internal functions **/
/** DO NOT USE **/
RiakClient.prototype._setHeaders = function(req, headers) {
  for (var headerName in headers) {
    var headerValue = headers[headerName];
    req.setRequestHeader(headerName, headerValue);
  }
  req.setRequestHeader('X-Riak-ClientId', this.clientId);
};

RiakClient.prototype._newResponseHandler = function(req, callback) {
  return function() {
    if (req.readyState != 4) {
      return;
    }
    else {
      if (callback !== null && callback !== undefined) {
	callback(req);
      }
    }
  };
};

RiakClient.prototype._newBuilderResponseHandler = function(bucket, key, req, callback) {
  return function() {
    if (req.readyState != 4) {
      return;
    }
    else {
      if (callback !== null && callback !== undefined) {
	var obj = null;
	if (req.status == 200) {
	  var vclock = req.getResponseHeader('X-Riak-Vclock');
	  var contentType = req.getResponseHeader('Content-Type');
	  var body = req.responseText;
	  obj = new RiakObject(bucket, key, body, contentType, vclock);
	}
	callback(obj, req);
      }
    }
  };
};

RiakClient.prototype._buildPath = function(bucket, key) {
  return this.baseUrl + '/' + bucket + '/' + key;
};

RiakClient.prototype._newAjaxRequest = function() {
  try {
    return new XMLHttpRequest();
  }
  catch (trymsxml) {
    try {
      return new ActiveXObject("Msxml2.XMLHTTP");
    }
    catch (trymicrosoft) {
      try {
	return new ActiveXObject("Microsoft.XMLHTTP");
      }
      catch (failed) {
	throw('Error creating XMHTTPRequest');
      }
    }
  }
};