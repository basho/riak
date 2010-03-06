
/**
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
**/

/**
 * This is a Javascript client for the Riak REST API. It
 * has two dependencies:
 *   Douglas Crockford's JSON library: http://www.json.org/js.html
 *   jQuery: http://jquery.com/ (but only for Ajax requests)
 *
 */

/**
 * TODO: Handle sibling values
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
    },

    /**
     * Was request successful?
     * @param req- XMLHttpRequest object
     * @return true if status is 2xx, false otherwise
     */
    wasSuccessful: function(req) {
      return req.status > 199 && req.status < 300;
    },
    parseSiblings: function(vclock, text) {
      var c = text.split('\n');
      c = c.slice(1).slice(0, c.length - 2);
      var separator = c[0];
      c = c.slice(1);
      var siblings = [];
      var sibling = {};
      for (var i = 0; i < c.length; i++) {
        if (c[i] === '') {
          continue;
        }
	if (c[i].substr(0, separator.length) == separator) {
	  sibling.vclock = vclock;
	  siblings.push(sibling);
  	  sibling = {};
        }
	else {
	  if (c[i].indexOf(':') > -1) {
	    var bits = c[i].split(': ');
	    if (bits[0] === 'Content-Type') {
	      sibling.contentType = bits[1];
	    }
	    else if (bits[0] === 'Link') {
	      sibling.linkHeader = bits[1];
	    }
	  }
	  else {
	    sibling.body = c[i];
          }
        }
      }
      return siblings;
    }
  };
}();

/**
 * Builds a map/reduce chain and executes it
 * @param client RiakClient object
 * @param bucketName Riak bucket name
 * @param key Riak bucket key (optional)
 */
function RiakMapper(client, bucketName, key) {
  if (bucketName === undefined ||
      client === undefined) {
    throw('Cannot construct RiakMapper without bucketName and client');
  }
  this.client = client;
  this.bucket = bucketName;
  this.key = key;
  this.phases = [];
};

/**
 * Add a map phase to a map/reduce job
 * @param options - Hash describing the map phase
 * @return RiakMapper instance for method chaining fun
 */
RiakMapper.prototype.map = function(options) {
  this.phases.push(this._buildPhase({map: null}, options));
  return this;
};

/**
 * Add a map phase to a map/reduce job
 * @param options - Hash describing the reduce phase
 * @return RiakMapper instance for method chaining fun
 */
RiakMapper.prototype.reduce = function(options) {
  this.phases.push(this._buildPhase({reduce: null}, options));
  return this;
};

/**
 * Add a link phase to a map/reduce job
 * @param options - Hash describing the link phase
 */
RiakMapper.prototype.link = function(options) {
  this.phases.push(this._buildPhase({link: null}, options));
  return this;
};

/**
 * Runs a map/reduce job
 * @param timeout - Job timeout (in milliseconds). Defaults to 60000
 * @param callback - Function to call when op completes
 *
 * callback - function(success, request, results)
 * @param success - Boolean indicating success or failure
 * @param results - JSON decoded results or null
 * @param request - XMLHttpRequest object
 */
RiakMapper.prototype.run = function(timeout, callback) {
  if (timeout === undefined || timeout === null) {
    timeout = 60000;
  }
  else if (typeof timeout === 'function') {
    callback = timeout;
    timeout = 60000;
  }
  var mapper = this;
  var job = {'inputs': this._buildInputs(),
	     'query': this.phases,
	     'timeout': timeout};
  jQuery.ajax({url: this.client.mapredUrl,
	       type: 'POST',
	       data: JSON.stringify(job),
	       beforeSend: function(req) { req.setRequestHeader('X-Riak-ClientId', mapper.client.clientId); },
	       complete: function(req, StatusText) { if (callback !== undefined) {
						       if (RiakUtil.wasSuccessful(req)) {
							 callback(true, JSON.parse(req.responseText), req);
						       }
						       else {
							 try {
							   var error = {error: JSON.parse(req.responseText)};
							   callback(false, error, req);
							 }
							 catch (e) {
							   callback(false, null, req);
							 }
						       } } } });
};

/** Start RiakMapper internals **/
RiakMapper.prototype._buildPhase = function(starter, options) {
  if (typeof options.source === 'function') {
      options.source = options.source.toString();
  }
  if ((starter.map === null ||
       starter.reduce === null) && (options.language === null || options.language === undefined)) {
    options.language = 'javascript';
  }
  if (starter.map === null) {
    starter.map = options;
  }
  else if (starter.reduce === null){
    starter.reduce = options;
  }
  else {
    if (options.bucket === null || options.bucket === undefined) {
      options.bucket = this.bucketName;
    }
    starter.link = options;
  }
  return starter;
};

RiakMapper.prototype._buildInputs = function() {
  if (this.key !== null && this.key !== undefined) {
    return [[this.bucket, this.key]];
  }
  else {
    return this.bucket;
  }
}
/** End RiakMapper internals **/

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
  if (client === undefined) {
    throw("Cannot construct RiakObject without a client reference");
  }
  this.bucket = bucketName;
  this.key = key;
  this.client = client;
  if (contentType === undefined) {
    this.contentType = 'application/octet-stream';
  }
  else {
    this.contentType = contentType;
  }
  if (contentType === 'application/json') {
    if (body !== undefined) {
      this.body = JSON.parse(body);
    }
    else {
      this.body = '';
    }
  }
  else {
    if (body === undefined) {
      this.body = '';
    }
    else {
      this.body = body;
    }
  }
  this.vclock = vclock;
  this.links = [];
};

/**
 * 'Hydrates' a RiakObject from a HTTP request
 * @param bucket - Riak bucket name
 * @param key - Riak bucket key
 * @param client - Owning RiakClient
 * @param req - XMLHttpRequest
 */
RiakObject.fromRequest = function(bucket, key, client, req) {
  var contentType = req.getResponseHeader('Content-Type');
  var vclock = req.getResponseHeader('X-Riak-Vclock');
  var linkHeader = req.getResponseHeader('Link');
  var body = req.responseText;
  var retval = new RiakObject(bucket, key, client, body, contentType, vclock);
  retval.setLinks(linkHeader);
  return retval;
};

RiakObject.fromMultipart = function(bucket, key, client, multipartChunk) {
  var retval = new RiakObject(bucket, key, client, multipartChunk.body, multipartChunk.contentType, multipartChunk.vclock);
  retval.setLinks(multipartChunk.linkHeader);
  return retval;
}

/**
 * Begins building a map/reduce job which will
 * use the current object as input
 * @param options - Hash description the map phase
 */
RiakObject.prototype.map = function(options) {
  var mapper = new RiakMapper(this.client, this.bucket, this.key);
  return mapper.map(options);
};

/**
 * Begins building a map/reduce job which will
 * use the current object as input
 * @param options - Hash description the reduce phase
 */
RiakObject.prototype.reduce = function(options) {
  var mapper = new RiakMapper(this.client, this.bucket, this.key);
  return mapper.reduce(options);
};

/**
 * Begins building a map/reduce job which will
 * use the current object as input
 * @param options - Hash description the link phase
 */
RiakObject.prototype.link = function(options) {
  var mapper = new RiakMapper(this.client, this.bucket, this.key);
  return mapper.link(options);
};

/**
 * Parses a raw link header and populates the links array
 * @param linkHeader - Raw link header string
 */
RiakObject.prototype.setLinks = function(linkHeader) {
  var parsedLinks = new Array();
  var links = linkHeader.split(",");
  for (var i = 0; i < links.length; i++) {
    var linkParts = links[i].split(';');
    var linkTag = RiakUtil.trim(linkParts[1]);
    var linkTo = RiakUtil.trim(linkParts[0].replace(/Link: ?/, ''));
    linkTo = linkTo.replace(/</, '').replace(/>/, '');
    linkTo = linkTo.replace(/\"/g, '');
    linkTag = linkTag.replace('riaktag=', '');
    linkTag = linkTag.replace(/\"/g, '');
    parsedLinks.push({tag: linkTag.toString(), target: linkTo.toString()});
  }
  this.links = parsedLinks;
};

/**
 * Retrieves the links collection
 * @return Array of link hashes (e.g. [{tag: 'userInfo', target: '/riak/users/bob'}])
 */
RiakObject.prototype.getLinks = function() {
  return this.links;
};

/**
 * Returns the links formatted for the Link header
 * @return - Link header string
 */
RiakObject.prototype.getLinkHeader = function() {
  if (this.links.length == 0) {
    return '';
  }
  var header = '';
  this.links.forEach(function(link) {
		       header = header + '<' + link.target + '>; ';
		       if (link.tag === 'rel=up') {
			 header = header + 'rel="up", ';
		       }
		       else {
			 header = header + 'riaktag=\"' + link.tag + '\", ';
		       } } );
  header = header.replace(/\"\"/g, '\"');
  return header.replace(/,\s$/, '');
};

/**
 * Adds a link to the object's link collection
 * @param link - Pointer to other object (e.g. /riak/foo/bar)
 * @param tag - Tag for the link (e.g. 'userInfo')
 * @param noDuplicates - Toggle duplicate checking on/off
 * @return true if added, false otherwise
 */
RiakObject.prototype.addLink = function(link, tag, noDuplicates) {
  if (link.indexOf('/') == -1) {
    throw('Invalid link: ' + link);
  }
  var retval = true;
  if (noDuplicates === false || noDuplicates === undefined) {
    this.links.push({tag: tag, target:link});
  }
  else {
    var foundDuplicate = false;
    for (var i = 0; i < this.links.length; i++) {
      foundDuplicate = this.links[i].tag === tag &&
	this.links[i].target === link;
      if (foundDuplicate) {
	retval = false;
	break;
      }
    }
    if (!foundDuplicate) {
      this.links.push({tag: tag, target: link});
    }
  }
  return retval;
};

/**
 * Removes a link from the links collection based on
 * link and tag
 * @param link - Pointer to other object
 * @param tag - Tag for the link
 * @return true if link removed, false if not
 */
RiakObject.prototype.removeLink = function(link, tag) {
  var retval = false;
  var newLinks = this.links.filter(function(l) { return l.link !== link || l.tag !== tag; });
  if (newLinkes.length != this.links.length) {
    retval = true;
    this.links = newLinks;
  }
  return retval;
};

/**
 * Resets the links collection to an empty array
 */
RiakObject.prototype.clearLinks = function() {
  this.links = [];
};


/**
 * Deletes an object from a Riak bucket
 * @param callback - Function to call when op complete
 *
 * callback - function(success, request)
 * @param success - Boolean flag indicating successful removal
 * @param request - XMLHTTPRequest object
 */
RiakObject.prototype.remove = function(callback) {
  var object = this;
  jQuery.ajax({url: this.client._buildPath('DELETE', this.bucket, this.key),
	       type: 'DELETE',
	       beforeSend: function(req) { req.setRequestHeader('X-Riak-ClientId', object.client.clientId);
					   req.setRequestHeader('Accept', 'multipart/mixed');
					   if (object.vclock !== undefined && object.vclock !== null) {
					     req.setRequestHeader('X-Riak-Vclock', object.vclock);
					   }
					 },
	       complete: function(req, statusText) { if (callback !== undefined) {
						      if (RiakUtil.wasSuccessful(req)) {
							callback(true, req);
						       }
						       else {
							 callback(false, req);
						       }
						     } }});
};

/**
 * Store the object in Riak
 * @param callback - Function to call when op completes
 *
 * callback - function(status, object, request)
 * @param status - 'status' of the result: 'ok', 'failed', or 'siblings'
 * @param object - If status is 'ok', object is an updated RiakObject instance
 *                 If status is 'siblings', object is an array of RiakObject instances
 *                 which the client can pick from to resolve the conflict
 *                 If status is 'failed', object is null
 *                 NOTE: Use the updated version to prevent siblings & vector clock explosion
 * @param request - XMLHttpRequest object
 */
RiakObject.prototype.store = function(callback) {
  if (this.contentType === null) {
    throw('RiakObject missing contentType');
  }
  var object = this;
  var objectData = null;
  if (this.contentType === 'application/json') {
    if (this.body !== undefined && this.body !== null) {
      objectData = JSON.stringify(this.body);
    }
    else {
      objectData = this.body;
    }
  }
  else {
    objectData = this.body;
  }
  jQuery.ajax({url: this.client._buildPath('PUT', this.bucket, this.key),
	  type: 'PUT',
	  data: objectData,
	  contentType: this.contentType,
	  dataType: 'text',
	  beforeSend: function(req) { req.setRequestHeader('X-Riak-ClientId', object.client.clientId);
				      req.setRequestHeader('Accept', 'multipart/mixed');
				      if (object.vclock !== undefined && object.vclock !== null) {
					req.setRequestHeader('X-Riak-Vclock', object.vclock);
				      }
				      var linkHeader = object.getLinkHeader();
				      if (linkHeader !== '') {
					req.setRequestHeader('Link', linkHeader);
				      }
				    },
	  complete: function(req, statusText) { object._store(req, callback); } });
};

/** Start RiakObject Internals **/
RiakObject.prototype._store = function(req, callback) {
  if (req.readyState != 4) {
    return;
  }
  if (callback !== undefined && callback !== null) {
    if (req.status == 200 || req.status == 204) {
      callback('ok', RiakObject.fromRequest(this.bucket, this.key, this.client, req), req);
    }
    /* Uh-oh, we've got siblings! */
    else if (req.status == 300) {
      var siblingData = RiakUtil.parseSiblings(req.getResponseHeader('X-Riak-Vclock'),
					       req.responseText);
      var thisObject = this;
      var siblings = siblingData.map(function(sd) { return RiakObject.fromMultipart(thisObject.bucket, thisObject.key, thisObject.client, sd); });
      callback('siblings', siblings, req);
    }
    else {
      callback('failed', null, req);
    }
  }
};

/** End RiakObject Internals **/

/**
 * Models a Riak bucket
 * @param bucket - Riak bucket name
 * @param client - RiakClient reference
 */
function RiakBucket(bucket, client, props) {
  if (client === undefined) {
    throw("Cannot construct RiakBucket without client reference");
  }
  this.name = bucket;
  this.client = client;
  if (props == undefined) {
    this.props = {};
  }
  else {
    this.props = props.props;
  }
};

/**
 * "Hydrates" a RiakBucket from a HTTP request
 * @param bucketName - Riak bucket name (duh!)
 * @param client - RiakClient object
 * @param req - Active XMLHttpRequest object
 * @return populated RiakBucket instance
 */
RiakBucket.fromRequest = function(bucketName, client, req) {
  var props = JSON.parse(req.responseText);
  return new RiakBucket(bucketName, client, props);
};

/**
 * Begins building a map/reduce job which will
 * use the entire bucket contents as input
 * @param options - Hash description the map phase
 * @return RiakMapper object
 */
RiakBucket.prototype.map = function(options) {
  var mapper = new RiakMapper(this.client, this.name);
  return mapper.map(options);
};

/**
 * Begins building a map/reduce job which will
 * use the entire bucket contents as input
 * @param options - Hash description the reduce phase
 * @return RiakMapper object
 */
RiakBucket.prototype.reduce = function(options) {
  var mapper = new RiakMapper(this.client, this.name);
  return mapper.reduce(options);
};

/**
 * Begins building a map/reduce job which will
 * use the entire bucket contents as input
 * @param options - Hash description the link phase
 * @return RiakMapper object
 */
RiakBucket.prototype.link = function(options) {
  var mapper = new RiakMapper(this.client, this.name);
  options.bucket = this.name;
  return mapper.link(options);
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
 * Stores bucket
 * @param callback - Function to call when op has completed
 *
 * callback - function(bucket, request)
 * @param bucket - Updated bucket or null if store failed
 * @param request - XMLHTTPRequest object
 */
RiakBucket.prototype.store = function(callback) {
  var bucket = this;
  var currentProps = {};
  currentProps.props = this.props;
  jQuery.ajax({url: this.client._buildPath('PUT', this.name),
	  type: 'PUT',
	  data: JSON.stringify(currentProps),
	  contentType: 'application/json',
	  dataType: 'text',
	  beforeSend: function(req) { req.setRequestHeader('X-Riak-ClientId', bucket.client.clientId); },
	  complete: function(req, statusText) { bucket._store(req, callback); } });
};

/**
 * Fetch an entry from the bucket
 * @param key - Riak bucket key
 * @param callback - Function to call when op has completed
 *
 * callback - function(object, request)
 * @param object - RiakObject if found, otherwise null
 * @param request - XMLHTTPRequest object
 */
RiakBucket.prototype.get = function(key, callback) {
  var bucket = this;
  jQuery.ajax({url: this.client._buildPath('GET', this.name, key),
	  type: 'GET',
	  dataType: 'text',
	  beforeSend: function(req) { req.setRequestHeader('X-Riak-ClientId', bucket.client.clientId); },
	  complete: function(req, statusText) { bucket._handleGetObject(key, req, callback, false); } });
};

/**
 * Fetch an entry from the bucket or create a new one
 * if not found
 * @param key - Riak bucket key
 * @param callback - Function to call when op has completed
 *
 * callback - function(object, request)
 * @param object - RiakObject instance
 * @param request - XMLHTTPRequest object
 */
RiakBucket.prototype.get_or_new = function(key, callback) {
  var bucket = this;
  jQuery.ajax({url: this.client._buildPath('GET', this.name, key),
	  type: 'GET',
	  dataType: 'text',
	  beforeSend: function(req) { req.setRequestHeader('X-Riak-ClientId', bucket.client.clientId); },
	  complete: function(req, statusText) { bucket._handleGetObject(key, req, callback, true); } });
};


/** Start RiakBucket internals **/

RiakBucket.prototype._store = function(req, callback) {
  if (req.readyState != 4) {
    return;
  }
  if (callback !== undefined) {
    if (req.status == 204) {
      this.client.bucket(this.name, callback);
    }
    else {
      callback(null, req);
    }
  }
};

RiakBucket.prototype._handleGetObject = function(key, req, callback, createEmpty) {
  if (req.readyState != 4) {
    return;
  }
  var object = null;
  if (callback !== null) {
    if (req.status == 200) {
      object = RiakObject.fromRequest(this.name, key, this.client, req);
    }
    else if ((req.status == 0 || req.status == 404) && createEmpty === true) {
      object = new RiakObject(this.name, key, this.client);
    }
    callback('ok', object, req);
  }
};

/** End RiakBucket internals **/


/**
 * Entry point for interacting with Riak
 * @param baseUrl - URL for 'raw' interface (optional, default: '/riak')
 * @param mapredUrl - URL for map/reduce jobs (optional, default: '/mapred')
 */
function RiakClient(baseUrl, mapredUrl) {
  if (baseUrl === undefined) {
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
  if (mapredUrl !== undefined) {
    this.mapredUrl = mapredUrl;
  }
  else {
    this.mapredUrl = '/mapred';
  }
};

/**
 * Fetches a bucket from Riak
 * Buckets *always* exist so no need to handle
 * @param bucket Riak bucket name
 * @param callback Function to call when op completes
 *
 * callback - function(bucket, request)
 * @param bucket - RiakBucket instance
 * @param request - XMLHTTPRequest object
 */
RiakClient.prototype.bucket = function(bucket, callback) {
  var client = this;
  jQuery.ajax({url: this._buildPath('GET', bucket),
	  type: 'GET',
	  contentType: 'application/json',
	  dataType: 'text',
	  beforeSend: function(req) { req.setRequestHeader('X-Riak-ClientId', this.clientId); },
	  complete: function(req, statusText) { client._handleGetBucket(bucket, req, callback, false); } });
};

/** Begin RiakClient internal functions **/
RiakClient.prototype._handleGetBucket = function(bucketName, req, callback, createEmpty) {
  var bucket = null;
  if (req.readyState != 4) {
    return;
  }
  if (callback !== undefined) {
    if (req.status == 200) {
      bucket = RiakBucket.fromRequest(bucketName, this, req);
    }
    callback(bucket, req);
  }
};

RiakClient.prototype._buildPath = function(method, bucket, key) {
  var path = this.baseUrl + bucket;
  if (key !== undefined) {
    path = path + '/' + key;
    if (method === 'PUT') {
      path = path + '?returnbody=true';
    }
  }
  else {
    if (method === 'GET') {
      path = path + '?keys=false';
    }
  }
  return path;
};

/** End RiakClient internal Functions **/