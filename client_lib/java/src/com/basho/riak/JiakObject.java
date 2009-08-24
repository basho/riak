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

package com.basho.riak;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Container class for Riak/Jiak data. The actual user data in a JiakObject is
 * contained in the <code>.object</code> field of a JiakObject. The other
 * top-level fields are metadata that must be carried along with the object in
 * order to properly modify the object in subsequent requests.
 * 
 * @author Andy Gross <andy@basho.com>
 * @version 0.1
 */

public class JiakObject {

	private final String bucket;
	private final String key;
	private JSONArray links;
	private JSONObject object;
	private String vclock;
	private String lastmod;
	private String vtag;

	/**
	 * Construct an empty Jiak object.
	 * 
	 * @param bucket
	 *            The containing bucket for this JiakObject.
	 * @param key
	 *            The key of this JiakObject.
	 */
	public JiakObject(final String bucket, final String key) {
		this(bucket, key, new JSONArray());
	}

	/**
	 * Construct a Jiak object and define a starting set of links.
	 * 
	 * @param bucket
	 *            The containing bucket for this JiakObject.
	 * @param key
	 *            The key of this JiakObject.
	 * @param links
	 *            The links for this JiakObject. The links field is a JSONArray
	 *            of JSONArrays of the form:
	 *            <code>["bucket", "key", "tag"]</code>.
	 */
	public JiakObject(final String bucket, final String key,
			final JSONArray links) {
		this(bucket, key, links, new JSONObject());
	}

	/**
	 * Construct a Jiak object and define a starting set of links and object
	 * values.
	 * 
	 * @param bucket
	 *            The containing bucket for this JiakObject.
	 * @param key
	 *            The key of this JiakObject.
	 * @param links
	 *            The links for this JiakObject. The links field is a JSONArray
	 *            of JSONArrays of the form:
	 *            <code>["bucket", "key", "tag"]</code>.
	 * @param object
	 *            A JSONObject representing the initial value of this object.
	 */
	public JiakObject(final String bucket, final String key,
			final JSONArray links, final JSONObject object) {
		this.bucket = bucket;
		this.key = key;
		this.links = links;
		this.object = object;
		vclock = null;
		lastmod = null;
		vtag = null;
	}

	/**
	 * Get the bucket for this JiakObject.
	 * 
	 * @return The bucket for this object.
	 */
	public String getBucket() {
		return bucket;
	}

	/**
	 * Get the key for this JiakObject.
	 * 
	 * @return The key for this object.
	 */
	public String getKey() {
		return key;
	}

	/**
	 * Get the links for this JiakObject.
	 * 
	 * @return A JSONArray of links for this object.
	 */
	public JSONArray getLinks() {
		return links;
	}

	/**
	 * Set the links for this JiakObject.
	 * 
	 * @param links
	 *            The links to set for this object.
	 */
	public void setLinks(final JSONArray links) {
		this.links = links;
	}

	/**
	 * Get the object data for this JiakObject.
	 * 
	 * @return The actual object data for this JiakObject.
	 */
	public JSONObject getObject() {
		return object;
	}

	/**
	 * Set the object data for this JiakObject.
	 * 
	 * @param object
	 *            A JSONObject representing the actual object data for this
	 *            JiakObject.
	 */
	public void setObject(final JSONObject object) {
		this.object = object;
	}

	/**
	 * Get a string representation of the vector clock for this object.
	 * 
	 * @return The Vector Clock for this object.
	 */
	public String getVclock() {
		return vclock;
	}

	/**
	 * Get the last-modified timestamp for this object.
	 * 
	 * @return The last modified timestamp of this object.
	 */
	public String getLastmod() {
		return lastmod;
	}

	/**
	 * Return the "VTag" for this JiakObject. A VTag is a hashed representation
	 * of the vector clock of an object, suitable for quick comparison of two
	 * object values, or for use as an HTTP Entity Tag.
	 * 
	 * @return The VTag for this JiakObject
	 */
	public String getVtag() {
		return vtag;
	}

	/**
	 * Update this object with new data from a Riak server. This method is
	 * mainly intended for internal use an should be used with care.
	 * 
	 * @param data
	 *            A JSONObject returned from calling an HTTP method on a Riak
	 *            server.
	 * @throws JSONException
	 *             If an error occurs unparsing the JSONObject.
	 */
	public void update(final JSONObject data) throws JSONException {
		vclock = data.getString("vclock");
		lastmod = data.getString("lastmod");
		vtag = data.getString("vtag");
		object = data.getJSONObject("object");
		links = data.getJSONArray("links");
	}

	/**
	 * Accessor for values in the underlying object data.
	 * 
	 * @param key
	 *            The field name in the object to return.
	 * @return The value associated with the key in the object data.
	 * @throws JSONException
	 */
	public Object get(final String key) throws JSONException {
		return object.get(key);
	}

	/**
	 * Setter for values in the underlying object data.
	 * 
	 * @param key
	 *            The field name to set.
	 * @param value
	 *            The value to set.
	 * @throws JSONException
	 *             If the key-value are invalid JSON.
	 */
	public void set(final String key, final Object value) throws JSONException {
		object.put(key, value);
	}

	/**
	 * Return a JSONObject representing the JiakObject (metadata and object
	 * state).
	 * 
	 * @return A JSONObject.
	 * @throws JSONException
	 *             If a failure occurs converting the object to JSON.
	 */
	public JSONObject toJSONObject() throws JSONException {
		final JSONObject o = new JSONObject();
		o.put("vclock", vclock);
		o.put("lastmod", lastmod);
		o.put("vtag", vtag);
		o.put("bucket", bucket);
		o.put("key", key);
		o.put("links", links);
		o.put("object", object);
		return o;
	}

	/**
	 * Return a JSON string representing this JiakObject. This is equivalent to
	 * calling <code>.toJSONObject.toString()</code>.
	 * 
	 * @return A JSON string.
	 * @throws JSONException
	 *             If a failure occurs converting the object to JSON.
	 */
	public String toJSONString() throws JSONException {
		return toJSONObject().toString();
	}
}