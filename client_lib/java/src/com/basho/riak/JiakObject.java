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

public class JiakObject {

	private String bucket;
	private String key;
	private JSONArray links;
	private JSONObject object;
	private String vclock;
	private String lastmod;
	private String vtag;

	public JiakObject(String bucket, String key) {
		this(bucket, key, new JSONArray());
	}
	
	public JiakObject(String bucket, String key, JSONArray links) {
		this(bucket, key, links, new JSONObject());
	}
	
	public JiakObject(String bucket, String key, JSONArray links, JSONObject object) {
		this.bucket = bucket;
		this.key = key;
		this.links = links;
		this.object = object;
		this.vclock = null;
		this.lastmod = null;
		this.vtag = null;
	}

	public String getBucket() {
		return bucket;
	}

	public void setBucket(String bucket) {
		this.bucket = bucket;
	}

	public String getKey() {
		return key;
	}

	public void setKey(String key) {
		this.key = key;
	}

	public JSONArray getLinks() {
		return links;
	}

	public void setLinks(JSONArray links) {
		this.links = links;
	}

	public JSONObject getObject() {
		return object;
	}

	public void setObject(JSONObject object) {
		this.object = object;
	}

	public String getVclock() {
		return vclock;
	}

	public void setVclock(String vclock) {
		this.vclock = vclock;
	}

	public String getLastmod() {
		return lastmod;
	}

	public void setLastmod(String lastmod) {
		this.lastmod = lastmod;
	}

	public String getVtag() {
		return vtag;
	}

	public void setVtag(String vtag) {
		this.vtag = vtag;
	}

	public void update(JSONObject data) throws JSONException {
		this.vclock = data.getString("vclock");
		this.lastmod = data.getString("lastmod");
		this.vtag = data.getString("vtag");
		this.object = data.getJSONObject("object");
		this.links = data.getJSONArray("links");
	}

	public Object get(String key) throws JSONException {
		return this.object.get(key);
	}
	
	public void set(String key, Object value) throws JSONException {
		this.object.put(key, value);
	}
	
	public JSONObject toJSONObject() throws JSONException { 
		JSONObject o = new JSONObject();
		o.put("vclock", vclock);
		o.put("lastmod", lastmod);
		o.put("vtag", vtag);
		o.put("bucket", bucket);
		o.put("key", key);
		o.put("links", links);
		o.put("object", object);
		return o;
	}
	
	public String toJSONString() throws JSONException { 
		return toJSONObject().toString();
	}
}