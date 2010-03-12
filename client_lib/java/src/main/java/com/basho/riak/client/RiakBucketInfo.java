/*
 * This file is provided to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package com.basho.riak.client;

import java.util.ArrayList;
import java.util.Collection;

import org.json.JSONException;
import org.json.JSONObject;

import com.basho.riak.client.util.Constants;

/**
 * Represents the metadata stored in a bucket including its schema and the list
 * of keys contained in the bucket.
 */
public class RiakBucketInfo {

    private JSONObject schema;
    private Collection<String> keys;

    /**
     * Returns the bucket's properties.
     */
    public JSONObject getSchema() {
        return schema;
    }

    /**
     * The object keys in this bucket.
     */
    public Collection<String> getKeys() {
        return keys;
    }

    /**
     * Construct a bucket info to populate for a writeSchema request.
     */
    public RiakBucketInfo() {
        this(null, null);
    }

    /**
     * Construct a bucket info using the JSON data from a listBucket() response.
     * 
     * @param schema
     *            The JSON object containing the bucket's schema
     * @param keys
     *            The keys in the bucket
     */
    public RiakBucketInfo(JSONObject schema, Collection<String> keys) {

        if (schema != null) {
            this.schema = schema;
        } else {
            this.schema = new JSONObject();
        }
        if (keys != null) {
            this.keys = keys;
        } else {
            this.keys = new ArrayList<String>();
        }
    }

    /**
     * Allow siblings to be returned for an object. If false, last write wins.
     */
    public void setAllowMult(boolean allowMult) {
        try {
            getSchema().put(Constants.FL_SCHEMA_ALLOW_MULT, allowMult);
        } catch (JSONException unreached) {
            throw new IllegalStateException("operation is valid json", unreached);
        }
    }

    public Boolean getAllowMult() {
        return getSchema().optBoolean(Constants.FL_SCHEMA_ALLOW_MULT);
    }

    /**
     * Number of replicas per object in this bucket.
     */
    public void setNVal(int n) {
        try {
            getSchema().put(Constants.FL_SCHEMA_NVAL, n);
        } catch (JSONException unreached) {
            throw new IllegalStateException("operation is valid json", unreached);
        }
    }

    public Integer getNVal() {
        return getSchema().optInt(Constants.FL_SCHEMA_NVAL);
    }

    /**
     * Erlang module and name of the function to use to hash object keys. See
     * Riak's documentation.
     */
    public void setCHashFun(String mod, String fun) {
        if (mod == null) {
            mod = "";
        }
        if (fun == null) {
            fun = "";
        }
        try {
            JSONObject chashfun = new JSONObject();
            chashfun.put(Constants.FL_SCHEMA_CHASHFUN_MOD, mod);
            chashfun.put(Constants.FL_SCHEMA_CHASHFUN_FUN, fun);
            getSchema().put(Constants.FL_SCHEMA_CHASHFUN, chashfun);
        } catch (JSONException unreached) {
            throw new IllegalStateException("operation is valid json", unreached);
        }
    }

    /**
     * The chash_keyfun property as {@literal <module>:<function>}
     */
    public String getCHashFun() {
        JSONObject chashfun = getSchema().optJSONObject(Constants.FL_SCHEMA_CHASHFUN);
        if (chashfun == null)
            return null;
        String mod = chashfun.optString(Constants.FL_SCHEMA_CHASHFUN_MOD, "");
        String fun = chashfun.optString(Constants.FL_SCHEMA_CHASHFUN_FUN, "");
        return mod + ":" + fun;
    }

    /**
     * Erlang module and name of the function to use to walk object links. See
     * Riak's documentation.
     */
    public void setLinkFun(String mod, String fun) {
        if (mod == null) {
            mod = "";
        }
        if (fun == null) {
            fun = "";
        }
        try {
            JSONObject linkfun = new JSONObject();
            linkfun.put(Constants.FL_SCHEMA_LINKFUN_MOD, mod);
            linkfun.put(Constants.FL_SCHEMA_LINKFUN_FUN, fun);
            getSchema().put(Constants.FL_SCHEMA_LINKFUN, linkfun);
        } catch (JSONException unreached) {
            throw new IllegalStateException("operation is valid json", unreached);
        }
    }

    /**
     * The linkfun property as {@literal <module>:<function>}
     */
    public String getLinkFun() {
        JSONObject linkfun = getSchema().optJSONObject(Constants.FL_SCHEMA_LINKFUN);
        if (linkfun == null)
            return null;
        String mod = linkfun.optString(Constants.FL_SCHEMA_LINKFUN_MOD, "");
        String fun = linkfun.optString(Constants.FL_SCHEMA_LINKFUN_FUN, "");
        return mod + ":" + fun;
    }
}
