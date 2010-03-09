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
package com.basho.riak.client.response;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Collection;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.json.JSONTokener;

import com.basho.riak.client.RiakBucketInfo;
import com.basho.riak.client.util.ClientUtils;
import com.basho.riak.client.util.Constants;

/**
 * Response from a GET request at a bucket's URL. Decorates an HttpResponse to
 * interpret listBucket response from Riak, which is a JSON object with the keys
 * "props" and "keys".
 */
public class BucketResponse extends HttpResponseDecorator implements HttpResponse {

    private RiakBucketInfo bucketInfo = null;

    /**
     * On a 2xx response, parses the JSON response into a {@link RiakBucketInfo}
     * 
     * @param r
     *            The HTTP response from a GET at a bucket
     * @throws JSONException
     *             If the response is a 2xx but contains invalid JSON
     * @throws IOException
     *             If a communication error with the Riak server while trying to
     *             read the streamed response
     */
    public BucketResponse(HttpResponse r) throws JSONException, IOException {
        super(r);

        if (r != null && r.isSuccess()) {
            JSONObject props;
            Collection<String> keys;
            if (!r.isStreamed()) {
                JSONObject json = new JSONObject(r.getBody());
                JSONArray jsonKeys = json.optJSONArray(Constants.FL_KEYS);
                props = json.optJSONObject(Constants.FL_SCHEMA);
                keys = ClientUtils.jsonArrayAsList(jsonKeys);
            } else {
                InputStream stream = r.getStream();
                JSONTokener tokens = new JSONTokener(new InputStreamReader(stream));

                // suck in the first object from the stream, which is the schema
                // and give the rest to the streamed keys collection
                props = new JSONObject(tokens).optJSONObject(Constants.FL_SCHEMA);
                keys = new StreamedKeysCollection(tokens);
            }
            bucketInfo = new RiakBucketInfo(props, keys);
        }
    }

    /**
     * Whether the bucket's schema and keys were returned in the response from
     * Riak
     */
    public boolean hasBucketInfo() {
        return bucketInfo != null;
    }

    /**
     * The bucket's schema and keys
     */
    public RiakBucketInfo getBucketInfo() {
        return bucketInfo;
    }
}