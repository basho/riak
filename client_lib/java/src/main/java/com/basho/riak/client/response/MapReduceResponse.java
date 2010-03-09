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

import org.json.JSONArray;
import org.json.JSONException;

/**
 * Response from a map-reduce query (POST to /mapred). Decorates an HttpResponse
 * and parses returned JSON array returned from Riak.
 */
public class MapReduceResponse extends HttpResponseDecorator implements HttpResponse {

    JSONArray result = null;

    /**
     * On a 2xx response, parses the response into a {@link JSONArray}
     * 
     * @param r
     *            The HTTP response query POST'd to the map-reduce resource
     * @throws JSONException
     *             Response is a 2xx but doesn't contain a valid JSON array
     */
    public MapReduceResponse(HttpResponse r) throws JSONException {
        super(r);

        if (r != null && r.isSuccess() && (r.getBody() != null)) {
            result = new JSONArray(r.getBody());
        }
    }

    /**
     * The result of the map-reduce query as a JSON array
     */
    public JSONArray getResults() {
        return result;
    }
}
