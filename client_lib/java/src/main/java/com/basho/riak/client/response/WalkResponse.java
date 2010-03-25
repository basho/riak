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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.basho.riak.client.RiakClient;
import com.basho.riak.client.RiakObject;
import com.basho.riak.client.util.ClientUtils;
import com.basho.riak.client.util.Constants;
import com.basho.riak.client.util.Multipart;

/**
 * Response from a GET request for an object with link walking. Decorates an
 * HttpResponse to interpret walk responses from Riak which returns
 * multipart/mixed documents.
 */
public class WalkResponse extends HttpResponseDecorator implements HttpResponse {

    private List<? extends List<RiakObject>> steps = new ArrayList<List<RiakObject>>();

    /**
     * On a 2xx response, parses the HTTP body into a list of steps. Each step
     * contains a list of objects returned in that step. The HTTP body is a
     * multipart/mixed message with multipart/mixed subparts
     */
    public WalkResponse(HttpResponse r, RiakClient riak) throws RiakResponseRuntimeException {
        super(r);

        if (r != null && r.isSuccess()) {
            steps = parseSteps(r, riak);
        }
    }

    public WalkResponse(HttpResponse r) throws RiakResponseRuntimeException {
        this(r, null);
    }

    /** Whether objects were contained in the response */
    public boolean hasSteps() {
        return steps.size() > 0;
    }

    /**
     * Steps accumulated from link walking. See RiakClient.walk() for more
     * information.
     */
    public List<? extends List<RiakObject>> getSteps() {
        return steps;
    }

    /**
     * Parse a multipart/mixed message with multipart/mixed subparts into a list
     * of lists.
     * 
     * @param r
     *            HTTP response from Riak
     * @param riak
     *            {@link RiakClient} to associate this object with
     * @return A list of lists of {@link RiakObject}s represented by the
     *         response.
     * @throws RiakResponseRuntimeException
     *             If one of the parts of the body doesn't contain a proper
     *             multipart/mixed message
     */
    private static List<? extends List<RiakObject>> parseSteps(HttpResponse r, RiakClient riak)
            throws RiakResponseRuntimeException {
        String bucket = r.getBucket();
        String key = r.getKey();
        List<List<RiakObject>> parsedSteps = new ArrayList<List<RiakObject>>();
        List<Multipart.Part> parts = Multipart.parse(r.getHttpHeaders(), r.getBody());

        if (parts != null) {
            for (Multipart.Part part : parts) {
                Map<String, String> partHeaders = part.getHeaders();
                String contentType = partHeaders.get(Constants.HDR_CONTENT_TYPE);

                if (contentType == null ||
                    !(contentType.trim().toLowerCase().startsWith(Constants.CTYPE_MULTIPART_MIXED)))
                    throw new RiakResponseRuntimeException(r, "multipart/mixed subparts expected in link walk results");

                parsedSteps.add(ClientUtils.parseMultipart(riak, bucket, key, partHeaders, part.getBody()));
            }
        }

        return parsedSteps;
    }
}
