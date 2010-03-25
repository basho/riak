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

import java.io.InputStream;
import java.util.Map;

import org.apache.commons.httpclient.HttpMethod;

/**
 * Used with RiakClient.stream() to process the HTTP responses for fetch
 * requests as a stream.
 */
public interface StreamHandler {

    /**
     * Process the HTTP response whose value is given as a stream.
     * 
     * @param bucket
     *            The object's bucket
     * @param key
     *            The object's key
     * @param status
     *            The HTTP status code returned for the request
     * @param headers
     *            The HTTP headers returned in the response
     * @param in
     *            InputStream of the object's value (body)
     * @param httpMethod
     *            The original {@link HttpMethod} used to make the request. Its
     *            connection is still open and will be closed by the caller on
     *            return.
     * @return true if the object was processed; false otherwise
     */
    public boolean process(String bucket, String key, int status, Map<String, String> headers, InputStream in,
                           HttpMethod httpMethod);
}
