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
 * HTTP response information resulting from some HTTP operation
 */
public interface HttpResponse {

    /**
     * The target object's bucket
     */
    public String getBucket();

    /**
     * The target object's key or null if bucket is target
     */
    public String getKey();

    /**
     * Resulting status code from the HTTP request.
     */
    public int getStatusCode();

    /**
     * The HTTP response headers.
     */
    public Map<String, String> getHttpHeaders();

    /**
     * The HTTP response body or null if isStreamed()
     */
    public String getBody();

    /**
     * The HTTP response body as an input stream if isStreamed(); null otherwise
     */
    public InputStream getStream();

    /**
     * Whether the response body is available as an input stream
     */
    public boolean isStreamed();

    /**
     * The actual {@link HttpMethod} used to make the HTTP request. Most of the
     * data here can be retrieved more simply using methods in this class. Also,
     * note that the connection will already be closed, so calling
     * getHttpMethod().getResponseBodyAsStream() will return null.
     */
    public HttpMethod getHttpMethod();

    /**
     * Whether the HTTP response is considered a success. Generally this
     * translates to a 2xx for any request, a 304 for GET and HEAD requests, or
     * 404 for DELETE requests.
     */
    public boolean isSuccess();

    /**
     * Whether the HTTP request returned a 4xx or 5xx response
     */
    public boolean isError();

    /**
     * Releases the underlying the HTTP connection when the response is streamed
     */
    public void close();
}
