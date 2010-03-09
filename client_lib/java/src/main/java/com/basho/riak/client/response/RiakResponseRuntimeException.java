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
 * Thrown when the Riak server returns a malformed response. The HTTP response
 * is returned in the exception.
 */
public class RiakResponseRuntimeException extends RuntimeException implements HttpResponse {

    private static final long serialVersionUID = 2853253336513247178L;
    private HttpResponse response = null;

    public RiakResponseRuntimeException(HttpResponse response) {
        super();
        this.response = response;
    }

    public RiakResponseRuntimeException(HttpResponse response, String message, Throwable cause) {
        super(message, cause);
        this.response = response;
    }

    public RiakResponseRuntimeException(HttpResponse response, String message) {
        super(message);
        this.response = response;
    }

    public RiakResponseRuntimeException(HttpResponse response, Throwable cause) {
        super(cause);
        this.response = response;
    }

    public String getBody() {
        if (response == null)
            return null;
        return response.getBody();
    }

    public InputStream getStream() {
        if (response == null)
            return null;
        return response.getStream();
    }

    public boolean isStreamed() {
        if (response == null)
            return false;
        return response.isStreamed();
    }

    public String getBucket() {
        if (response == null)
            return null;
        return response.getBucket();
    }

    public Map<String, String> getHttpHeaders() {
        if (response == null)
            return null;
        return response.getHttpHeaders();
    }

    public HttpMethod getHttpMethod() {
        if (response == null)
            return null;
        return response.getHttpMethod();
    }

    public String getKey() {
        if (response == null)
            return null;
        return response.getKey();
    }

    public int getStatusCode() {
        if (response == null)
            return -1;
        return response.getStatusCode();
    }

    public boolean isError() {
        return true;
    }

    public boolean isSuccess() {
        return false;
    }

    public void close() {
        if (response != null)
            response.close();
    }
}
