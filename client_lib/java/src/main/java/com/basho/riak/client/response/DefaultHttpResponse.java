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
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.httpclient.HttpMethod;

import com.basho.riak.client.util.Constants;

/**
 * Simple implementation of HttpResponse interface. Simply stores and returns
 * the various fields.
 */
public class DefaultHttpResponse implements HttpResponse {

    private String bucket;
    private String key;
    private int status = -1;
    private Map<String, String> headers = null;
    private String body = null;
    private InputStream stream = null;
    private HttpMethod httpMethod = null;

    public DefaultHttpResponse(String bucket, String key, int status, Map<String, String> headers, String body,
            InputStream stream, HttpMethod httpMethod) {
        if (headers == null) {
            headers = new HashMap<String, String>();
        }

        this.bucket = bucket;
        this.key = key;
        this.status = status;
        this.headers = headers;
        this.body = body;
        this.stream = stream;
        this.httpMethod = httpMethod;
    }

    public String getBucket() {
        return bucket;
    }

    public String getKey() {
        return key;
    }

    public int getStatusCode() {
        return status;
    }

    public Map<String, String> getHttpHeaders() {
        return headers;
    }

    public String getBody() {
        return body;
    }

    public InputStream getStream() {
        return stream;
    }

    public boolean isStreamed() {
        return stream != null;
    }

    public HttpMethod getHttpMethod() {
        return httpMethod;
    }

    public boolean isSuccess() {
        String method = null;
        if (httpMethod != null) {
            method = httpMethod.getName();
        }

        return (status >= 200 && status < 300) ||
               ((status == 300 || status == 304) && Constants.HTTP_HEAD_METHOD.equals(method)) ||
               ((status == 300 || status == 304) && Constants.HTTP_GET_METHOD.equals(method)) ||
               ((status == 404) && Constants.HTTP_DELETE_METHOD.equals(method));
    }

    public boolean isError() {
        String method = null;
        if (httpMethod != null) {
            method = httpMethod.getName();
        }

        return (status < 100 || status >= 400) && !((status == 404) && Constants.HTTP_DELETE_METHOD.equals(method));
    }

    public void close() {
        if (httpMethod != null) {
            httpMethod.releaseConnection();
        }
    }
}
