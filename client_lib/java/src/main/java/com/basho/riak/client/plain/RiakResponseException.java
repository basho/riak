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
package com.basho.riak.client.plain;

import java.io.InputStream;
import java.util.Map;

import org.apache.commons.httpclient.HttpMethod;

import com.basho.riak.client.response.HttpResponse;
import com.basho.riak.client.response.RiakResponseRuntimeException;

/**
 * A checked decorator for {@link RiakResponseRuntimeException}
 */
public class RiakResponseException extends Exception implements HttpResponse {

    private static final long serialVersionUID = 5932513075276473483L;
    private RiakResponseRuntimeException impl;

    public RiakResponseException(RiakResponseRuntimeException e) {
        super(e.getMessage(), e.getCause());
        impl = e;
    }

    public String getBody() {
        return impl.getBody();
    }

    public InputStream getStream() {
        return impl.getStream();
    }

    public boolean isStreamed() {
        return impl.isStreamed();
    }
    
    public String getBucket() {
        return impl.getBucket();
    }

    public Map<String, String> getHttpHeaders() {
        return impl.getHttpHeaders();
    }

    public HttpMethod getHttpMethod() {
        return impl.getHttpMethod();
    }

    public String getKey() {
        return impl.getKey();
    }

    public int getStatusCode() {
        return impl.getStatusCode();
    }

    public boolean isError() {
        return impl.isError();
    }

    public boolean isSuccess() {
        return impl.isSuccess();
    }

    public void close() {
        impl.close();
    }
}
