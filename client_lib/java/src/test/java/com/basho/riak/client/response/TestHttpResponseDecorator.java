package com.basho.riak.client.response;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.httpclient.HttpMethod;
import org.json.JSONException;
import org.junit.Test;

public class TestHttpResponseDecorator {

    @Test public void delegates_http_response_methods_to_impl() throws JSONException {
        
        final String BUCKET = "bucket";
        final String KEY = "key";
        final String BODY = "body";
        final int STATUS_CODE = 1;
        final Map<String, String> HTTP_HEADERS = new HashMap<String, String>();
        final HttpMethod HTTP_METHOD = mock(HttpMethod.class);
        final boolean IS_SUCCESS = true;
        final boolean IS_ERROR = true;
        
        HttpResponse mockHttpResponse = mock(HttpResponse.class);
        when(mockHttpResponse.getBucket()).thenReturn(BUCKET);
        when(mockHttpResponse.getKey()).thenReturn(KEY);
        when(mockHttpResponse.getBody()).thenReturn(BODY);
        when(mockHttpResponse.getStatusCode()).thenReturn(STATUS_CODE);
        when(mockHttpResponse.getHttpHeaders()).thenReturn(HTTP_HEADERS);
        when(mockHttpResponse.getHttpMethod()).thenReturn(HTTP_METHOD);
        when(mockHttpResponse.isSuccess()).thenReturn(IS_SUCCESS);
        when(mockHttpResponse.isError()).thenReturn(IS_ERROR);
        
        HttpResponseDecorator impl = new HttpResponseDecorator(mockHttpResponse);
        
        assertEquals(BUCKET, impl.getBucket());
        assertEquals(KEY, impl.getKey());
        assertEquals(BODY, impl.getBody());
        assertEquals(STATUS_CODE, impl.getStatusCode());
        assertSame(HTTP_HEADERS, impl.getHttpHeaders());
        assertSame(HTTP_METHOD, impl.getHttpMethod());
        assertEquals(IS_SUCCESS, impl.isSuccess());
        assertEquals(IS_ERROR, impl.isError());
        
        verify(mockHttpResponse).getBucket();
        verify(mockHttpResponse).getKey();
        verify(mockHttpResponse).getBody();
        verify(mockHttpResponse).getStatusCode();
        verify(mockHttpResponse).getHttpHeaders();
        verify(mockHttpResponse).getHttpMethod();
        verify(mockHttpResponse).isSuccess();
        verify(mockHttpResponse).isError();
    }
}
