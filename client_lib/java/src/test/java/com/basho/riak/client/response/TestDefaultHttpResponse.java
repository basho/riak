package com.basho.riak.client.response;

import static org.mockito.Mockito.*;
import static org.junit.Assert.*;

import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.methods.DeleteMethod;
import org.apache.commons.httpclient.methods.GetMethod;
import org.apache.commons.httpclient.methods.HeadMethod;
import org.junit.Test;

public class TestDefaultHttpResponse {

    DefaultHttpResponse impl;

    @Test public void status_2xx_300_and_304_success_for_head() {
        HeadMethod head = new HeadMethod();
        
        for (int i = 200; i < 300; i++) {
            impl = new DefaultHttpResponse(null, null, i, null, null, null, head);
            assertTrue(impl.isSuccess());
        }

        impl = new DefaultHttpResponse(null, null, 300, null, null, null, head);
        assertTrue(impl.isSuccess());

        impl = new DefaultHttpResponse(null, null, 304, null, null, null, head);
        assertTrue(impl.isSuccess());
    }
    @Test public void status_2xx_300_and_304_success_for_get() {
        GetMethod get = new GetMethod();

        for (int i = 200; i < 300; i++) {
            impl = new DefaultHttpResponse(null, null, i, null, null, null, get);
            assertTrue(impl.isSuccess());
        }

        impl = new DefaultHttpResponse(null, null, 300, null, null, null, get);
        assertTrue(impl.isSuccess());

        impl = new DefaultHttpResponse(null, null, 304, null, null, null, get);
        assertTrue(impl.isSuccess());
    }
    @Test public void status_2xx_and_404_success_for_delete() {
        DeleteMethod delete = new DeleteMethod();

        for (int i = 200; i < 300; i++) {
            impl = new DefaultHttpResponse(null, null, i, null, null, null, delete);
            assertTrue(impl.isSuccess());
        }

        impl = new DefaultHttpResponse(null, null, 404, null, null, null, delete);
        assertTrue(impl.isSuccess());
    }
    @Test public void status_4xx_is_error() {
        for (int i = 400; i < 500; i++) {
            impl = new DefaultHttpResponse(null, null, i, null, null, null, null);
            assertTrue(impl.isError());
        }
    }
    @Test public void status_5xx_is_error() {
        for (int i = 500; i < 600; i++) {
            impl = new DefaultHttpResponse(null, null, i, null, null, null, null);
            assertTrue(impl.isError());
        }
    }
    @Test public void status_less_than_100_is_error() {
        for (int i = -1; i < 100; i++) {
            impl = new DefaultHttpResponse(null, null, i, null, null, null, null);
            assertTrue(impl.isError());
        }
    }
    @Test public void null_http_method_with_200_is_still_success() {
        impl = new DefaultHttpResponse(null, null, 200, null, null, null, null);
        assertTrue(impl.isSuccess());
    }
    @Test public void headers_are_never_null() {
        impl = new DefaultHttpResponse(null, null, 200, null, null, null, null);
        assertNotNull(impl.getHttpHeaders());
    }
    
    @Test public void close_closes_underlying_http_connection() {
        HttpMethod mockHttpMethod = mock(HttpMethod.class);
        impl = new DefaultHttpResponse(null, null, 200, null, null, null, mockHttpMethod);
        impl.close();
        verify(mockHttpMethod).releaseConnection();
    }
}
