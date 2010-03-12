package com.basho.riak.client.request;

import static org.junit.Assert.*;

import org.junit.Test;

public class TestRequestMeta {

    @Test public void readParams_sets_r_query_parameter() {
        final int R = 10;
        RequestMeta impl = RequestMeta.readParams(R);
        assertEquals(Integer.toString(R), impl.getQueryParam("r"));
    }

    @Test public void writeParams_sets_non_null_w_and_dw_query_parameter() {
        final int W = 10;
        final int DW = 11;
        RequestMeta impl = RequestMeta.writeParams(W, DW);
        assertEquals(Integer.toString(W), impl.getQueryParam("w"));
        assertEquals(Integer.toString(DW), impl.getQueryParam("dw"));

        impl = RequestMeta.writeParams(W, null);
        assertEquals(Integer.toString(W), impl.getQueryParam("w"));
        assertEquals(null, impl.getQueryParam("dw"));

        impl = RequestMeta.writeParams(W, DW);
        assertEquals(Integer.toString(W), impl.getQueryParam("w"));
        assertEquals(Integer.toString(DW), impl.getQueryParam("dw"));
    }

    @Test public void headers_are_returned_by_get_headers() {
        final String HEADER_KEY = "key";
        final String HEADER_VALUE = "value";
        
        RequestMeta impl = new RequestMeta();
        impl.setHeader(HEADER_KEY, HEADER_VALUE);
        assertEquals(HEADER_VALUE, impl.getHeader(HEADER_KEY));
        assertEquals(HEADER_VALUE, impl.getHeaders().get(HEADER_KEY));
    }

    @Test public void query_params_are_returned_by_get_query_params() {
        final String QP_KEY = "key";
        final String QP_VALUE = "value";
        
        RequestMeta impl = new RequestMeta();
        impl.setQueryParam(QP_KEY, QP_VALUE);
        assertEquals(QP_VALUE, impl.getQueryParam(QP_KEY));
        assertTrue(impl.getQueryParams().contains(QP_KEY + "=" + QP_VALUE));
    }

    @Test public void query_parameters_string_contains_formatted_parameters() {
        RequestMeta impl = new RequestMeta();
        impl.setQueryParam("p1", "v1");
        impl.setQueryParam("p2", "v2");
        impl.setQueryParam("p3", "v3");
        impl.getQueryParams().matches("(^p1=v1|&p1=v1).*"); // param at beginning of string or preceded by ampersand 
        impl.getQueryParams().matches("(^p2=v2|&p2=v2).*");
        impl.getQueryParams().matches("(^p3=v3|&p3=v3).*");
    }
    
    @Test public void no_leading_or_trailing_ampersands_for_query_parameters_string() {
        RequestMeta impl = new RequestMeta();
        impl.setQueryParam("p1", "v1");
        impl.setQueryParam("p2", "v2");
        impl.setQueryParam("p3", "v3");
        assertFalse(impl.getQueryParams().startsWith("&"));
        assertFalse(impl.getQueryParams().endsWith("&"));
    }
}
