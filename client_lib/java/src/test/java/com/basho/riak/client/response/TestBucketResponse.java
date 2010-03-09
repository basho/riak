package com.basho.riak.client.response;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.Iterator;

import org.json.JSONException;
import org.junit.Test;

public class TestBucketResponse {

    final String BODY = 
        "{\"props\":" +
            "{\"name\":\"b\"," +
            "\"allow_mult\":false," +
            "\"big_vclock\":50," +
            "\"chash_keyfun\":" +
                "{\"mod\":\"riak_util\"," +
                "\"fun\":\"chash_std_keyfun\"}," +
            "\"linkfun\":" +
                "{\"mod\":\"jiak_object\"," +
                "\"fun\":\"mapreduce_linkfun\"}," +
            "\"n_val\":3," +
            "\"old_vclock\":86400," +
            "\"small_vclock\":10," +
            "\"young_vclock\":20}," +
            "\"keys\":" +
                "[\"j\",\"k\",\"l\"]}";
    final InputStream STREAM = new ByteArrayInputStream( 
        ("{\"props\":" +
            "{\"name\":\"b\"," +
            "\"allow_mult\":false," +
            "\"big_vclock\":50," +
            "\"chash_keyfun\":" +
                "{\"mod\":\"riak_util\"," +
                "\"fun\":\"chash_std_keyfun\"}," +
            "\"linkfun\":" +
                "{\"mod\":\"jiak_object\"," +
                "\"fun\":\"mapreduce_linkfun\"}," +
            "\"n_val\":3," +
            "\"old_vclock\":86400," +
            "\"small_vclock\":10," +
            "\"young_vclock\":20}}" +
        "{\"keys\":[\"j\"]}{\"keys\":[]}{\"keys\":[]}{\"keys\":[\"k\",\"l\"]}")
        .getBytes());

    @Test public void doesnt_throw_on_null_impl() throws JSONException, IOException {
        new BucketResponse(null);
    }

    @Test public void parses_schema_field() throws JSONException, IOException {
        HttpResponse mockHttpResponse = mock(HttpResponse.class);
        when(mockHttpResponse.getBody()).thenReturn(BODY);
        when(mockHttpResponse.isSuccess()).thenReturn(true);
        
        BucketResponse impl = new BucketResponse(mockHttpResponse);
        
        assertEquals(false, impl.getBucketInfo().getAllowMult());
        assertEquals(3, impl.getBucketInfo().getNVal().intValue());
        assertEquals("riak_util:chash_std_keyfun", impl.getBucketInfo().getCHashFun());
        assertEquals("jiak_object:mapreduce_linkfun", impl.getBucketInfo().getLinkFun());
    }

    @Test public void parses_keys_field() throws Exception {
        HttpResponse mockHttpResponse = mock(HttpResponse.class);
        when(mockHttpResponse.getBody()).thenReturn(BODY);
        when(mockHttpResponse.isSuccess()).thenReturn(true);
        
        BucketResponse impl = new BucketResponse(mockHttpResponse);
        Collection<String> keys = impl.getBucketInfo().getKeys();
        
        assertEquals(3, keys.size());
        assertTrue(keys.contains("j"));
        assertTrue(keys.contains("k"));
        assertTrue(keys.contains("l"));
    }

    @Test public void parses_streamed_schema_field() throws JSONException, IOException {
        HttpResponse mockHttpResponse = mock(HttpResponse.class);
        when(mockHttpResponse.getStream()).thenReturn(STREAM);
        when(mockHttpResponse.isStreamed()).thenReturn(true);
        when(mockHttpResponse.isSuccess()).thenReturn(true);
        
        BucketResponse impl = new BucketResponse(mockHttpResponse);
        
        assertEquals(false, impl.getBucketInfo().getAllowMult());
        assertEquals(3, impl.getBucketInfo().getNVal().intValue());
        assertEquals("riak_util:chash_std_keyfun", impl.getBucketInfo().getCHashFun());
        assertEquals("jiak_object:mapreduce_linkfun", impl.getBucketInfo().getLinkFun());
    }

    @Test public void parses_streamed_keys() throws Exception {
        HttpResponse mockHttpResponse = mock(HttpResponse.class);
        when(mockHttpResponse.getStream()).thenReturn(STREAM);
        when(mockHttpResponse.isStreamed()).thenReturn(true);
        when(mockHttpResponse.isSuccess()).thenReturn(true);
        
        BucketResponse impl = new BucketResponse(mockHttpResponse);
        Collection<String> keys = impl.getBucketInfo().getKeys();

        Iterator<String> key = keys.iterator();
        assertEquals("j", key.next());
        assertEquals("k", key.next());
        assertEquals("l", key.next());
    }
}
