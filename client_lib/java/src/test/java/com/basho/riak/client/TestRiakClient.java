package com.basho.riak.client;

import static org.junit.Assert.*;
import static org.mockito.Matchers.*;
import static org.mockito.Mockito.*;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import com.basho.riak.client.request.RequestMeta;
import com.basho.riak.client.request.RiakWalkSpec;
import com.basho.riak.client.response.FetchResponse;
import com.basho.riak.client.response.HttpResponse;
import com.basho.riak.client.response.RiakResponseRuntimeException;
import com.basho.riak.client.response.StreamHandler;
import com.basho.riak.client.response.WalkResponse;
import com.basho.riak.client.util.ClientHelper;
import com.basho.riak.client.util.Constants;

public class TestRiakClient {

    final String bucket = "bucket";
    final String key = "key";
    final String walkSpec = "walkSpec";
    final String mrJob = "mrJob";

    @Mock ClientHelper mockHelper;
    @Mock RiakBucketInfo bucketInfo;
    @Mock StreamHandler handler;
    @Mock RequestMeta meta;
    @Mock RiakObject object;
    @Mock RiakWalkSpec riakWalkSpec;
    @Mock HttpResponse mockHttpResponse;
    RiakClient impl;

    @Before public void setup() {
        MockitoAnnotations.initMocks(this);
        when(mockHttpResponse.getHttpHeaders()).thenReturn(new HashMap<String, String>());
        impl = new RiakClient(mockHelper);
    }
    
    @Test public void interface_methods_defer_to_helper() throws IOException {

        impl.setBucketSchema(bucket, bucketInfo, meta);
        verify(mockHelper).setBucketSchema(eq(bucket), any(JSONObject.class), same(meta));

        impl.listBucket(bucket, meta);
        verify(mockHelper).listBucket(bucket, meta, false);

        impl.streamBucket(bucket, meta);
        verify(mockHelper).listBucket(bucket, meta, true);

        impl.store(object, meta);
        verify(mockHelper).store(object, meta);

        impl.fetchMeta(bucket, key, meta);
        verify(mockHelper).fetchMeta(bucket, key, meta);

        impl.fetch(bucket, key, meta, false);
        verify(mockHelper).fetch(bucket, key, meta, false);

        impl.stream(bucket, key, handler, meta);
        verify(mockHelper).stream(bucket, key, handler, meta);

        impl.delete(bucket, key, meta);
        verify(mockHelper).delete(bucket, key, meta);

        impl.walk(bucket, key, walkSpec, meta);
        verify(mockHelper).walk(bucket, key, walkSpec, meta);
        
        impl.mapReduce(mrJob, meta);
        verify(mockHelper).mapReduce(mrJob, meta);
    }
    
    @Test public void convenience_methods_defer_to_main_methods_with_null_meta() {
        impl = spy(impl);

        impl.setBucketSchema(bucket, bucketInfo);
        verify(impl).setBucketSchema(bucket, bucketInfo, null);

        impl.listBucket(bucket);
        verify(impl).listBucket(bucket, null);

        impl.streamBucket(bucket);
        verify(impl).streamBucket(bucket, null);

        impl.store(object);
        verify(impl).store(object, null);

        impl.fetchMeta(bucket, key);
        verify(impl).fetchMeta(bucket, key, null);

        impl.fetch(bucket, key);
        verify(impl).fetch(bucket, key, null, false);

        impl.fetch(bucket, key, meta);
        verify(impl).fetch(bucket, key, meta, false);

        impl.stream(bucket, key);
        verify(impl).fetch(bucket, key, null, true);

        impl.stream(bucket, key, meta);
        verify(impl).fetch(bucket, key, meta, true);

        impl.delete(bucket, key);
        verify(impl).delete(bucket, key, null);

        when(riakWalkSpec.toString()).thenReturn(walkSpec);
        impl.walk(bucket, key, walkSpec);
        impl.walk(bucket, key, riakWalkSpec);
        verify(impl, times(2)).walk(bucket, key, walkSpec, null);
        
        impl.mapReduce(mrJob);
        verify(impl).mapReduce(mrJob, null);
    }
    
    @Test public void method_defers_to_helper_on_expection() throws JSONException, IOException {
        impl = spy(impl);
        
        doThrow(new JSONException("")).when(impl).getBucketResponse(any(HttpResponse.class));
        doThrow(new RiakResponseRuntimeException(null)).when(impl).getFetchResponse(any(HttpResponse.class));
        doThrow(new RiakResponseRuntimeException(null)).when(impl).getWalkResponse(any(HttpResponse.class));
        doThrow(new JSONException("")).when(impl).getMapReduceResponse(any(HttpResponse.class));

        impl.listBucket(bucket, meta);
        verify(mockHelper).toss(any(RiakResponseRuntimeException.class));
        reset(mockHelper);

        impl.streamBucket(bucket, meta);
        verify(mockHelper).toss(any(RiakResponseRuntimeException.class));
        reset(mockHelper);

        impl.fetchMeta(bucket, key, meta);
        verify(mockHelper).toss(any(RiakResponseRuntimeException.class));
        reset(mockHelper);
    
        impl.fetch(bucket, key, meta);
        verify(mockHelper).toss(any(RiakResponseRuntimeException.class));
        reset(mockHelper);

        impl.stream(bucket, key, meta);
        verify(mockHelper).toss(any(RiakResponseRuntimeException.class));
        reset(mockHelper);

        impl.walk(bucket, key, "", meta);
        verify(mockHelper).toss(any(RiakResponseRuntimeException.class));
        reset(mockHelper);
        
        impl.mapReduce(mrJob, meta);
        verify(mockHelper).toss(any(RiakResponseRuntimeException.class));
        reset(mockHelper);
    }
    
    @Test public void setBucketSchema_puts_schema_in_props_field() {
        final JSONObject mockJSONObject = mock(JSONObject.class);
        
        when(mockHelper.setBucketSchema(eq(bucket), any(JSONObject.class), same(meta))).thenAnswer(new Answer<HttpResponse>() {
            public HttpResponse answer(InvocationOnMock invocation) throws Throwable {
                assertSame(mockJSONObject,
                           ((JSONObject) invocation.getArguments()[1])      // second argument is "schema"
                               .getJSONObject(Constants.FL_SCHEMA));
                return null;
            } 
        });
        when(bucketInfo.getSchema()).thenReturn(mockJSONObject);
        
        impl.setBucketSchema(bucket, bucketInfo, meta);
        
        // since we need to peer into the internals of the JSONObject created by setBucketSchema(), 
        // verification is done in the Answer impl above.
    }

    @Test public void fetch_adds_accept_header_for_multipart_content() {
        final RequestMeta meta = new RequestMeta();
        impl.fetch(bucket, key, meta, false);
        assertTrue(meta.getHeader("accept").contains("multipart/mixed"));
        
        meta.setHeader("accept", "text/plain");
        impl.fetch(bucket, key, meta, false);
        assertTrue(meta.getHeader("accept").contains("text/plain"));
        assertTrue(meta.getHeader("accept").contains("multipart/mixed"));
    }

    @Test public void fetch_meta_asks_fetchresponse_to_associate_new_objects_with_client() {
        when(mockHelper.fetchMeta(anyString(), anyString(), any(RequestMeta.class))).thenReturn(mockHttpResponse);
        when(mockHttpResponse.isSuccess()).thenReturn(true);
        
        FetchResponse r = impl.fetchMeta(bucket, key);

        assertSame(impl, r.getObject().getRiakClient());
    }

    @Test public void fetch_asks_fetchresponse_to_associate_new_objects_with_client() {
        when(mockHelper.fetch(anyString(), anyString(), any(RequestMeta.class), anyBoolean())).thenReturn(mockHttpResponse);
        when(mockHttpResponse.isSuccess()).thenReturn(true);
        
        FetchResponse r = impl.fetch(bucket, key);

        assertSame(impl, r.getObject().getRiakClient());
    }

    @Test public void stream_asks_fetchresponse_to_associate_new_objects_with_client() {
        when(mockHelper.fetch(anyString(), anyString(), any(RequestMeta.class), anyBoolean())).thenReturn(mockHttpResponse);
        when(mockHttpResponse.isSuccess()).thenReturn(true);
        
        FetchResponse r = impl.stream(bucket, key);

        assertSame(impl, r.getObject().getRiakClient());
    }

    @Test public void walk_asks_fetchresponse_to_associate_new_objects_with_client() {
        @SuppressWarnings("serial") final Map<String, String> HEADERS = new HashMap<String, String>() {{
            put("Content-Type".toLowerCase(), "multipart/mixed; boundary=BCVLGEKnH0gY7KsH5nW3xnzhYbU");
        }};
        final String BODY = "\n" + "--BCVLGEKnH0gY7KsH5nW3xnzhYbU\n"
            + "Content-Type: multipart/mixed; boundary=7Ymillu08Tqzwb9Cm6Bs8OewFd5\n" 
            + "\n"
            + "--7Ymillu08Tqzwb9Cm6Bs8OewFd5\n" 
            + "Location: /riak/b/k1\n" 
            + "\n" 
            + "foo\n"
            + "--7Ymillu08Tqzwb9Cm6Bs8OewFd5--\n"
            + "\n"
            + "--BCVLGEKnH0gY7KsH5nW3xnzhYbU--\n";
    
        when(mockHelper.walk(anyString(), anyString(), anyString(), any(RequestMeta.class))).thenReturn(mockHttpResponse);
        when(mockHttpResponse.getHttpHeaders()).thenReturn(HEADERS);
        when(mockHttpResponse.getBody()).thenReturn(BODY);
        when(mockHttpResponse.isSuccess()).thenReturn(true);
        
        WalkResponse r = impl.walk(bucket, key, walkSpec);

        assertSame(impl, r.getSteps().get(0).get(0).getRiakClient());
   }
}