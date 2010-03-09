package com.basho.riak.client.itest;

import static com.basho.riak.client.itest.Utils.*;
import static org.junit.Assert.*;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.junit.Test;

import com.basho.riak.client.RiakClient;
import com.basho.riak.client.RiakObject;
import com.basho.riak.client.response.BucketResponse;
import com.basho.riak.client.response.FetchResponse;
import com.basho.riak.client.util.ClientUtils;

public class ITestStreaming {

    static String RIAK_URL = "http://127.0.0.1:8098/riak";
    
    @Test public void stream_keys() {
        final RiakClient c = new RiakClient(RIAK_URL);
        final String BUCKET = "test_stream_keys";
        final int NUM_KEYS = 20;
        
        // Add objects
        for (int i = 0; i < NUM_KEYS; i++) {
            assertSuccess(c.store(new RiakObject(BUCKET, "key" + Integer.toString(i), "v"), WRITE_3_REPLICAS()));
        }

        // Iterate over keys in bucket
        BucketResponse r = c.streamBucket(BUCKET);
        assertSuccess(r);

        List<String> keys = new ArrayList<String>();
        for (String key : r.getBucketInfo().getKeys()) {
            keys.add(key);
        }
        for (int i = 0; i < NUM_KEYS; i++) {
            assertTrue("Should contain key" + i + ": " + keys, keys.contains("key" + i));
        }
        
        r.close();

        // Clear out the objects we're testing with
        for (int i = 0; i < NUM_KEYS; i++) {
            assertSuccess(c.delete(BUCKET, "key" + Integer.toString(i)));
        }
    }

    @Test public void stream_object() throws IOException {
        final RiakClient c = new RiakClient(RIAK_URL);
        final String BUCKET = "test_stream_object";
        final String KEY = "key";
        final byte[] bytes = new byte[512];
        new Random().nextBytes(bytes);
        
        // Add object
        assertSuccess(c.store(new RiakObject(BUCKET, KEY, new String(bytes)), WRITE_3_REPLICAS()));

        // Stream the object back
        FetchResponse r = c.stream(BUCKET, KEY);
        assertSuccess(r);
        
        assertTrue(r.isStreamed());
        assertNotNull(r.getStream());
        
        ByteArrayOutputStream os = new ByteArrayOutputStream(512);
        ClientUtils.copyStream(r.getStream(), os);
        assertArrayEquals(bytes, os.toByteArray());
        
        r.close();
    }
}
