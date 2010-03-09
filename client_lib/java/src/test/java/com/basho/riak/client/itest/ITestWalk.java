package com.basho.riak.client.itest;

import static com.basho.riak.client.itest.Utils.*;
import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import com.basho.riak.client.RiakClient;
import com.basho.riak.client.RiakLink;
import com.basho.riak.client.RiakObject;
import com.basho.riak.client.response.WalkResponse;

public class ITestWalk {
    
    public static String RIAK_URL = "http://127.0.0.1:8098/riak";

    @Test
    public void test_walk() {
        final RiakClient c = new RiakClient(RIAK_URL);
        final String BUCKET = "test_walk";
        final String ROOT = "root";
        final String LEAF1 = "leaf1";
        final String LEAF2 = "leaf2";
        final String EXCLUDED_LEAF = "excluded_leaf";
        final String INCLUDED_VALUE = "included";
        final String EXCLUDED_VALUE = "excluded";
        final String TAG_INCLUDE = "tag_include";
        final String TAG_EXCLUDE = "tag_exclude";

        // Clear out the objects we're testing with
        assertSuccess(c.delete(BUCKET, ROOT));
        assertSuccess(c.delete(BUCKET, LEAF1));
        assertSuccess(c.delete(BUCKET, LEAF2));
        assertSuccess(c.delete(BUCKET, EXCLUDED_LEAF));
        
        // Add a few objects
        RiakObject leaf1 = new RiakObject(BUCKET, LEAF1, INCLUDED_VALUE);
        RiakObject leaf2 = new RiakObject(BUCKET, LEAF2, INCLUDED_VALUE);
        RiakObject excludedLeaf = new RiakObject(BUCKET, EXCLUDED_LEAF, EXCLUDED_VALUE);
        RiakObject root = new RiakObject(c, BUCKET, ROOT)
                            .addLink(new RiakLink(BUCKET, LEAF1, TAG_INCLUDE))
                            .addLink(new RiakLink(BUCKET, LEAF2, TAG_INCLUDE))
                            .addLink(new RiakLink(BUCKET, EXCLUDED_LEAF, TAG_EXCLUDE));
        assertSuccess(c.store(root, WRITE_3_REPLICAS()));
        assertSuccess(c.store(leaf1, WRITE_3_REPLICAS()));
        assertSuccess(c.store(leaf2, WRITE_3_REPLICAS()));
        assertSuccess(c.store(excludedLeaf, WRITE_3_REPLICAS()));
        
        // Perform walk
        WalkResponse walkresp = root.walk(BUCKET, TAG_INCLUDE).run();
        assertSuccess(walkresp);
        assertTrue(walkresp.hasSteps());
        assertEquals(1, walkresp.getSteps().size());
        assertEquals(2, walkresp.getSteps().get(0).size());
        
        // Verify expected only linked to objects are returned
        List<? extends List<RiakObject>> steps = walkresp.getSteps();
        List<String> keys = new ArrayList<String>();
        for (List<RiakObject> step : steps) {
            for (RiakObject object : step) {
                keys.add(object.getKey());
                assertEquals(INCLUDED_VALUE, object.getValue());
            }
        }
        assertTrue(keys.contains(LEAF1));
        assertTrue(keys.contains(LEAF2));
    }
}
