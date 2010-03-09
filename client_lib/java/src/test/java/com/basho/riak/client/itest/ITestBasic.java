package com.basho.riak.client.itest;

import static com.basho.riak.client.itest.Utils.*;
import static org.junit.Assert.*;

import org.junit.Test;

import com.basho.riak.client.RiakBucketInfo;
import com.basho.riak.client.RiakClient;
import com.basho.riak.client.RiakLink;
import com.basho.riak.client.RiakObject;
import com.basho.riak.client.response.BucketResponse;
import com.basho.riak.client.response.FetchResponse;
import com.basho.riak.client.response.StoreResponse;

/**
 * Basic exercises such as store, fetch, and modify objects for the Riak client.
 * Assumes Riak is reachable at 127.0.0.1:8098/riak.
 */
public class ITestBasic {

    public static String RIAK_URL = "http://127.0.0.1:8098/riak";

    @Test public void store_fetch_modify() {
        final RiakClient c = new RiakClient(RIAK_URL);
        final String VALUE1 = "value1";
        final String VALUE2 = "value2";
        final RiakLink LINK = new RiakLink("bucket", "key", "tag");
        final String USERMETA_KEY = "usermeta";
        final String USERMETA_VALUE = "value";
        final String BUCKET = "store_fetch_modify";
        final String KEY = "key";

        // Set bucket schema to return siblings
        RiakBucketInfo bucketInfo = new RiakBucketInfo();
        bucketInfo.setAllowMult(true);
        assertSuccess(c.setBucketSchema(BUCKET, bucketInfo));

        // Make sure object doesn't exist
        assertSuccess(c.delete(BUCKET, KEY, WRITE_3_REPLICAS()));

        FetchResponse fetchresp = c.fetch(BUCKET, KEY);
        assertEquals(404, fetchresp.getStatusCode());

        // Store a new object
        RiakObject o = new RiakObject(BUCKET, KEY, VALUE1);
        StoreResponse storeresp = c.store(o, WRITE_3_REPLICAS());
        assertSuccess(storeresp);

        // Retrieve it back
        fetchresp = c.fetch(BUCKET, KEY);
        assertSuccess(fetchresp);
        assertTrue(fetchresp.hasObject());
        assertEquals(VALUE1, fetchresp.getObject().getValue());
        assertTrue(fetchresp.getObject().getLinks().isEmpty());
        assertTrue(fetchresp.getObject().getUsermeta().isEmpty());

        // Modify and store it
        o = fetchresp.getObject();
        o.setValue(VALUE2);
        o.getLinks().add(LINK);
        o.getUsermeta().put(USERMETA_KEY, USERMETA_VALUE);
        storeresp = c.store(o, WRITE_3_REPLICAS());
        assertSuccess(storeresp);

        // Validate modification happened
        fetchresp = c.fetch(BUCKET, KEY);
        assertSuccess(fetchresp);
        assertTrue(fetchresp.hasObject());
        assertFalse(fetchresp.hasSiblings());
        assertEquals(VALUE2, fetchresp.getObject().getValue());
        assertEquals(1, fetchresp.getObject().getLinks().size());
        assertEquals(LINK, fetchresp.getObject().getLinks().get(0));
        assertEquals(USERMETA_VALUE, fetchresp.getObject().getUsermeta().get(USERMETA_KEY));
    }

    @Test public void test_bucket_schema() {
        final RiakClient c = new RiakClient(RIAK_URL);
        final String BUCKET = "test_bucket_schema";
        final String KEY1 = "key1";
        final String KEY2 = "key2";
        final String KEY3 = "key3";
        final String CHASH_MOD = "riak_util";
        final String CHASH_FUN = "chash_bucketonly_keyfun";

        // Clear out the objects we're testing with
        assertSuccess(c.delete(BUCKET, KEY1));
        assertSuccess(c.delete(BUCKET, KEY2));
        assertSuccess(c.delete(BUCKET, KEY3));

        // Add a few objects
        assertSuccess(c.store(new RiakObject(BUCKET, KEY1, "v"), WRITE_3_REPLICAS()));
        assertSuccess(c.store(new RiakObject(BUCKET, KEY2, "v"), WRITE_3_REPLICAS()));
        assertSuccess(c.store(new RiakObject(BUCKET, KEY3, "v"), WRITE_3_REPLICAS()));

        // Get the current bucket schema and contents
        BucketResponse bucketresp = c.listBucket(BUCKET);
        assertSuccess(bucketresp);
        assertTrue(bucketresp.hasBucketInfo());
        RiakBucketInfo bucketInfo = bucketresp.getBucketInfo();
        int nval = bucketInfo.getNVal();

        // Verify that contents are correct
        assertTrue("Should contain key1: " + bucketInfo.getKeys(), bucketInfo.getKeys().contains(KEY1));
        assertTrue("Should contain key2: " + bucketInfo.getKeys(), bucketInfo.getKeys().contains(KEY2));
        assertTrue("Should contain key3: " + bucketInfo.getKeys(), bucketInfo.getKeys().contains(KEY3));

        // Change some properties
        bucketInfo.setNVal(nval + 1);
        bucketInfo.setCHashFun(CHASH_MOD, CHASH_FUN);
        assertSuccess(c.setBucketSchema(BUCKET, bucketInfo));

        // Verify that properties stuck
        bucketresp = c.listBucket(BUCKET);
        assertSuccess(bucketresp);
        assertTrue(bucketresp.hasBucketInfo());
        bucketInfo = bucketresp.getBucketInfo();
        assertEquals(nval + 1, bucketInfo.getNVal().intValue());
        assertEquals(CHASH_MOD + ":" + CHASH_FUN, bucketInfo.getCHashFun());
    }
}
