package com.basho.riak.client;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;

import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Test;

import com.basho.riak.client.util.Constants;

public class TestRiakBucketInfo {

    RiakBucketInfo impl;
    JSONObject schema = new JSONObject();
    List<String> keys = new ArrayList<String>();

    @Test public void constructor_schema_and_keys_returned_by_accessors() {
        final String KEY = "key";
        keys.add(KEY);

        impl = new RiakBucketInfo(schema, keys);

        assertSame(schema, impl.getSchema());
        assertTrue(impl.getKeys().contains(KEY));
    }

    @Test public void objects_constructed_for_null_constructor_arguments() {
        impl = new RiakBucketInfo(null, null);
        assertNotNull(impl.getSchema());
        assertNotNull(impl.getKeys());
    }

    @Test public void allow_mult_sets_schema() throws JSONException {
        final boolean ALLOW_MULT = true;

        impl = new RiakBucketInfo();
        impl.setAllowMult(ALLOW_MULT);

        assertEquals(ALLOW_MULT, impl.getAllowMult());
        assertEquals(ALLOW_MULT, impl.getSchema().getBoolean(Constants.FL_SCHEMA_ALLOW_MULT));
    }

    @Test public void n_val_sets_schema() throws JSONException {
        final int N_VAL = 10;

        impl = new RiakBucketInfo();
        impl.setNVal(N_VAL);

        assertEquals(N_VAL, (int) impl.getNVal());
        assertEquals(N_VAL, impl.getSchema().getInt(Constants.FL_SCHEMA_NVAL));
    }

    @Test public void link_fun_sets_schema() throws JSONException {
        final String LINK_MOD = "link_mod";
        final String LINK_FUN = "link_fun";

        impl = new RiakBucketInfo();
        impl.setLinkFun(LINK_MOD, LINK_FUN);

        assertEquals(LINK_MOD + ":" + LINK_FUN, impl.getLinkFun());
        assertEquals(LINK_MOD, 
                     impl.getSchema().getJSONObject(Constants.FL_SCHEMA_LINKFUN).getString(Constants.FL_SCHEMA_LINKFUN_MOD));
        assertEquals(LINK_FUN,
                     impl.getSchema().getJSONObject(Constants.FL_SCHEMA_LINKFUN).getString(Constants.FL_SCHEMA_LINKFUN_FUN));
    }

    @Test public void chash_fun_sets_schema() throws JSONException {
        final String HASH_MOD = "hash_mod";
        final String HASH_FUN = "hash_fun";

        impl = new RiakBucketInfo();
        impl.setCHashFun(HASH_MOD, HASH_FUN);

        assertEquals(HASH_MOD + ":" + HASH_FUN, impl.getCHashFun());
        assertEquals(HASH_MOD,
                     impl.getSchema().getJSONObject(Constants.FL_SCHEMA_CHASHFUN).getString(Constants.FL_SCHEMA_CHASHFUN_MOD));
        assertEquals(HASH_FUN,
                     impl.getSchema().getJSONObject(Constants.FL_SCHEMA_CHASHFUN).getString(Constants.FL_SCHEMA_CHASHFUN_FUN));
    }
}
