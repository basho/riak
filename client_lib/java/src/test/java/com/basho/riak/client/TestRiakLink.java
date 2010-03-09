package com.basho.riak.client;

import static org.junit.Assert.*;

import org.junit.Test;

public class TestRiakLink {

    @Test public void constructor_args_persisted() {
        RiakLink link = new RiakLink("bucket", "key", "tag");
        assertEquals("bucket", link.getBucket());
        assertEquals("key", link.getKey());
        assertEquals("tag", link.getTag());
    }
    
    @Test public void equals_handles_null_values() {
        RiakLink link1 = new RiakLink("bucket", "key", "tag");
        RiakLink link2 = new RiakLink(null, null, null);
        assertFalse(link1.equals(link2));
        assertFalse(link2.equals(link1));
    }
    
    @Test public void equals_performs_equality_check_on_fields() {
        RiakLink link1 = new RiakLink("bucket", "key", "tag");
        RiakLink link2 = new RiakLink("bucket", "key", "tag");
        RiakLink link3 = new RiakLink("bucket", "key", "different");
        assertTrue(link1.equals(link2));
        assertFalse(link1.equals(link3));
    }
}
