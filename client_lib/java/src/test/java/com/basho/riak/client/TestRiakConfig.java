package com.basho.riak.client;

import static org.junit.Assert.*;

import org.junit.Test;

public class TestRiakConfig {

    @Test public void chomps_ending_slash() {
        final String URL = "url";
        RiakConfig impl = new RiakConfig(URL + "/");
        assertEquals(URL, impl.getUrl());

        impl.setUrl(URL + "/");
        assertEquals(URL, impl.getUrl());

        impl = new RiakConfig("ip", "port", URL + "/");
        assertFalse(impl.getUrl().endsWith("/"));
    }

    @Test public void builds_correct_url_from_ip_port_and_prefix() {
        RiakConfig impl = new RiakConfig("ip", "port", "/prefix");
        assertEquals("http://ip:port/prefix", impl.getUrl());
    }
    
    @Test public void calculates_correct_base_url() {
        RiakConfig impl = new RiakConfig("http://ip:port/path/to/riak");
        assertEquals("http://ip:port", impl.getBaseUrl());
        
        impl = new RiakConfig("http://ip:port/prefix");
        assertEquals("http://ip:port", impl.getBaseUrl());
        
        impl = new RiakConfig("http://ip:port");
        assertEquals("http://ip:port", impl.getBaseUrl());

        impl = new RiakConfig("http://ip");
        assertEquals("http://ip", impl.getBaseUrl());

        impl = new RiakConfig("ip:port/prefix");
        assertEquals("ip:port", impl.getBaseUrl());

        impl = new RiakConfig("ip/prefix");
        assertEquals("ip", impl.getBaseUrl());

        impl = new RiakConfig("ip");
        assertEquals("ip", impl.getBaseUrl());
    }

    @Test public void calculates_correct_base_mapred_url() {
        RiakConfig impl = new RiakConfig("http://ip:port/path/to/riak");
        assertEquals("http://ip:port/mapred", impl.getMapReduceUrl());

        impl.setMapReducePath("/path/to/mapred/");
        assertEquals("http://ip:port/path/to/mapred", impl.getMapReduceUrl());
    }
}
