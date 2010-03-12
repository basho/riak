package com.basho.riak.client.util;

import static org.junit.Assert.*;

import java.util.Map;

import org.junit.Test;

public class TestLinkHeader {

    @Test public void parses_null_and_empty_headers() {
        String header = null;
        Map<String, Map<String, String>> links = LinkHeader.parse(header);
        assertEquals(0, links.size());

        header = "";
        links = LinkHeader.parse(header);
        assertEquals(0, links.size());

        header = " ";
        links = LinkHeader.parse(header);
        assertEquals(0, links.size());
    }

    @Test public void ignores_malformed_header_components() {
        final String header = "</res>, ;>>abc, </res2>";
        Map<String, Map<String, String>> links = LinkHeader.parse(header);
        assertEquals(2, links.size());
        assertNotNull(links.get("/res"));
        assertNotNull(links.get("/res2"));
    }

    @Test public void ignores_empty_elements() {
        final String header = ",</res>; param=value, ,, </res2>,";
        Map<String, Map<String, String>> links = LinkHeader.parse(header);
        
        assertEquals(2, links.size());
        
        assertNotNull(links.get("/res"));
        assertEquals(1, links.get("/res").keySet().size());
        assertEquals("value", links.get("/res").get("param"));

        assertNotNull(links.get("/res2"));
        assertEquals(0, links.get("/res2").keySet().size());
    }

    @Test public void parses_one_link_with_one_parameter() {
        final String header = "</res>; param=value";
        Map<String, Map<String, String>> links = LinkHeader.parse(header);
        assertEquals(1, links.size());
        assertNotNull(links.get("/res"));
        assertEquals("value", links.get("/res").get("param"));
    }

    @Test public void parses_multiple_links() {
        final String header = "</res>; param=value, </res2>; param2=value2, </another/res>; param3=value3";
        Map<String, Map<String, String>> links = LinkHeader.parse(header);
        assertEquals(3, links.size());
        assertNotNull(links.get("/res"));
        assertNotNull(links.get("/res2"));
        assertNotNull(links.get("/another/res"));
        assertEquals("value", links.get("/res").get("param"));
        assertEquals("value2", links.get("/res2").get("param2"));
        assertEquals("value3", links.get("/another/res").get("param3"));
    }

    @Test public void handles_varying_whitespace() {
        final String header = "</res>;param=value;param2=value2,</res2>;   param3=value3;  param4=value4,    </another/res>";
        Map<String, Map<String, String>> links = LinkHeader.parse(header);
        assertEquals(3, links.size());
        assertNotNull(links.get("/res"));
        assertNotNull(links.get("/res2"));
        assertNotNull(links.get("/another/res"));
        assertEquals("value", links.get("/res").get("param"));
        assertEquals("value2", links.get("/res").get("param2"));
        assertEquals("value3", links.get("/res2").get("param3"));
        assertEquals("value4", links.get("/res2").get("param4"));
        assertEquals(0, links.get("/another/res").keySet().size());
    }
    
    @Test public void handles_malformed_links() {
        String header = "; param=value";
        Map<String, Map<String, String>> links = LinkHeader.parse(header);
        assertEquals(0, links.size());
        
        header = "<>/res>; param=value";
        links = LinkHeader.parse(header);
        assertEquals(0, links.size());

        header = "<; param=value";
        links = LinkHeader.parse(header);
        assertEquals(0, links.size());
    }

    @Test public void parses_link_with_no_parameters() {  
        String header = "</res>";
        Map<String, Map<String, String>> links = LinkHeader.parse(header);
        assertEquals(1, links.size());
        assertEquals(0, links.get("/res").keySet().size());
    }

    @Test public void parses_link_with_multiple_parameters() {  
        String header = "</res>; param1=value1; param2=value2; param3=value3";
        Map<String, Map<String, String>> links = LinkHeader.parse(header);
        assertEquals(1, links.size());
        assertEquals(3, links.get("/res").keySet().size());
        assertEquals("value1", links.get("/res").get("param1"));
        assertEquals("value2", links.get("/res").get("param2"));
        assertEquals("value3", links.get("/res").get("param3"));
    }

    @Test public void parses_link_with_quoted_parameter() {
        String header = "</res>; param1=\"\\\"\\value1\\\"\"";   // backslash escaped quote and 'v' in quoted string
        Map<String, Map<String, String>> links = LinkHeader.parse(header);
        assertEquals(1, links.size());
        assertEquals("\"value1\"", links.get("/res").get("param1"));
    }

    @Test public void maps_valueless_parameter_to_null() {
        final String header = "</res>; param";
        Map<String, Map<String, String>> links = LinkHeader.parse(header);
        
        assertTrue(links.get("/res").keySet().contains("param"));
        assertEquals(null, links.get("/res").get("param"));
    }

    @Test public void ignores_links_with_malformed_parameters() {
        String header = "</res>; param=, </res2>";
        Map<String, Map<String, String>> links = LinkHeader.parse(header);
        assertEquals(1, links.size());
        assertNotNull(links.get("/res2"));

        header = "</res>; @, </res2>";
        links = LinkHeader.parse(header);
        assertEquals(1, links.size());
        assertNotNull(links.get("/res2"));

        header = "</res>; \"param\", </res2>";
        links = LinkHeader.parse(header);
        assertEquals(1, links.size());
        assertNotNull(links.get("/res2"));
}
}
