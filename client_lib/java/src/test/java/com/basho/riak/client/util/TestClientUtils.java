package com.basho.riak.client.util;

import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpMethodRetryHandler;
import org.apache.commons.httpclient.params.HttpClientParams;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.basho.riak.client.RiakClient;
import com.basho.riak.client.RiakConfig;
import com.basho.riak.client.RiakLink;
import com.basho.riak.client.RiakObject;

public class TestClientUtils {

    @Mock RiakClient mockRiakClient;

    RiakConfig config = new RiakConfig();

    @Test public void newHttpClient_uses_configs_http_client_if_exists() {
        MockitoAnnotations.initMocks(this);

        HttpClient httpClient = new HttpClient();
        config.setHttpClient(httpClient);
        assertSame(httpClient, ClientUtils.newHttpClient(config));
    }

    @Test public void newHttpClient_sets_max_total_connections() {
        final int maxConnections = 11;
        config.setMaxConnections(maxConnections);

        HttpClient httpClient = ClientUtils.newHttpClient(config);

        assertEquals(maxConnections, httpClient.getHttpConnectionManager().getParams().getMaxTotalConnections());
    }

    @Test public void newHttpClient_sets_connection_timeout() {
        final long timeout = 11;
        config.setTimeout(timeout);

        HttpClient httpClient = ClientUtils.newHttpClient(config);

        assertEquals(timeout, httpClient.getParams().getConnectionManagerTimeout());
    }

    @Test public void newHttpClient_sets_retry_handler() {
        final HttpMethodRetryHandler handler = mock(HttpMethodRetryHandler.class);
        config.setRetryHandler(handler);

        HttpClient httpClient = ClientUtils.newHttpClient(config);

        assertSame(handler, httpClient.getParams().getParameter(HttpClientParams.RETRY_HANDLER));
    }

    @Test public void makeURI_url_encodes_bucket() {
        String url = ClientUtils.makeURI(config, "/");
        assertTrue("Expected bucket to be encoded as %2F in URL" + url, url.endsWith("/%2F"));

    }

    @Test public void makeURI_url_encodes_key() {
        String url = ClientUtils.makeURI(config, "b", "/");
        assertTrue("Expected key to be encoded as %2F in URL: " + url, url.endsWith("/b/%2F"));
    }

    @Test public void makeURI_prepends_slash_to_extra_if_extra_is_a_path_component() {
        String url = ClientUtils.makeURI(config, "b", "k", "path_component");
        assertTrue("Expected a slash before path_component in URL: " + url, url.endsWith("/path_component"));
    }

    @Test public void getPathFromURL_masks_full_url() {
        String path = ClientUtils.getPathFromUrl("http://host.com:10000/path/to/object");
        assertEquals("/path/to/object", path);
    }

    @Test public void getPathFromURL_masks_portless_url() {
        String path = ClientUtils.getPathFromUrl("http://host.com/path/to/object");
        assertEquals("/path/to/object", path);
    }

    @Test public void getPathFromURL_masks_schemeless_url() {
        String path = ClientUtils.getPathFromUrl("host.com:10000/path/to/object");
        assertEquals("/path/to/object", path);
    }

    @Test public void getPathFromURL_returns_path_in_path_only_url() {
        String path = ClientUtils.getPathFromUrl("/path/to/object");
        assertEquals("/path/to/object", path);
    }

    @Test public void getPathFromURL_handles_empty_url() {
        String path = ClientUtils.getPathFromUrl("");
        assertEquals("", path);
    }

    @Test public void getPathFromURL_handles_null() {
        String path = ClientUtils.getPathFromUrl(null);
        assertNull(path);
    }

    @Test public void unquote_string_removes_surrounding_quotes() {
        String s = ClientUtils.unquoteString("\"string\"");
        assertFalse("Starting quote should be removed", s.startsWith("\""));
        assertFalse("Trailing quote should be removed", s.endsWith("\""));
    }

    @Test public void unquote_string_unescapes_blackslash_escaped_chars() {
        String s = ClientUtils.unquoteString("\"\\\\\\a\\n\"");
        assertEquals("\\an", s);
    }

    @Test public void unquote_string_handles_trailing_blackslash() {
        String s = ClientUtils.unquoteString("\"\\\\\\a\\n\\\"");
        assertEquals("\\an\\", s);
    }

    @Test public void unquote_string_handles_unquoted_string() {
        String s = ClientUtils.unquoteString("\\\\\\a\\n");
        assertEquals("\\an", s);
    }

    @Test public void json_opt_string_handles_any_type_and_nulls_without_throwing() throws JSONException {
        // optString() is used by jsonObjectAsMap and jsonArrayAsList with the
        // assumption that
        // it won't throw. Just making sure...
        JSONObject jsonObject = new JSONObject();
        JSONArray jsonArray = new JSONArray();

        assertEquals("", jsonObject.optString("k"));
        assertEquals("", jsonArray.optString(0));

        jsonObject.put("k", new JSONArray());
        jsonArray.put(0, new JSONArray());
        assertEquals("[]", jsonObject.optString("k"));
        assertEquals("[]", jsonArray.optString(0));

        jsonObject.put("k", new JSONObject());
        jsonArray.put(0, new JSONObject());
        assertEquals("{}", jsonObject.optString("k"));
        assertEquals("{}", jsonArray.optString(0));

        jsonObject.put("k", 12);
        jsonArray.put(0, 12);
        assertEquals("12", jsonObject.optString("k"));
        assertEquals("12", jsonArray.optString(0));

        jsonObject.put("k", true);
        jsonArray.put(0, true);
        assertEquals("true", jsonObject.optString("k"));
        assertEquals("true", jsonArray.optString(0));
    }

    @Test public void json_object_as_map_puts_all_fields_using_opt_string() throws JSONException {
        JSONObject json = spy(new JSONObject().put("k1", "v1").put("k2", "v2").put("k3", "v3"));
        Map<String, String> map = ClientUtils.jsonObjectAsMap(json);

        verify(json).optString("k1");
        verify(json).optString("k2");
        verify(json).optString("k3");

        assertEquals("v1", map.get("k1"));
        assertEquals("v2", map.get("k2"));
        assertEquals("v3", map.get("k3"));
    }

    @Test public void json_array_as_list_adds_all_elements_using_opt_string() {
        JSONArray json = spy(new JSONArray().put("v1").put("v2").put("v3"));
        List<String> list = ClientUtils.jsonArrayAsList(json);

        verify(json).optString(0);
        verify(json).optString(1);
        verify(json).optString(2);

        assertEquals("v1", list.get(0));
        assertEquals("v2", list.get(1));
        assertEquals("v3", list.get(2));
    }

    @Test public void parses_null_or_empty_link_header() {
        Collection<RiakLink> links;

        links = ClientUtils.parseLinkHeader(null);
        assertNotNull(links);
        assertTrue(links.isEmpty());

        ClientUtils.parseLinkHeader("");
        assertNotNull(links);
        assertTrue(links.isEmpty());
    }

    @Test public void parses_links_from_header() {
        // Rely on LinkHeader tests to verify that link parsing actually works
        // for other cases (single link, multiple links, malformed, etc)
        List<RiakLink> links = ClientUtils.parseLinkHeader("</link/1>; riaktag=t, </link/2>; riaktag=\"abc\"");
        assertEquals(2, links.size());
        assertEquals(new RiakLink("link", "1", "t"), links.get(0));
        assertEquals(new RiakLink("link", "2", "abc"), links.get(1));
    }

    @Test public void parse_usermeta_handles_null_or_empty_header_set() {
        Map<String, String> usermeta;

        usermeta = ClientUtils.parseUsermeta(null);
        assertNotNull(usermeta);
        assertTrue(usermeta.isEmpty());

        usermeta = ClientUtils.parseUsermeta(new HashMap<String, String>());
        assertNotNull(usermeta);
        assertTrue(usermeta.isEmpty());
    }

    @Test public void parse_usermeta_returns_correct_headers() {
        @SuppressWarnings("serial") Map<String, String> usermeta = new HashMap<String, String>() {
            {
                put("h1", "v1");
                put("h2", "v2");
                put("X-Riak-Meta-Test", "v");
            }
        };

        usermeta = ClientUtils.parseUsermeta(usermeta);
        assertEquals(1, usermeta.size());
        assertEquals("v", usermeta.get("Test"));
    }

    @Test public void parse_usermeta_is_case_insensitive() {
        @SuppressWarnings("serial") Map<String, String> usermeta = new HashMap<String, String>() {
            {
                put("X-Riak-Meta-Test", "v");
                put("x-riak-meta-Test2", "v2");
            }
        };

        usermeta = ClientUtils.parseUsermeta(usermeta);
        assertEquals(2, usermeta.size());
        assertEquals("v", usermeta.get("Test"));
        assertEquals("v2", usermeta.get("Test2"));
    }

    @Test public void parse_multipart_handles_null_params() {
        List<RiakObject> objects = ClientUtils.parseMultipart(null, null, null, null, null);
        assertNotNull(objects);
    }

    @Test public void parse_multipart_returns_correct_riak_and_bucket_and_key() {
        Map<String, String> headers = new HashMap<String, String>();
        headers.put("content-type", "multipart/mixed; boundary=boundary");
        String body = "\n--boundary\n" + "\n" + "--boundary--";

        List<RiakObject> objects = ClientUtils.parseMultipart(mockRiakClient, "b", "k", headers, body);
        assertEquals(1, objects.size());
        assertSame(mockRiakClient, objects.get(0).getRiakClient());
        assertEquals("b", objects.get(0).getBucket());
        assertEquals("k", objects.get(0).getKey());
    }

    @Test public void parse_multipart_returns_all_header_metadata_for_object() {
        Map<String, String> headers = new HashMap<String, String>();
        headers.put("content-type", "multipart/mixed; boundary=boundary");
        headers.put("x-riak-vclock", "vclock");
        String body = "\n--boundary\n" + "Content-Type: text/plain\n" + "Last-Modified: lastmod\n"
                      + "Link: </riak/b/l>; riaktag=t\n" + "ETag: vtag\n" + "X-Riak-Meta-Test: value\n" + "\n"
                      + "--boundary--";

        List<RiakObject> objects = ClientUtils.parseMultipart(mockRiakClient, "b", "k", headers, body);

        assertEquals(1, objects.get(0).getLinks().size());
        assertEquals(new RiakLink("b", "l", "t"), objects.get(0).getLinks().get(0));

        assertEquals(1, objects.get(0).getUsermeta().size());
        assertEquals("value", objects.get(0).getUsermeta().get("test"));

        assertEquals("text/plain", objects.get(0).getContentType());
        assertEquals("vclock", objects.get(0).getVclock());
        assertEquals("lastmod", objects.get(0).getLastmod());
        assertEquals("vtag", objects.get(0).getVtag());
    }

    @Test public void parse_multipart_returns_value() {
        Map<String, String> headers = new HashMap<String, String>();
        headers.put("content-type", "multipart/mixed; boundary=boundary");
        String body = "\n--boundary\n" + "\n" + "foo\n" + "--boundary--";

        List<RiakObject> objects = ClientUtils.parseMultipart(mockRiakClient, "b", "k", headers, body);
        assertEquals("foo", objects.get(0).getValue());
    }

    // Rely on Multipart tests to verify that multipart parsing actually works
    
    @Test public void join_handles_null_array() {
        assertNull(ClientUtils.join(null, null));
    }

    @Test public void join_handles_empty_array() {
        assertNull(ClientUtils.join(new String[0], null));
    }

    @Test public void join_handles_one_element_array() {
        assertEquals("x", ClientUtils.join(new String[] {"x"}, ","));
    }

    @Test public void join_handles_one_multielement_array() {
        assertEquals("x,y,z", ClientUtils.join(new String[] {"x", "y", "z"}, ","));
    }

    @Test public void join_handles_one_multicharacter_delimiter() {
        assertEquals("x//y//z", ClientUtils.join(new String[] {"x", "y", "z"}, "//"));
    }
    
    @Test(expected=IllegalArgumentException.class) public void encode_client_id_throws_on_null() {
        ClientUtils.encodeClientId(null);
    }

    @Test(expected=IllegalArgumentException.class) public void encode_client_id_throws_on_too_short_input() {
        ClientUtils.encodeClientId("id");
    }

    @Test public void encode_client_id_encodes_first_4_bytes() {
        String id = "clientid";
        String encoded = ClientUtils.encodeClientId(id);
        
        byte[] decoded = Base64.decodeBase64(encoded.getBytes());
        assertEquals(4, decoded.length);
        assertEquals(id.getBytes()[0], decoded[0]);
        assertEquals(id.getBytes()[1], decoded[1]);
        assertEquals(id.getBytes()[2], decoded[2]);
        assertEquals(id.getBytes()[3], decoded[3]);
    }

    @Test public void random_client_id_returns_4_encoded_bytes() {
        String encoded = ClientUtils.randomClientId();
        byte[] decoded = Base64.decodeBase64(encoded.getBytes());
        assertEquals(4, decoded.length);
    }

    @Test public void random_client_id_returns_different_ids() {
        String id1 = ClientUtils.randomClientId();
        String id2 = ClientUtils.randomClientId();
        assertNotNull(id1);
        assertNotNull(id2);
        assertFalse(id1.equals(id2));
    }

}
