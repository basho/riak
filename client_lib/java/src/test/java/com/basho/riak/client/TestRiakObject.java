package com.basho.riak.client;

import static org.junit.Assert.*;
import static org.mockito.Matchers.*;
import static org.mockito.Mockito.*;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.methods.EntityEnclosingMethod;
import org.apache.commons.httpclient.methods.RequestEntity;
import org.junit.Before;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import com.basho.riak.client.request.RequestMeta;
import com.basho.riak.client.response.FetchResponse;
import com.basho.riak.client.response.HttpResponse;
import com.basho.riak.client.response.StoreResponse;
import com.basho.riak.client.response.WalkResponse;
import com.basho.riak.client.util.Constants;

public class TestRiakObject {

    RiakObject impl;

    @Before public void setup() {
        impl = new RiakObject("b", "k");
    }

    @Test public void content_type_defaults_to_octet_stream() {
        assertEquals("application/octet-stream", impl.getContentType());
    }

    @Test public void links_never_null() {
        impl = new RiakObject("b", "k", null, null, null, null, null, null, null);
        assertNotNull(impl.getLinks());

        impl.setLinks((List<RiakLink>) null);
        assertNotNull(impl.getLinks());

        impl.copyData(new RiakObject(null, null));
        assertNotNull(impl.getLinks());
    }

    @Test public void usermeta_never_null() {
        impl = new RiakObject("b", "k", null, null, null, null, null, null, null);
        assertNotNull(impl.getUsermeta());

        impl.setUsermeta((Map<String, String>) null);
        assertNotNull(impl.getUsermeta());

        impl.copyData(new RiakObject(null, null));
        assertNotNull(impl.getUsermeta());
    }

    @Test public void copyData_does_deep_copy() {
        final String value = "value";
        final InputStream valueStream = mock(InputStream.class);
        final long valueStreamLength = 10;
        final String ctype = "ctype";
        final List<RiakLink> links = new ArrayList<RiakLink>();
        final Map<String, String> usermeta = new HashMap<String, String>();
        final String vclock = "vclock";
        final String lastmod = "lastmod";
        final String vtag = "vtag";
        final RiakLink link = new RiakLink("b", "l", "t");
        links.add(link);

        RiakObject copy = new RiakObject("b", "k2", value, ctype, links, usermeta, vclock, lastmod, vtag);
        copy.setValueStream(valueStream, valueStreamLength);
        impl.copyData(copy);

        assertEquals("b", impl.getBucket());
        assertEquals("k", impl.getKey());
        assertEquals(value, impl.getValue());
        assertSame(valueStream, impl.getValueStream());
        assertEquals(valueStreamLength, impl.getValueStreamLength().longValue());
        assertEquals(ctype, impl.getContentType());
        assertEquals(links, impl.getLinks());
        assertEquals(usermeta, impl.getUsermeta());
        assertEquals(vclock, impl.getVclock());
        assertEquals(lastmod, impl.getLastmod());
        assertEquals(vtag, impl.getVtag());

        assertNotSame(copy.getValueAsBytes(), impl.getValueAsBytes());
        assertNotSame(copy.getLinks(), impl.getLinks());
        assertNotSame(copy.getLinks().get(0), impl.getLinks().get(0));
        assertNotSame(copy.getUsermeta(), impl.getUsermeta());
    }

    @Test public void copyData_copies_null_data() {
        final String value = "value";
        final String ctype = "ctype";
        final List<RiakLink> links = new ArrayList<RiakLink>();
        final Map<String, String> usermeta = new HashMap<String, String>();
        final String vclock = "vclock";
        final String lastmod = "lastmod";
        final String vtag = "vtag";
        final RiakLink link = new RiakLink("b", "l", "t");
        links.add(link);

        impl = new RiakObject("b", "k", value, ctype, links, usermeta, vclock, lastmod, vtag);
        impl.copyData(new RiakObject(null, null));

        assertEquals("b", impl.getBucket());
        assertEquals("k", impl.getKey());
        assertNull(impl.getValue());
        assertEquals(0, impl.getLinks().size());
        assertEquals(0, impl.getUsermeta().size());
        assertNull(impl.getVclock());
        assertNull(impl.getLastmod());
        assertNull(impl.getVtag());
    }

    @Test public void updateMeta_nulls_out_meta_when_given_null_response() {
        final String vclock = "vclock";
        final String lastmod = "lastmod";
        final String vtag = "vtag";

        impl = new RiakObject("b", "k", null, null, null, null, vclock, lastmod, vtag);
        impl.updateMeta((StoreResponse) null);

        assertNull(impl.getVclock());
        assertNull(impl.getLastmod());
        assertNull(impl.getVtag());

        impl = new RiakObject("b", "k", null, null, null, null, vclock, lastmod, vtag);
        impl.updateMeta((FetchResponse) null);

        assertNull(impl.getVclock());
        assertNull(impl.getLastmod());
        assertNull(impl.getVtag());
    }

    @Test public void value_stream_is_separate_from_value() {
        final String value = "value";
        final byte[] isvalue = "isbytes".getBytes();
        final InputStream is = new ByteArrayInputStream(isvalue);

        impl.setValue(value);
        impl.setValueStream(is);

        assertEquals(value, impl.getValue());
        assertSame(is, impl.getValueStream());
    }

    @Test public void convenience_riak_client_methods_defer_to_riak_client() {
        RiakClient mockRiakClient = mock(RiakClient.class);
        RequestMeta mockRequestMeta = mock(RequestMeta.class);
        StoreResponse mockStoreResponse = mock(StoreResponse.class);
        FetchResponse mockFetchResponse = mock(FetchResponse.class);
        HttpResponse mockHttpResponse = mock(HttpResponse.class);
        WalkResponse mockWalkResponse = mock(WalkResponse.class);
        
        when(mockRiakClient.store(any(RiakObject.class), any(RequestMeta.class))).thenReturn(mockStoreResponse);
        impl = new RiakObject(mockRiakClient, "b", "k");
        StoreResponse sr1 = impl.store();
        StoreResponse sr2 = impl.store(mockRequestMeta);
        
        verify(mockRiakClient).store(impl, null);        
        verify(mockRiakClient).store(impl, mockRequestMeta);
        assertSame(mockStoreResponse, sr1);
        assertSame(mockStoreResponse, sr2);

        when(mockRiakClient.fetchMeta(anyString(), anyString(), any(RequestMeta.class))).thenReturn(mockFetchResponse);
        FetchResponse fr1 = impl.fetchMeta();
        FetchResponse fr2 = impl.fetchMeta(mockRequestMeta);
        
        verify(mockRiakClient).fetchMeta("b", "k", null);        
        verify(mockRiakClient).fetchMeta("b", "k", mockRequestMeta);
        assertSame(mockFetchResponse, fr1);
        assertSame(mockFetchResponse, fr2);

        when(mockRiakClient.fetch(anyString(), anyString(), any(RequestMeta.class))).thenReturn(mockFetchResponse);
        fr1 = impl.fetch();
        fr2 = impl.fetch(mockRequestMeta);
        
        verify(mockRiakClient).fetch("b", "k", null);        
        verify(mockRiakClient).fetch("b", "k", mockRequestMeta);        
        assertSame(mockFetchResponse, fr1);
        assertSame(mockFetchResponse, fr2);

        when(mockRiakClient.delete(anyString(), anyString(), any(RequestMeta.class))).thenReturn(mockHttpResponse);
        HttpResponse hr1 = impl.delete();
        HttpResponse hr2 = impl.delete(mockRequestMeta);
        
        verify(mockRiakClient).delete("b", "k", null);        
        verify(mockRiakClient).delete("b", "k", mockRequestMeta);
        assertSame(mockHttpResponse, hr1);
        assertSame(mockHttpResponse, hr2);

        when(mockRiakClient.walk(anyString(), anyString(), anyString(), any(RequestMeta.class))).thenReturn(mockWalkResponse);
        WalkResponse wr1 = impl.walk().run();
        WalkResponse wr2 = impl.walk().run(mockRequestMeta);
        
        verify(mockRiakClient).walk("b", "k", "_,_,_/", null);        
        verify(mockRiakClient).walk("b", "k", "_,_,_/", mockRequestMeta);
        assertSame(mockWalkResponse, wr1);
        assertSame(mockWalkResponse, wr2);
    }
    
    @Test public void store_updates_meta_on_success() {
        RiakClient mockRiakClient = mock(RiakClient.class);
        StoreResponse mockStoreResponse = mock(StoreResponse.class);
        
        when(mockRiakClient.store(any(RiakObject.class), any(RequestMeta.class))).thenReturn(mockStoreResponse);
        when(mockStoreResponse.isSuccess()).thenReturn(true);
        impl = spy(new RiakObject(mockRiakClient, "b", "k"));
        impl.store();
        
        verify(impl).updateMeta(mockStoreResponse);
    }

    @Test public void store_does_not_modify_meta_on_failure() {
        RiakClient mockRiakClient = mock(RiakClient.class);
        StoreResponse mockStoreResponse = mock(StoreResponse.class);
        
        when(mockRiakClient.store(any(RiakObject.class), any(RequestMeta.class))).thenReturn(mockStoreResponse);
        when(mockStoreResponse.isSuccess()).thenReturn(false);
        impl = spy(new RiakObject(mockRiakClient, "b", "k"));
        impl.store();
        
        verify(impl, never()).updateMeta(any(StoreResponse.class));
        verify(impl, never()).copyData(any(RiakObject.class));
    }

    @Test public void fetchMeta_updates_meta_on_success() {
        RiakClient mockRiakClient = mock(RiakClient.class);
        FetchResponse mockFetchResponse = mock(FetchResponse.class);
        
        when(mockRiakClient.fetchMeta(anyString(), anyString(), any(RequestMeta.class))).thenReturn(mockFetchResponse);
        when(mockFetchResponse.isSuccess()).thenReturn(true);
        impl = spy(new RiakObject(mockRiakClient, "b", "k"));
        impl.fetchMeta();
        
        verify(impl).updateMeta(mockFetchResponse);
    }

    @Test public void fetchMeta_does_not_modify_meta_on_failure() {
        RiakClient mockRiakClient = mock(RiakClient.class);
        FetchResponse mockFetchResponse = mock(FetchResponse.class);
        
        when(mockRiakClient.fetchMeta(anyString(), anyString(), any(RequestMeta.class))).thenReturn(mockFetchResponse);
        when(mockFetchResponse.isSuccess()).thenReturn(false);
        impl = spy(new RiakObject(mockRiakClient, "b", "k"));
        impl.fetchMeta();
        
        verify(impl, never()).updateMeta(any(FetchResponse.class));
        verify(impl, never()).copyData(any(RiakObject.class));
    }

    @Test public void fetch_shallow_copies_object_on_success() {
        RiakClient mockRiakClient = mock(RiakClient.class);
        FetchResponse mockFetchResponse = mock(FetchResponse.class);
        RiakObject mockRiakObject = mock(RiakObject.class);
        when(mockRiakClient.fetch(anyString(), anyString(), any(RequestMeta.class))).thenReturn(mockFetchResponse);
        when(mockFetchResponse.isSuccess()).thenReturn(true);
        when(mockFetchResponse.getObject()).thenReturn(mockRiakObject);
        impl = spy(new RiakObject(mockRiakClient, "b", "k"));
        impl.fetch();
        
        verify(impl).shallowCopy(mockRiakObject);
        verify(mockFetchResponse).setObject(impl);
    }

    @Test public void fetch_does_not_update_data_on_failure() {
        RiakClient mockRiakClient = mock(RiakClient.class);
        FetchResponse mockFetchResponse = mock(FetchResponse.class);
        
        when(mockRiakClient.fetch(anyString(), anyString(), any(RequestMeta.class))).thenReturn(mockFetchResponse);
        when(mockFetchResponse.isSuccess()).thenReturn(false);
        when(mockFetchResponse.getObject()).thenReturn(null);
        impl = spy(new RiakObject(mockRiakClient, "b", "k"));
        impl.fetch();
        
        verify(impl, never()).updateMeta(any(FetchResponse.class));
        verify(impl, never()).shallowCopy(any(RiakObject.class));
        verify(impl, never()).copyData(any(RiakObject.class));
    }
    
    @Test public void walk_returns_one_step_correctly() {
        RiakClient mockRiakClient = mock(RiakClient.class);
        
        impl = new RiakObject(mockRiakClient, "b", "k");

        impl.walk().run();
        verify(mockRiakClient).walk("b", "k", "_,_,_/", null);

        impl.walk(false).run();
        verify(mockRiakClient).walk("b", "k", "_,_,0/", null);

        impl.walk("bucket").run();
        verify(mockRiakClient).walk("b", "k", "bucket,_,_/", null);

        impl.walk("bucket", false).run();
        verify(mockRiakClient).walk("b", "k", "bucket,_,0/", null);

        impl.walk("bucket", "tag").run();
        verify(mockRiakClient).walk("b", "k", "bucket,tag,_/", null);

        impl.walk("bucket", "tag", false).run();
        verify(mockRiakClient).walk("b", "k", "bucket,tag,0/", null);
    }

    @Test public void walk_returns_multiple_step_correctly() {
        RiakClient mockRiakClient = mock(RiakClient.class);
        impl = new RiakObject(mockRiakClient, "b", "k");

        impl.walk().walk("bucket").walk("bucket", "tag", false).run();
        
        verify(mockRiakClient).walk("b", "k", "_,_,_/bucket,_,_/bucket,tag,0/", null);    
    }

    // The following could be combined as "convenience_riak_client_methods_throw_if_no_associated_riak_client"
    @Test(expected=IllegalStateException.class) public void store_throws_if_no_associated_riak_client() {
        impl.store();
    }
    @Test(expected=IllegalStateException.class) public void fetchMeta_throws_if_no_associated_riak_client() {
        impl.fetchMeta();
    }
    @Test(expected=IllegalStateException.class) public void fetch_throws_if_no_associated_riak_client() {
        impl.fetch();
    }
    @Test(expected=IllegalStateException.class) public void delete_throws_if_no_associated_riak_client() {
        impl.delete();
    }
    @Test(expected=IllegalStateException.class) public void walk_throws_if_no_associated_riak_client() {
        impl.walk().run();
    }

    @SuppressWarnings("unchecked") @Test public void write_to_http_method_gives_value_stream_priority_over_value() {
        final String value = "value";
        final byte[] isvalue = "isbytes".getBytes();
        final InputStream is = new ByteArrayInputStream(isvalue);
        final ByteArrayOutputStream os = new ByteArrayOutputStream();
        final EntityEnclosingMethod mockHttpMethod = mock(EntityEnclosingMethod.class);

        when(mockHttpMethod.getPath()).thenReturn("/path/to/object");
        doAnswer(new Answer() {
            public Object answer(InvocationOnMock invocation) throws Throwable {
                ((RequestEntity) invocation.getArguments()[0]).writeRequest(os);
                return null;
            }
        }).when(mockHttpMethod).setRequestEntity(any(RequestEntity.class));

        impl.setValue(value);
        impl.setValueStream(is);
        impl.writeToHttpMethod(mockHttpMethod);

        assertArrayEquals(os.toByteArray(), isvalue);
    }

    @Test public void write_to_http_method_always_sets_entity_even_if_value_is_null() {
        final EntityEnclosingMethod mockHttpMethod = mock(EntityEnclosingMethod.class);

        impl.setValue((String) null);
        impl.setValueStream(null);
        impl.writeToHttpMethod(mockHttpMethod);

        verify(mockHttpMethod).setRequestEntity((RequestEntity) notNull());
    }

    @Test public void write_to_http_method_sets_link_header() {
        final RiakLink link = new RiakLink("b", "l", "t");
        final EntityEnclosingMethod mockHttpMethod = mock(EntityEnclosingMethod.class);

        when(mockHttpMethod.getPath()).thenReturn("/riak/b/k");

        impl.getLinks().add(link);
        impl.writeToHttpMethod(mockHttpMethod);

        verify(mockHttpMethod).setRequestHeader(eq(Constants.HDR_LINK), contains("</riak/b/l>; riaktag=\"t\""));
    }

    @Test public void write_to_http_method_doesnt_sets_link_header_if_no_links() {
        final EntityEnclosingMethod mockHttpMethod = mock(EntityEnclosingMethod.class);

        impl.writeToHttpMethod(mockHttpMethod);

        verify(mockHttpMethod, never()).setRequestHeader(eq(Constants.HDR_LINK), anyString());
    }

    @Test public void write_to_http_method_sets_user_meta_headers() {
        final EntityEnclosingMethod mockHttpMethod = mock(EntityEnclosingMethod.class);

        impl.getUsermeta().put("k", "v");
        impl.writeToHttpMethod(mockHttpMethod);

        verify(mockHttpMethod).setRequestHeader(Constants.HDR_USERMETA_REQ_PREFIX + "k", "v");
    }

    @Test public void write_to_http_method_doesnt_sets_user_meta_headers_if_no_usermeta() {
        final EntityEnclosingMethod mockHttpMethod = mock(EntityEnclosingMethod.class);

        impl.writeToHttpMethod(mockHttpMethod);

        verify(mockHttpMethod, never()).setRequestHeader(contains(Constants.HDR_USERMETA_REQ_PREFIX), anyString());
    }

    @Test public void write_to_http_method_sets_vclock() {
        final String vclock = "vclock";
        final EntityEnclosingMethod mockHttpMethod = mock(EntityEnclosingMethod.class);

        impl = new RiakObject("b", "k", null, null, null, null, vclock, null, null);
        impl.writeToHttpMethod(mockHttpMethod);

        verify(mockHttpMethod).setRequestHeader(Constants.HDR_VCLOCK, vclock);
    }

    @Test public void get_base_path_finds_empty_base_path() {
        final EntityEnclosingMethod mockHttpMethod = mock(EntityEnclosingMethod.class);

        when(mockHttpMethod.getPath()).thenReturn(null);
        assertEquals("", impl.getBasePathFromHttpMethod(mockHttpMethod));

        when(mockHttpMethod.getPath()).thenReturn("");
        assertEquals("", impl.getBasePathFromHttpMethod(mockHttpMethod));

        when(mockHttpMethod.getPath()).thenReturn("/");
        assertEquals("", impl.getBasePathFromHttpMethod(mockHttpMethod));

        when(mockHttpMethod.getPath()).thenReturn("/b");
        assertEquals("", impl.getBasePathFromHttpMethod(mockHttpMethod));

        when(mockHttpMethod.getPath()).thenReturn("/b/k");
        assertEquals("", impl.getBasePathFromHttpMethod(mockHttpMethod));
    }

    @Test public void get_base_path_finds_one_element_base_path() {
        final EntityEnclosingMethod mockHttpMethod = mock(EntityEnclosingMethod.class);

        when(mockHttpMethod.getPath()).thenReturn("/riak/b/k");
        assertEquals("/riak", impl.getBasePathFromHttpMethod(mockHttpMethod));
    }

    @Test public void get_base_path_finds_multiple_element_base_path() {
        final EntityEnclosingMethod mockHttpMethod = mock(EntityEnclosingMethod.class);

        when(mockHttpMethod.getPath()).thenReturn("/path/to/riak/b/k");
        assertEquals("/path/to/riak", impl.getBasePathFromHttpMethod(mockHttpMethod));
    }

    @Test public void get_base_path_handles_trailing_slash() {
        final EntityEnclosingMethod mockHttpMethod = mock(EntityEnclosingMethod.class);

        when(mockHttpMethod.getPath()).thenReturn("/riak/b/k/");
        assertEquals("/riak", impl.getBasePathFromHttpMethod(mockHttpMethod));
    }
}
