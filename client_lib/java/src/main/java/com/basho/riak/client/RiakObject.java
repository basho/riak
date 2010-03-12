/*
 * This file is provided to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */
package com.basho.riak.client;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpMethod;
import org.apache.commons.httpclient.methods.ByteArrayRequestEntity;
import org.apache.commons.httpclient.methods.EntityEnclosingMethod;
import org.apache.commons.httpclient.methods.InputStreamRequestEntity;
import org.apache.commons.httpclient.util.DateParseException;
import org.apache.commons.httpclient.util.DateUtil;

import com.basho.riak.client.request.RequestMeta;
import com.basho.riak.client.request.RiakWalkSpec;
import com.basho.riak.client.response.FetchResponse;
import com.basho.riak.client.response.HttpResponse;
import com.basho.riak.client.response.RiakIORuntimeException;
import com.basho.riak.client.response.RiakResponseRuntimeException;
import com.basho.riak.client.response.StoreResponse;
import com.basho.riak.client.response.WalkResponse;
import com.basho.riak.client.util.Constants;

/**
 * A Riak object.
 */
public class RiakObject {

    private RiakClient riak;
    private String bucket;
    private String key;
    private byte[] value;
    private List<RiakLink> links;
    private Map<String, String> usermeta;
    private String contentType;
    private String vclock;
    private String lastmod;
    private String vtag;
    private InputStream valueStream;
    private Long valueStreamLength;

    /**
     * Create an empty object. The content type defaults to
     * application/octet-stream.
     * 
     * @param riak
     *            Riak instance this object is associated with, which is used by
     *            the convenience methods in this class (e.g.
     *            {@link RiakObject#store()}).
     * @param bucket
     *            The object's bucket
     * @param key
     *            The object's key
     * @param value
     *            The object's value
     * @param contentType
     *            The object's content type which defaults to
     *            application/octet-stream if null.
     * @param links
     *            Links to other objects
     * @param usermeta
     *            Custom metadata key-value pairs for this object
     * @param vclock
     *            An opaque vclock assigned by Riak
     * @param lastmod
     *            The last time this object was modified according to Riak
     * @param vtag
     *            This object's entity tag assigned by Riak
     */
    public RiakObject(RiakClient riak, String bucket, String key, String value, String contentType,
            List<RiakLink> links, Map<String, String> usermeta, String vclock, String lastmod, String vtag) {
        this.riak = riak;
        this.bucket = bucket;
        this.key = key;
        this.vclock = vclock;
        this.lastmod = lastmod;
        this.vtag = vtag;

        setValue(value);
        setContentType(contentType);
        setLinks(links);
        setUsermeta(usermeta);
    }

    public RiakObject(RiakClient riak, String bucket, String key) {
        this(riak, bucket, key, null, null, null, null, null, null, null);
    }

    public RiakObject(RiakClient riak, String bucket, String key, String value) {
        this(riak, bucket, key, value, null, null, null, null, null, null);
    }

    public RiakObject(RiakClient riak, String bucket, String key, String value, String contentType) {
        this(riak, bucket, key, value, contentType, null, null, null, null, null);
    }

    public RiakObject(RiakClient riak, String bucket, String key, String value, String contentType, List<RiakLink> links) {
        this(riak, bucket, key, value, contentType, links, null, null, null, null);
    }

    public RiakObject(RiakClient riak, String bucket, String key, String value, String contentType,
            List<RiakLink> links, Map<String, String> usermeta) {
        this(riak, bucket, key, value, contentType, links, usermeta, null, null, null);
    }

    public RiakObject(String bucket, String key) {
        this(null, bucket, key, null, null, null, null, null, null, null);
    }

    public RiakObject(String bucket, String key, String value) {
        this(null, bucket, key, value, null, null, null, null, null, null);
    }

    public RiakObject(String bucket, String key, String value, String contentType) {
        this(null, bucket, key, value, contentType, null, null, null, null, null);
    }

    public RiakObject(String bucket, String key, String value, String contentType, List<RiakLink> links) {
        this(null, bucket, key, value, contentType, links, null, null, null, null);
    }

    public RiakObject(String bucket, String key, String value, String contentType, List<RiakLink> links,
            Map<String, String> usermeta) {
        this(null, bucket, key, value, contentType, links, usermeta, null, null, null);
    }

    public RiakObject(String bucket, String key, String value, String contentType, List<RiakLink> links,
            Map<String, String> usermeta, String vclock, String lastmod, String vtag) {
        this(null, bucket, key, value, contentType, links, usermeta, vclock, lastmod, vtag);
    }

    /**
     * A {@link RiakObject} can be loosely attached to the {@link RiakClient}
     * from which retrieve it was retrieved. Calling convenience methods like
     * {@link RiakObject#store()} will store this object use that client.
     */
    public RiakClient getRiakClient() {
        return riak;
    }

    public RiakObject setRiakClient(RiakClient client) {
        riak = client;
        return this;
    }

    /**
     * Copy the metadata and value from <code>object</code>. The bucket and key
     * are not copied.
     * 
     * @param object
     *            The source object to copy from
     */
    public void copyData(RiakObject object) {
        if (object == null)
            return;

        if (object.value != null) {
            value = Arrays.copyOf(object.value, object.value.length);
        } else {
            value = null;
        }

        valueStream = object.valueStream;
        valueStreamLength = object.valueStreamLength;

        links = new ArrayList<RiakLink>();
        if (object.links != null) {
            for (RiakLink link : object.links) {
                links.add(new RiakLink(link));
            }
        }
        usermeta = new HashMap<String, String>();
        if (object.usermeta != null) {
            usermeta.putAll(object.usermeta);
        }
        contentType = object.contentType;
        vclock = object.vclock;
        lastmod = object.lastmod;
        vtag = object.vtag;
    }

    /**
     * Perform a shallow copy of the object
     */
    void shallowCopy(RiakObject object) {
        value = object.value;
        links = object.links;
        usermeta = object.usermeta;
        contentType = object.contentType;
        vclock = object.vclock;
        lastmod = object.lastmod;
        vtag = object.vtag;
        valueStream = object.valueStream;
        valueStreamLength = object.valueStreamLength;
    }

    /**
     * Update the object's metadata. This usually happens when Riak returns
     * updated metadata from a store operation.
     * 
     * @param response
     *            Response from a store operation containing an updated vclock,
     *            last modified date, and vtag
     */
    public void updateMeta(StoreResponse response) {
        if (response == null) {
            vclock = null;
            lastmod = null;
            vtag = null;
        } else {
            vclock = response.getVclock();
            lastmod = response.getLastmod();
            vtag = response.getVtag();
        }
    }

    /**
     * Update the object's metadata from a fetch or fetchMeta operation
     * 
     * @param response
     *            Response from a fetch or fetchMeta operation containing a
     *            vclock, last modified date, and vtag
     */
    public void updateMeta(FetchResponse response) {
        if (response == null || response.getObject() == null) {
            vclock = null;
            lastmod = null;
            vtag = null;
        } else {
            vclock = response.getObject().getVclock();
            lastmod = response.getObject().getLastmod();
            vtag = response.getObject().getVtag();
        }
    }

    /**
     * The object's bucket
     */
    public String getBucket() {
        return bucket;
    }

    /**
     * The object's key
     */
    public String getKey() {
        return key;
    }

    /**
     * The object's value
     */
    public String getValue() {
        return (value == null ? null : new String(value));
    }

    public byte[] getValueAsBytes() {
        return value;
    }

    public void setValue(String value) {
        if (value != null) {
            this.value = value.getBytes();
        } else {
            this.value = null;
        }
    }

    public void setValue(byte[] value) {
        this.value = value;
    }

    /**
     * Set the object's value as a stream. A value set here is independent of
     * and has precedent over any value set using setValue():
     * {@link RiakObject#writeToHttpMethod(HttpMethod)} will always write the
     * value from getValueStream() if it is not null. Calling getValue() will
     * always return values set via setValue(), and calling getValueStream()
     * will always return the stream set via setValueStream.
     * 
     * @param in
     *            Input stream representing the object's value
     * @param len
     *            Length of the InputStream or null if unknown. If null, the
     *            value will be buffered in memory to determine its size before
     *            sending to the server.
     */
    public void setValueStream(InputStream in, Long len) {
        valueStream = in;
        valueStreamLength = len;
    }

    public void setValueStream(InputStream in) {
        valueStream = in;
    }

    public InputStream getValueStream() {
        return valueStream;
    }

    public void setValueStreamLength(Long len) {
        valueStreamLength = len;
    }

    public Long getValueStreamLength() {
        return valueStreamLength;
    }

    /**
     * The object's links -- may be empty, but never be null. New links can be
     * added using addLink() or getLinks().add()
     */
    public List<RiakLink> getLinks() {
        return links;
    }

    public void setLinks(List<RiakLink> links) {
        if (links == null) {
            links = new ArrayList<RiakLink>();
        }
        this.links = links;
    }

    public RiakObject addLink(RiakLink link) {
        if (link != null) {
            links.add(link);
        }
        return this;
    }

    /**
     * User-specified metadata for the object in the form of key-value pairs --
     * may be empty, but never be null. New key-value pairs can be added using
     * addUsermeta() or getUsermeta().put()
     */
    public Map<String, String> getUsermeta() {
        return usermeta;
    }

    public void setUsermeta(Map<String, String> usermeta) {
        if (usermeta == null) {
            usermeta = new HashMap<String, String>();
        }
        this.usermeta = usermeta;
    }

    public RiakObject addUsermeta(String key, String value) {
        usermeta.put(key, value);
        return this;
    }

    /**
     * The object's content type as a MIME type
     */
    public String getContentType() {
        return contentType;
    }

    public void setContentType(String contentType) {
        if (contentType != null) {
            this.contentType = contentType;
        } else {
            this.contentType = Constants.CTYPE_OCTET_STREAM;
        }
    }

    /**
     * The object's opaque vclock assigned by Riak
     */
    public String getVclock() {
        return vclock;
    }

    /**
     * The modification date of the object determined by Riak
     */
    public String getLastmod() {
        return lastmod;
    }

    /**
     * Convenience method to get the last modified header parsed into a Date
     * object. Returns null if header is null, malformed, or cannot be parsed.
     */
    public Date getLastmodAsDate() {
        try {
            return DateUtil.parseDate(lastmod);
        } catch (DateParseException e) {
            return null;
        }
    }

    /**
     * An entity tag for the object assigned by Riak
     */
    public String getVtag() {
        return vtag;
    }

    /**
     * Convenience method for calling
     * {@link RiakClient#store(RiakObject, RequestMeta)} followed by
     * {@link RiakObject#updateMeta(StoreResponse)}
     * 
     * @throws IllegalStateException
     *             if this object was not fetched from a Riak instance, so there
     *             is not associated server to store it with.
     */
    public StoreResponse store(RequestMeta meta) {
        return store(riak, meta);
    }

    public StoreResponse store() {
        return store(riak, null);
    }

    /**
     * Store this object to a different Riak instance.
     * 
     * @param riak
     *            Riak instance to store this object to
     * @param meta
     *            Same as {@link RiakClient#store(RiakObject, RequestMeta)}
     * @throws IllegalStateException
     *             if this object was not fetched from a Riak instance, so there
     *             is not associated server to store it with.
     */
    public StoreResponse store(RiakClient riak, RequestMeta meta) {
        if (riak == null)
            throw new IllegalStateException("Cannot store an object without a RiakClient");

        StoreResponse r = riak.store(this, meta);
        if (r.isSuccess()) {
            this.updateMeta(r);
        }
        return r;
    }

    /**
     * Convenience method for calling {@link RiakClient#fetch(String, String)}
     * followed by {@link RiakObject#copyData(RiakObject)}
     * 
     * @param meta
     *            Same as {@link RiakClient#fetch(String, String, RequestMeta)}
     * @throws IllegalStateException
     *             if this object was not fetched from a Riak instance, so there
     *             is not associated server to refetch it from.
     */
    public FetchResponse fetch(RequestMeta meta) {
        if (riak == null)
            throw new IllegalStateException("Cannot fetch an object without a RiakClient");

        FetchResponse r = riak.fetch(bucket, key, meta);
        if (r.getObject() != null) {
            RiakObject other = r.getObject();
            shallowCopy(other);
            r.setObject(this);
        }
        return r;
    }

    public FetchResponse fetch() {
        return fetch(null);
    }

    /**
     * Convenience method for calling
     * {@link RiakClient#fetchMeta(String, String, RequestMeta)} followed by
     * {@link RiakObject#updateMeta(FetchResponse)}
     * 
     * @throws IllegalStateException
     *             if this object was not fetched from a Riak instance, so there
     *             is not associated server to refetch meta from.
     */
    public FetchResponse fetchMeta(RequestMeta meta) {
        if (riak == null)
            throw new IllegalStateException("Cannot fetch meta for an object without a RiakClient");

        FetchResponse r = riak.fetchMeta(bucket, key, meta);
        if (r.isSuccess()) {
            this.updateMeta(r);
        }
        return r;
    }

    public FetchResponse fetchMeta() {
        return fetchMeta(null);
    }

    /**
     * Convenience method for calling
     * {@link RiakClient#delete(String, String, RequestMeta)}.
     * 
     * @throws IllegalStateException
     *             if this object was not fetched from a Riak instance, so there
     *             is not associated server to delete from.
     */
    public HttpResponse delete(RequestMeta meta) {
        if (riak == null)
            throw new IllegalStateException("Cannot delete an object without a RiakClient");

        return riak.delete(bucket, key, meta);
    }

    public HttpResponse delete() {
        return delete(null);
    }

    /**
     * Convenience methods for building a link walk specification starting from
     * this object and calling
     * {@link RiakClient#walk(String, String, RiakWalkSpec)}
     * 
     * @param bucket
     *            The bucket to follow object links to
     * @param tag
     *            The link tags to follow from this object
     * @param keep
     *            Whether to keep the output from this link walking step. If not
     *            specified, then the output is only kept from the last step.
     * @return A {@link LinkBuilder} object to continue building the walk query
     *         or to run it.
     */
    public LinkBuilder walk(String bucket, String tag, boolean keep) {
        return new LinkBuilder().walk(bucket, tag, keep);
    }

    public LinkBuilder walk(String bucket, String tag) {
        return new LinkBuilder().walk(bucket, tag);
    }

    public LinkBuilder walk(String bucket, boolean keep) {
        return new LinkBuilder().walk(bucket, keep);
    }

    public LinkBuilder walk(String bucket) {
        return new LinkBuilder().walk(bucket);
    }

    public LinkBuilder walk() {
        return new LinkBuilder().walk();
    }

    public LinkBuilder walk(boolean keep) {
        return new LinkBuilder().walk(keep);
    }

    /**
     * Serializes this object to an existing {@link HttpMethod} which can be
     * sent as an HTTP request. Specifically, sends the object's link,
     * user-defined metadata and vclock as HTTP headers and the value as the
     * body. Used by {@link RiakClient} to create PUT requests.
     */
    public void writeToHttpMethod(HttpMethod httpMethod) {
        // Serialize headers
        String basePath = getBasePathFromHttpMethod(httpMethod);
        StringBuilder linkHeader = new StringBuilder();
        for (RiakLink link : links) {
            if (linkHeader.length() > 0) {
                linkHeader.append(", ");
            }
            linkHeader.append("<");
            linkHeader.append(basePath);
            linkHeader.append("/");
            linkHeader.append(link.getBucket());
            linkHeader.append("/");
            linkHeader.append(link.getKey());
            linkHeader.append(">; ");
            linkHeader.append(Constants.LINK_TAG);
            linkHeader.append("=\"");
            linkHeader.append(link.getTag());
            linkHeader.append("\"");
        }
        if (linkHeader.length() > 0) {
            httpMethod.setRequestHeader(Constants.HDR_LINK, linkHeader.toString());
        }
        for (String name : usermeta.keySet()) {
            httpMethod.setRequestHeader(Constants.HDR_USERMETA_REQ_PREFIX + name, usermeta.get(name));
        }
        if (vclock != null) {
            httpMethod.setRequestHeader(Constants.HDR_VCLOCK, vclock);
        }

        // Serialize body
        if (httpMethod instanceof EntityEnclosingMethod) {
            EntityEnclosingMethod entityEnclosingMethod = (EntityEnclosingMethod) httpMethod;

            // Any value set using setValueAsStream() has precedent over value
            // set using setValue()
            if (valueStream != null) {
                if (valueStreamLength != null && valueStreamLength >= 0) {
                    entityEnclosingMethod.setRequestEntity(new InputStreamRequestEntity(valueStream, valueStreamLength,
                                                                                        contentType));
                } else {
                    entityEnclosingMethod.setRequestEntity(new InputStreamRequestEntity(valueStream, contentType));
                }
            } else if (value != null) {
                entityEnclosingMethod.setRequestEntity(new ByteArrayRequestEntity(value, contentType));
            } else {
                entityEnclosingMethod.setRequestEntity(new ByteArrayRequestEntity("".getBytes(), contentType));
            }
        }
    }

    String getBasePathFromHttpMethod(HttpMethod httpMethod) {
        if (httpMethod == null || httpMethod.getPath() == null)
            return "";

        String path = httpMethod.getPath();
        int idx = path.length() - 1;

        // ignore any trailing slash
        if (path.endsWith("/")) {
            idx--;
        }

        // trim off last two path components
        idx = path.lastIndexOf('/', idx);
        idx = path.lastIndexOf('/', idx - 1);

        if (idx <= 0)
            return "";

        return path.substring(0, idx);
    }

    /**
     * Created by links() as a convenient way to build up link walking queries
     */
    public class LinkBuilder {

        private RiakWalkSpec walkSpec = new RiakWalkSpec();

        public LinkBuilder walk() {
            walkSpec.addStep(RiakWalkSpec.WILDCARD, RiakWalkSpec.WILDCARD);
            return this;
        }

        public LinkBuilder walk(boolean keep) {
            walkSpec.addStep(RiakWalkSpec.WILDCARD, RiakWalkSpec.WILDCARD, keep);
            return this;
        }

        public LinkBuilder walk(String bucket) {
            walkSpec.addStep(bucket, RiakWalkSpec.WILDCARD);
            return this;
        }

        public LinkBuilder walk(String bucket, boolean keep) {
            walkSpec.addStep(bucket, RiakWalkSpec.WILDCARD, keep);
            return this;
        }

        public LinkBuilder walk(String bucket, String tag) {
            walkSpec.addStep(bucket, tag);
            return this;
        }

        public LinkBuilder walk(String bucket, String tag, boolean keep) {
            walkSpec.addStep(bucket, tag, keep);
            return this;
        }

        public String getWalkSpec() {
            return walkSpec.toString();
        }

        /**
         * Execute the link walking query by calling
         * {@link RiakClient#walk(String, String, String, RequestMeta)}.
         * 
         * @param meta
         *            Extra metadata to attach to the request such as HTTP
         *            headers or query parameters.
         * @return See
         *         {@link RiakClient#walk(String, String, String, RequestMeta)}.
         * 
         * @throws RiakIORuntimeException
         *             If an error occurs during communication with the Riak
         *             server.
         * @throws RiakResponseRuntimeException
         *             If the Riak server returns a malformed response.
         */
        public WalkResponse run(RequestMeta meta) {
            if (riak == null)
                throw new IllegalStateException("Cannot perform object link walk without a RiakClient");
            return riak.walk(bucket, key, getWalkSpec(), meta);
        }

        public WalkResponse run() {
            return run(null);
        }
    }
}