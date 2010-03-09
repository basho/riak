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

import java.io.IOException;
import java.util.Map;
import java.util.Set;

import org.apache.commons.httpclient.HttpClient;
import org.json.JSONException;
import org.json.JSONObject;

import com.basho.riak.client.request.MapReduceBuilder;
import com.basho.riak.client.request.RequestMeta;
import com.basho.riak.client.request.RiakWalkSpec;
import com.basho.riak.client.response.BucketResponse;
import com.basho.riak.client.response.FetchResponse;
import com.basho.riak.client.response.HttpResponse;
import com.basho.riak.client.response.MapReduceResponse;
import com.basho.riak.client.response.RiakExceptionHandler;
import com.basho.riak.client.response.RiakIORuntimeException;
import com.basho.riak.client.response.RiakResponseRuntimeException;
import com.basho.riak.client.response.StoreResponse;
import com.basho.riak.client.response.StreamHandler;
import com.basho.riak.client.response.WalkResponse;
import com.basho.riak.client.util.ClientHelper;
import com.basho.riak.client.util.ClientUtils;
import com.basho.riak.client.util.Constants;

/**
 * Primary interface for interacting with Riak via HTTP.
 */
public class RiakClient {

    private ClientHelper helper;

    public RiakConfig getConfig() {
        return helper.getConfig();
    }

    public RiakClient(RiakConfig config) {
        this(config, null);
    }

    public RiakClient(RiakConfig config, String clientId) {
        helper = new ClientHelper(config, clientId);
    }

    public RiakClient(String url) {
        this(new RiakConfig(url), null);
    }

    public RiakClient(String url, String clientId) {
        this(new RiakConfig(url), clientId);
    }

    // Package protected constructor used for testing
    RiakClient(ClientHelper helper) {
        this.helper = helper;
    }

    /**
     * Set the properties for a Riak bucket.
     * 
     * @param bucket
     *            The bucket name.
     * @param bucketInfo
     *            Contains the schema to use for the bucket. Refer to the Riak
     *            documentation for a list of the recognized properties and the
     *            format of their values.
     * @param meta
     *            Extra metadata to attach to the request such as HTTP headers
     *            and query parameters.
     * 
     * @return {@link HttpResponse} containing HTTP response information.
     * 
     * @throws IllegalArgumentException
     *             If the provided schema values cannot be serialized to send to
     *             Riak.
     * @throws RiakIORuntimeException
     *             If an error occurs during communication with the Riak server.
     */
    public HttpResponse setBucketSchema(String bucket, RiakBucketInfo bucketInfo, RequestMeta meta) {
        JSONObject schema = null;
        try {
            schema = new JSONObject().put(Constants.FL_SCHEMA, bucketInfo.getSchema());
        } catch (JSONException unreached) {
            throw new IllegalStateException("wrapping valid json should be valid", unreached);
        }

        return helper.setBucketSchema(bucket, schema, meta);
    }

    public HttpResponse setBucketSchema(String bucket, RiakBucketInfo bucketInfo) {
        return setBucketSchema(bucket, bucketInfo, null);
    }

    /**
     * Return the properties and keys for a Riak bucket.
     * 
     * @param bucket
     *            The bucket to list.
     * @param meta
     *            Extra metadata to attach to the request such as HTTP headers
     *            and query parameters.
     * 
     * @return {@link BucketResponse} containing HTTP response information and
     *         the parsed schema and keys
     * 
     * @throws RiakIORuntimeException
     *             If an error occurs during communication with the Riak server.
     * @throws RiakResponseRuntimeException
     *             If the Riak server returns a malformed response.
     */
    public BucketResponse listBucket(String bucket, RequestMeta meta) {
        return listBucket(bucket, meta, false);
    }

    public BucketResponse listBucket(String bucket) {
        return listBucket(bucket, null);
    }

    /**
     * Same as {@link RiakClient#listBucket(String, RequestMeta)}, except
     * streams the response, so the user must remember to call
     * {@link BucketResponse#close() on the return value.
     */
    public BucketResponse streamBucket(String bucket, RequestMeta meta) {
        return listBucket(bucket, meta, true);
    }

    public BucketResponse streamBucket(String bucket) {
        return streamBucket(bucket, null);
    }

    BucketResponse listBucket(String bucket, RequestMeta meta, boolean streamResponse) {
        HttpResponse r = helper.listBucket(bucket, meta, streamResponse);
        try {
            return getBucketResponse(r);
        } catch (JSONException e) {
            try {
                return new BucketResponse(helper.toss(new RiakResponseRuntimeException(r, e)));
            } catch (Exception e1) {
                throw new IllegalStateException(
                                                "helper.toss() returns a unsuccessful result, so BucketResponse shouldn't try to parse it or throw");
            }
        } catch (IOException e) {
            try {
                return new BucketResponse(helper.toss(new RiakIORuntimeException(e)));
            } catch (Exception e1) {
                throw new IllegalStateException(
                                                "helper.toss() returns a unsuccessful result, so BucketResponse shouldn't try to read it or throw");
            }
        }
    }

    /**
     * Store a {@link RiakObject}.
     * 
     * @param object
     *            The {@link RiakObject} to store.
     * @param meta
     *            Extra metadata to attach to the request such as w and dw
     *            values for the request, HTTP headers, and other query
     *            parameters. See
     *            {@link RequestMeta#writeParams(Integer, Integer)}.
     * 
     * @return A {@link StoreResponse} containing HTTP response information and
     *         any updated information returned by the server such as the
     *         vclock, last modified date.
     * 
     * @throws RiakIORuntimeException
     *             If an error occurs during communication with the Riak server.
     * @throws RiakResponseRuntimeException
     *             If the Riak server returns a malformed response.
     */
    public StoreResponse store(RiakObject object, RequestMeta meta) {
        if (meta == null) {
            meta = new RequestMeta();
        }
        if (meta.getQueryParam(Constants.QP_RETURN_BODY) == null) {
            meta.setQueryParam(Constants.QP_RETURN_BODY, "true");
        }

        HttpResponse r = helper.store(object, meta);
        return new StoreResponse(r);
    }

    public StoreResponse store(RiakObject object) {
        return store(object, null);
    }

    /**
     * Fetch metadata (e.g. vclock, last modified, vtag) for the
     * {@link RiakObject} stored at <code>bucket</code> and <code>key</code>.
     * 
     * @param bucket
     *            The bucket containing the {@link RiakObject} to fetch.
     * @param key
     *            The key of the {@link RiakObject} to fetch.
     * @param meta
     *            Extra metadata to attach to the request such as an r- value
     *            for the request, HTTP headers, and other query parameters. See
     *            {@link RequestMeta#readParams(int)}.
     * 
     * @return {@link FetchResponse} containing HTTP response information and a
     *         {@link RiakObject} containing only metadata and no value.
     * 
     * @throws RiakIORuntimeException
     *             If an error occurs during communication with the Riak server.
     * @throws RiakResponseRuntimeException
     *             If the Riak server returns a malformed response.
     */
    public FetchResponse fetchMeta(String bucket, String key, RequestMeta meta) {
        try {
            return getFetchResponse(helper.fetchMeta(bucket, key, meta));
        } catch (RiakResponseRuntimeException e) {
            return new FetchResponse(helper.toss(e), this);
        }
    }

    public FetchResponse fetchMeta(String bucket, String key) {
        return fetchMeta(bucket, key, null);
    }

    /**
     * Fetch the {@link RiakObject} (which can include sibling objects) stored
     * at <code>bucket</code> and <code>key</code>.
     * 
     * @param bucket
     *            The bucket containing the {@link RiakObject} to fetch.
     * @param key
     *            The key of the {@link RiakObject} to fetch.
     * @param meta
     *            Extra metadata to attach to the request such as an r- value
     *            for the request, HTTP headers, and other query parameters. See
     *            {@link RequestMeta#readParams(int)}.
     * 
     * @return {@link FetchResponse} containing HTTP response information and a
     *         {@link RiakObject} or sibling objects.
     * 
     * @throws RiakIORuntimeException
     *             If an error occurs during communication with the Riak server.
     * @throws RiakResponseRuntimeException
     *             If the Riak server returns a malformed response.
     */
    public FetchResponse fetch(String bucket, String key, RequestMeta meta) {
        return fetch(bucket, key, meta, false);
    }

    public FetchResponse fetch(String bucket, String key) {
        return fetch(bucket, key, null, false);
    }

    /**
     * Similar to fetch(), except the HTTP connection is left open for
     * successful 2xx responses, and the Riak response is provided as a stream.
     * The user must remember to call {@link FetchResponse#close()} on the
     * return value.
     * 
     * Sibling responses (status code 300) must be read before parsing, so they
     * are not streamed. Therefore stream() is identical to fetch(), except that
     * getBody() returns null.
     * 
     * @param bucket
     *            The bucket containing the {@link RiakObject} to fetch.
     * @param key
     *            The key of the {@link RiakObject} to fetch.
     * @param meta
     *            Extra metadata to attach to the request such as an r- value
     *            for the request, HTTP headers, and other query parameters. See
     *            RequestMeta.readParams().
     * 
     * @return A streaming {@link FetchResponse} containing HTTP response
     *         information and the response stream. The HTTP connection must be
     *         closed manually by the user by calling
     *         {@link FetchResponse#close()}.
     */
    public FetchResponse stream(String bucket, String key, RequestMeta meta) {
        return fetch(bucket, key, meta, true);
    }

    public FetchResponse stream(String bucket, String key) {
        return fetch(bucket, key, null, true);
    }

    FetchResponse fetch(String bucket, String key, RequestMeta meta, boolean streamResponse) {
        if (meta == null) {
            meta = new RequestMeta();
        }

        String accept = meta.getHeader(Constants.HDR_ACCEPT);
        if (accept == null) {
            meta.setHeader(Constants.HDR_ACCEPT, Constants.CTYPE_ANY + ", " + Constants.CTYPE_MULTIPART_MIXED);
        } else {
            meta.setHeader(Constants.HDR_ACCEPT, accept + ", " + Constants.CTYPE_MULTIPART_MIXED);
        }

        HttpResponse r = helper.fetch(bucket, key, meta, streamResponse);

        try {
            return getFetchResponse(r);
        } catch (RiakResponseRuntimeException e) {
            return new FetchResponse(helper.toss(e), this);
        }

    }

    /**
     * Fetch and process the object stored at <code>bucket</code> and
     * <code>key</code> as a stream.
     * 
     * @param bucket
     *            The bucket containing the {@link RiakObject} to fetch.
     * @param key
     *            The key of the {@link RiakObject} to fetch.
     * @param handler
     *            A {@link StreamHandler} to process the Riak response.
     * @param meta
     *            Extra metadata to attach to the request such as an r- value
     *            for the request, HTTP headers, and other query parameters. See
     *            RequestMeta.readParams().
     * 
     * @return Result from calling handler.process() or true if handler is null.
     * 
     * @throws IOException
     *             If an error occurs during communication with the Riak server.
     * 
     * @see StreamHandler
     */
    public boolean stream(String bucket, String key, StreamHandler handler, RequestMeta meta) throws IOException {
        return helper.stream(bucket, key, handler, meta);
    }

    /**
     * Delete the object at <code>bucket</code> and <code>key</code>.
     * 
     * @param bucket
     *            The bucket containing the object.
     * @param key
     *            The key of the object
     * @param meta
     *            Extra metadata to attach to the request such as w and dw
     *            values for the request, HTTP headers, and other query
     *            parameters. See
     *            {@link RequestMeta#writeParams(Integer, Integer)}.
     * 
     * @return {@link HttpResponse} containing HTTP response information.
     * 
     * @throws RiakIORuntimeException
     *             If an error occurs during communication with the Riak server.
     */
    public HttpResponse delete(String bucket, String key, RequestMeta meta) {
        return helper.delete(bucket, key, meta);
    }

    public HttpResponse delete(String bucket, String key) {
        return delete(bucket, key, null);
    }

    /**
     * Perform a map/reduce link walking operation and return the objects for
     * which the "accumulate" flag is true.
     * 
     * @param bucket
     *            The bucket of the "starting object"
     * @param key
     *            The key of the "starting object"
     * @param walkSpec
     *            A URL-path (omit beginning /) of the form
     *            <code>bucket,tag-spec,accumulateFlag</code> The
     *            <code>tag-spec "_"</code> matches all tags.
     *            <code>accumulateFlag</code> is either the String "1" or "0".
     * @param meta
     *            Extra metadata to attach to the request such as HTTP headers
     *            or query parameters.
     * 
     * @return {@link WalkResponse} containing HTTP response information and a
     *         <code>List</code> of <code>Lists</code>, where each sub-list
     *         corresponds to a <code>walkSpec</code> element that had
     *         <code>accumulateFlag</code> equal to 1.
     * 
     * @throws RiakIORuntimeException
     *             If an error occurs during communication with the Riak server.
     * @throws RiakResponseRuntimeException
     *             If the Riak server returns a malformed response.
     * 
     * @see RiakWalkSpec
     */
    public WalkResponse walk(String bucket, String key, String walkSpec, RequestMeta meta) {
        HttpResponse r = helper.walk(bucket, key, walkSpec, meta);

        try {
            return getWalkResponse(r);
        } catch (RiakResponseRuntimeException e) {
            return new WalkResponse(helper.toss(e), this);
        }
    }

    public WalkResponse walk(String bucket, String key, String walkSpec) {
        return walk(bucket, key, walkSpec, null);
    }

    public WalkResponse walk(String bucket, String key, RiakWalkSpec walkSpec) {
        return walk(bucket, key, walkSpec.toString(), null);
    }

    /**
     * Execute a map reduce job on the Riak server.
     * 
     * @param job
     *            JSON string representing the map reduce job to run, which can
     *            be created using {@link MapReduceBuilder}
     * @param meta
     *            Extra metadata to attach to the request such as HTTP headers
     *            or query parameters.
     * 
     * @return {@link MapReduceResponse} containing HTTP response information
     *         and the result of the map reduce job
     * 
     * @throws RiakIORuntimeException
     *             If an error occurs during communication with the Riak server.
     * @throws RiakResponseRuntimeException
     *             If the Riak server does not return a valid JSON array.
     */
    public MapReduceResponse mapReduce(String job, RequestMeta meta) {
        HttpResponse r = helper.mapReduce(job, meta);
        try {
            return getMapReduceResponse(r);
        } catch (JSONException e) {
            helper.toss(new RiakResponseRuntimeException(r, e));
            return null;
        }
    }

    public MapReduceResponse mapReduce(String job) {
        return mapReduce(job, null);
    }

    /**
     * A convenience method for creating a MapReduceBuilder used for building a
     * map reduce job to submission to this client
     * 
     * @param bucket
     *            The bucket to perform the map reduce job over
     * @return A {@link MapReduceBuilder} to build the map reduce job
     */
    public MapReduceBuilder mapReduceOverBucket(String bucket) {
        return new MapReduceBuilder(this).setBucket(bucket);
    }

    /**
     * Same as {@link RiakClient#mapReduceOverBucket(String)}, except over a set
     * of objects instead of a bucket.
     * 
     * @param objects
     *            A set of objects represented as a map of { bucket : [ list of
     *            keys in bucket ] }
     */
    public MapReduceBuilder mapReduceOverObjects(Map<String, Set<String>> objects) {
        return new MapReduceBuilder(this).setRiakObjects(objects);
    }

    /**
     * The installed exception handler or null if not installed
     */
    public RiakExceptionHandler getExceptionHandler() {
        return helper.getExceptionHandler();
    }

    /**
     * If an exception handler is provided, then the Riak client will hand
     * exceptions to the handler rather than throwing them.
     * {@link ClientUtils#throwChecked(Throwable)} can be used to throw
     * undeclared checked exceptions to effectively "convert" RiakClient's
     * unchecked exceptions to checked exceptions.
     */
    public void setExceptionHandler(RiakExceptionHandler exceptionHandler) {
        helper.setExceptionHandler(exceptionHandler);
    }

    /**
     * Return the {@link HttpClient} used to make requests, which can be
     * configured.
     */
    public HttpClient getHttpClient() {
        return helper.getHttpClient();
    }

    /**
     * A 4-byte unique ID for this client. The ID is base 64 encoded and sent to
     * Riak to generating the object vclock on store operations. Refer to the
     * Riak documentation and
     * http://lists.basho.com/pipermail/riak-users_lists.basho.com/2009-
     * November/000153.html for information about the client ID.
     */
    public String getClientId() {
        return helper.getClientId();
    }

    public void setClientId(String clientId) {
        helper.setClientId(clientId);
    }

    // Encapsulate response creation so it can be stubbed for testing
    BucketResponse getBucketResponse(HttpResponse r) throws JSONException, IOException {
        return new BucketResponse(r);
    }

    FetchResponse getFetchResponse(HttpResponse r) throws RiakResponseRuntimeException {
        return new FetchResponse(r, this);
    }

    WalkResponse getWalkResponse(HttpResponse r) throws RiakResponseRuntimeException {
        return new WalkResponse(r, this);
    }

    MapReduceResponse getMapReduceResponse(HttpResponse r) throws JSONException {
        return new MapReduceResponse(r);
    }
}
