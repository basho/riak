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
package com.basho.riak.client.plain;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import com.basho.riak.client.RiakBucketInfo;
import com.basho.riak.client.RiakClient;
import com.basho.riak.client.RiakConfig;
import com.basho.riak.client.RiakObject;
import com.basho.riak.client.request.RequestMeta;
import com.basho.riak.client.request.RiakWalkSpec;
import com.basho.riak.client.response.BucketResponse;
import com.basho.riak.client.response.FetchResponse;
import com.basho.riak.client.response.HttpResponse;
import com.basho.riak.client.response.RiakExceptionHandler;
import com.basho.riak.client.response.RiakResponseRuntimeException;
import com.basho.riak.client.response.StoreResponse;
import com.basho.riak.client.response.StreamHandler;
import com.basho.riak.client.response.WalkResponse;

/**
 * An adapter from {@link RiakClient} to a slightly less HTTP, more
 * Java-centric, interface. Objects are returned without HTTP specific
 * information and exceptions are thrown on unsuccessful responses.
 */
public class PlainClient {

    private RiakClient impl;

    /** Connect to Riak using the given configuration. */
    public static PlainClient getClient(RiakConfig config) {
        return new PlainClient(new RiakClient(config));
    }

    /** Connect to Riak using the given URL. */
    public static PlainClient getClient(String url) {
        return new PlainClient(new RiakClient(url));
    }

    /**
     * The primary constructor. Wraps an existing {@link RiakClient} and
     * installs a custom {@link RiakExceptionHandler}.
     */
    public PlainClient(RiakClient riakClient) {
        impl = riakClient;
        impl.setExceptionHandler(new ConvertToCheckedExceptions());
    }

    /**
     * Like
     * {@link RiakClient#setBucketSchema(String, RiakBucketInfo, RequestMeta)},
     * except throws on a non-204 response.
     * 
     * @throws RiakIOException
     *             If an error occurs during communication with the Riak server.
     * @throws RiakResponseException
     *             if the server does not successfully update the bucket schema.
     */
    public void setBucketSchema(String bucket, RiakBucketInfo bucketInfo, RequestMeta meta) throws RiakIOException,
            RiakResponseException {
        HttpResponse r = impl.setBucketSchema(bucket, bucketInfo, meta);

        if (r.getStatusCode() != 204)
            throw new RiakResponseException(new RiakResponseRuntimeException(r, r.getBody()));
    }

    public void setBucketSchema(String bucket, RiakBucketInfo bucketInfo) throws RiakIOException, RiakResponseException {
        setBucketSchema(bucket, bucketInfo, null);
    }

    /**
     * Like {@link RiakClient#listBucket(String, RequestMeta)}, except throws on
     * a non-200 response
     * 
     * @throws RiakIOException
     *             If an error occurs during communication with the Riak server.
     * @throws RiakResponseException
     *             if the server does not return the bucket information
     */
    public RiakBucketInfo listBucket(String bucket, RequestMeta meta) throws RiakIOException, RiakResponseException {
        BucketResponse r = impl.listBucket(bucket, meta);

        if (r.getStatusCode() != 200)
            throw new RiakResponseException(new RiakResponseRuntimeException(r, r.getBody()));

        return r.getBucketInfo();
    }

    public RiakBucketInfo listBucket(String bucket) throws RiakIOException, RiakResponseException {
        return listBucket(bucket, null);
    }

    /**
     * Like {@link RiakClient#store(RiakObject, RequestMeta)}, except throws on
     * a non-200 or 204 response and updates the passed in {@link RiakObject}
     * with new metadata from Riak.
     * 
     * @throws RiakIOException
     *             If an error occurs during communication with the Riak server.
     * @throws RiakResponseException
     *             If the server does not succesfully store the object.
     */
    public void store(RiakObject object, RequestMeta meta) throws RiakIOException, RiakResponseException {
        StoreResponse r = impl.store(object, meta);

        if (r.getStatusCode() != 200 && r.getStatusCode() != 204)
            throw new RiakResponseException(new RiakResponseRuntimeException(r, r.getBody()));

        object.updateMeta(r);
    }

    public void store(RiakObject object) throws RiakIOException, RiakResponseException {
        store(object, null);
    }

    /**
     * Like {@link RiakClient#fetchMeta(String, String, RequestMeta)}, except it
     * returns the fetched object metadata directly or throws if the response is
     * not a 200, 304 or 404.
     * 
     * @return {@link RiakObject} or null if object doesn't exist.
     * 
     * @throws RiakIOException
     *             If an error occurs during communication with the Riak server.
     * @throws RiakResponseException
     *             If the server does return a valid object
     */
    public RiakObject fetchMeta(String bucket, String key, RequestMeta meta) throws RiakIOException,
            RiakResponseException {
        FetchResponse r = impl.fetchMeta(bucket, key, meta);

        if (r.getStatusCode() == 404)
            return null;

        if (r.getStatusCode() != 200 && r.getStatusCode() != 304)
            throw new RiakResponseException(new RiakResponseRuntimeException(r, r.getBody()));

        if (r.getStatusCode() == 200 && !r.hasObject())
            throw new RiakResponseException(new RiakResponseRuntimeException(r, "Failed to parse metadata"));

        return r.getObject();
    }

    public RiakObject fetchMeta(String bucket, String key) throws RiakIOException, RiakResponseException {
        return fetchMeta(bucket, key, null);
    }

    /**
     * Like {@link RiakClient#fetch(String, String, RequestMeta)}, except it
     * returns the fetched object directly or throws if the response is not a
     * 200, 304 or 404.
     * 
     * @return {@link RiakObject} or null if object doesn't exist. If siblings
     *         exist, then returns one of the siblings.
     * @throws RiakIOException
     *             If an error occurs during communication with the Riak server.
     * @throws RiakResponseException
     *             If the server does return a valid object
     */
    public RiakObject fetch(String bucket, String key, RequestMeta meta) throws RiakIOException, RiakResponseException {
        FetchResponse r = impl.fetch(bucket, key, meta);

        if (r.getStatusCode() == 404)
            return null;

        if (r.getStatusCode() != 200 && r.getStatusCode() != 304)
            throw new RiakResponseException(new RiakResponseRuntimeException(r, r.getBody()));

        if (r.getStatusCode() == 200 && !r.hasObject())
            throw new RiakResponseException(new RiakResponseRuntimeException(r, "Failed to parse object"));

        return r.getObject();
    }

    public RiakObject fetch(String bucket, String key) throws RiakIOException, RiakResponseException {
        return fetch(bucket, key, null);
    }

    /**
     * Like {@link RiakClient#fetch(String, String, RequestMeta)}, except it
     * returns the all the fetched objects directly or throws if the response is
     * not a 200, 304 or 404.
     * 
     * @return All sibling {@link RiakObject} or null no object exist.
     * @throws RiakIOException
     *             If an error occurs during communication with the Riak server.
     * @throws RiakResponseException
     *             If the server does return any valid objects
     */
    public Collection<? extends RiakObject> fetchAll(String bucket, String key, RequestMeta meta)
            throws RiakIOException, RiakResponseException {
        FetchResponse r = impl.fetch(bucket, key, meta);

        if (r.getStatusCode() == 404)
            return null;

        if (r.getStatusCode() != 200 && r.getStatusCode() != 304)
            throw new RiakResponseException(new RiakResponseRuntimeException(r, r.getBody()));

        if (r.getStatusCode() == 200 && !(r.hasObject() || r.hasSiblings()))
            throw new RiakResponseException(new RiakResponseRuntimeException(r, "Failed to parse object"));

        if (r.hasSiblings())
            return r.getSiblings();
        return Arrays.asList(r.getObject());
    }

    public Collection<? extends RiakObject> fetchAll(String bucket, String key) throws RiakIOException,
            RiakResponseException {
        return fetchAll(bucket, key, null);
    }

    /**
     * Identical to
     * {@link RiakClient#stream(String, String, StreamHandler, RequestMeta)}.
     */
    public boolean stream(String bucket, String key, StreamHandler handler, RequestMeta meta) throws IOException {
        return impl.stream(bucket, key, handler, meta);
    }

    /**
     * Like {@link RiakClient#delete(String, String, RequestMeta)}, except
     * throws on a non-200 or 404 response. Note that delete succeeds if the
     * object did not previously exist (404 response).
     * 
     * @throws RiakIOException
     *             If an error occurs during communication with the Riak server.
     * @throws RiakResponseException
     *             If the object was not deleted.
     */
    public void delete(String bucket, String key, RequestMeta meta) throws RiakIOException, RiakResponseException {
        HttpResponse r = impl.delete(bucket, key, meta);

        if (r.getStatusCode() != 204 && r.getStatusCode() != 404)
            throw new RiakResponseException(new RiakResponseRuntimeException(r, r.getBody()));
    }

    public void delete(String bucket, String key) throws RiakIOException, RiakResponseException {
        delete(bucket, key, null);
    }

    /**
     * Like {@link RiakClient#walk(String, String, String, RequestMeta)}, except
     * throws on a non-200 or 404 response.
     * 
     * @return list of lists of {@link RiakObject}s corresponding to steps of
     *         the walk. Returns null if the source object doesn't exist.
     * @throws RiakIOException
     *             If an error occurs during communication with the Riak server.
     * @throws RiakResponseException
     *             If the links could not be walked or the result steps were not
     *             returned.
     */
    public List<? extends List<? extends RiakObject>> walk(String bucket, String key, String walkSpec, RequestMeta meta)
            throws RiakIOException, RiakResponseException {
        WalkResponse r = impl.walk(bucket, key, walkSpec, meta);

        if (r.getStatusCode() == 404)
            return null;

        if (r.getStatusCode() != 200)
            throw new RiakResponseException(new RiakResponseRuntimeException(r, r.getBody()));

        if (!r.hasSteps())
            throw new RiakResponseException(new RiakResponseRuntimeException(r, "Failed to parse walk results"));

        return r.getSteps();
    }

    public List<? extends List<? extends RiakObject>> walk(String bucket, String key, String walkSpec)
            throws RiakIOException, RiakResponseException {
        return walk(bucket, key, walkSpec, null);
    }

    public List<? extends List<? extends RiakObject>> walk(String bucket, String key, RiakWalkSpec walkSpec)
            throws RiakIOException, RiakResponseException {
        return walk(bucket, key, walkSpec.toString(), null);
    }
}