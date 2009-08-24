/*
This file is provided to you under the Apache License,
Version 2.0 (the "License"); you may not use this file
except in compliance with the License.  You may obtain
a copy of the License at
   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.  
 */
package com.basho.riak;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.net.URLConnection;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.json.JSONException;
import org.json.JSONObject;

import sun.net.www.protocol.http.HttpURLConnection;

/**
 * Client class for accessing the Riak document store via the HTTP/JSON
 * interface.
 * 
 * @author Andy Gross <andy@basho.com>
 * @version 0.1
 */
public class JiakClient {

	private final String ip;
	private final String port;
	private final String prefix;

	/**
	 * Construct a JiakClient with the default server path prefix of /jiak/
	 * 
	 * @param ip
	 *            The IP address of the Riak server
	 * @param port
	 *            The Jiak web port to access
	 */
	public JiakClient(final String ip, final String port) {
		this(ip, port, "/jiak/");
	}

	/**
	 * Construct a JiakClient with a user-specified path prefix
	 * 
	 * @param ip
	 *            The IP address of the Riak server.
	 * @param port
	 *            The Jiak web port to access.
	 * @param prefix
	 *            The server path prefix.
	 */
	public JiakClient(final String ip, final String port, final String prefix) {
		this.ip = ip;
		this.port = port;
		this.prefix = prefix;
	}

	/**
	 * Set the schema describing the structure and per-field permissions for a
	 * Riak bucket.
	 * 
	 * @param bucket
	 *            The bucket name.
	 * @param allowedFields
	 *            The complete list of field names allowed in a document.
	 * @param writeMask
	 *            The list of writable fields, or null. If null, the write mask
	 *            is set to the value of <code>allowedFields</code>.
	 * @param readMask
	 *            The list of readable fields, or null. If null, the read mask
	 *            is set to the value of <code>allowedFields</code>.
	 * @param requiredFields
	 *            The list of fields that must be present when creating or
	 *            updating a document. If null, <code>requiredFields</code> is
	 *            set to an empty list.
	 * 
	 * @throws JSONException
	 *             If an error occurs assembling the JSON request body.
	 * @throws IOException
	 *             If an error occurs during communication with the Riak server.
	 * @throws JiakException
	 *             If the Riak server returns an error or or an unexpected
	 *             response code.
	 */
	public void setBucketSchema(final String bucket,
			final List<String> allowedFields, List<String> writeMask,
			List<String> readMask, List<String> requiredFields)
			throws JSONException, IOException, JiakException {

		if (requiredFields == null)
			requiredFields = new ArrayList<String>();
		if (writeMask == null)
			writeMask = new ArrayList<String>(allowedFields);
		if (readMask == null)
			readMask = new ArrayList<String>(allowedFields);

		final JSONObject schema = new JSONObject();
		schema.put("allowed_fields", allowedFields);
		schema.put("required_fields", requiredFields);
		schema.put("read_mask", readMask);
		schema.put("write_mask", writeMask);
		final JSONObject schemaReqBody = new JSONObject();
		schemaReqBody.put("schema", schema);
		final HashMap<String, String> reqHeaders = new HashMap<String, String>();
		reqHeaders.put("Content-Type", "application/json");
		reqHeaders.put("Accept", "application/json");
		final String reqURI = makeURI(bucket);
		final HttpURLConnection requestConn = doRequest("PUT", reqURI,
				schemaReqBody, reqHeaders);
		if (requestConn.getResponseCode() != 204)
			throw new JiakException(requestConn.getResponseMessage());
	}

	/**
	 * Return the schema and keys for a Riak bucket.
	 * 
	 * @param bucket
	 *            The bucket to list.
	 * @return A <code>JSONObject</code> with keys <code>schema</code> and
	 *         <code>keys</code>.
	 * @throws JSONException
	 *             If an error occurs assembling the JSON request body.
	 * @throws IOException
	 *             If an error occurs during communication with the Riak server.
	 * @throws JiakException
	 *             If the Riak server returns an error or or an unexpected
	 *             response code.
	 */
	public JSONObject listBucket(final String bucket) throws IOException,
			JSONException, JiakException {
		final HashMap<String, String> reqHeaders = new HashMap<String, String>();
		reqHeaders.put("Content-Type", "application/json");
		reqHeaders.put("Accept", "application/json");
		final HttpURLConnection requestConn = doRequest("GET", makeURI(bucket),
				null, reqHeaders);
		return expect(200, requestConn);
	}

	/**
	 * Store a {@link JiakObject}.
	 * 
	 * @param object
	 *            The {@link JiakObject} to store.
	 * 
	 * @throws JSONException
	 *             If an error occurs assembling the JSON request body.
	 * @throws IOException
	 *             If an error occurs during communication with the Riak server.
	 * @throws JiakException
	 *             If the Riak server returns an error or or an unexpected
	 *             response code.
	 */
	public void store(final JiakObject object) throws IOException,
			JSONException, JiakException {
		final HashMap<String, String> reqHeaders = new HashMap<String, String>();
		reqHeaders.put("Content-Type", "application/json");
		reqHeaders.put("Accept", "application/json");
		final String reqURI = makeURI(object.getBucket() + "/"
				+ object.getKey() + "?returnbody=true");
		final HttpURLConnection requestConn = doRequest("PUT", reqURI, object
				.toJSONObject(), reqHeaders);
		final JSONObject updated = expect(200, requestConn);
		object.update(updated);
	}

	/**
	 * Fetch the {@link JiakObject} stored at <code>bucket</code> and
	 * <code>key</code>.
	 * 
	 * @param bucket
	 *            The bucket containing the {@link JiakObject} to fetch.
	 * @param key
	 *            The key of the {@link JiakObject} to fetch.
	 * 
	 * @return A {@link JiakObject}.
	 * 
	 * @throws JSONException
	 *             If an error occurs assembling the JSON request body.
	 * @throws IOException
	 *             If an error occurs during communication with the Riak server.
	 * @throws JiakException
	 *             If the Riak server returns an error or or an unexpected
	 *             response code.
	 */
	public JiakObject fetch(final String bucket, final String key)
			throws IOException, JSONException, JiakException {
		final HashMap<String, String> reqHeaders = new HashMap<String, String>();
		reqHeaders.put("Content-Type", "application/json");
		reqHeaders.put("Accept", "application/json");
		final String reqURI = makeURI(bucket + "/" + key);
		final HttpURLConnection requestConn = doRequest("GET", reqURI, null,
				reqHeaders);
		if (requestConn.getResponseCode() == 404)
			return null;
		final JSONObject objData = expect(200, requestConn);
		final JiakObject object = new JiakObject(bucket, key);
		object.update(objData);
		return object;
	}

	/**
	 * Delete a {@link JiakObject}.
	 * 
	 * @param bucket
	 *            The bucket containing the object to delete.
	 * @param key
	 *            The key of the object to delete.
	 * 
	 * @throws JSONException
	 *             If an error occurs assembling the JSON request body.
	 * @throws IOException
	 *             If an error occurs during communication with the Riak server.
	 * @throws JiakException
	 *             If the Riak server returns an error or or an unexpected
	 *             response code.
	 */
	public void delete(final String bucket, final String key)
			throws IOException, JiakException {
		final String reqURI = makeURI(bucket + "/" + key);
		final HashMap<String, String> reqHeaders = new HashMap<String, String>();
		reqHeaders.put("Accept", "*/*");
		final HttpURLConnection requestConn = doRequest("DELETE", reqURI, null,
				reqHeaders);
		final int responseCode = requestConn.getResponseCode();
		if (responseCode != 404 && responseCode != 204)
			throw new JiakException(requestConn.getResponseMessage());
	}

	protected HttpURLConnection doRequest(final String method,
			final String uri, final JSONObject body,
			final Map<String, String> headers) throws IOException {
		final URL requestURL = new URL(uri);
		final HttpURLConnection requestConn = (HttpURLConnection) requestURL
				.openConnection();
		requestConn.setRequestMethod(method);
		if (headers != null)
			for (final String k : headers.keySet())
				requestConn.setRequestProperty(k, headers.get(k));
		if (body != null) {
			requestConn.setDoOutput(true);
			writeRequestBody(requestConn, body);
		}
		return requestConn;
	}

	protected JSONObject expect(final int responseCode,
			final HttpURLConnection connection) throws JSONException,
			JiakException, IOException {
		if (connection.getResponseCode() == responseCode) {
			final String responseBody = readResponseBody(connection);
			return new JSONObject(responseBody);
		}
		throw new JiakException(connection.getResponseMessage());
	}

	protected String makeURI(final String path) {
		return "http://" + ip + ":" + port + prefix + path;
	}

	protected static void writeRequestBody(final URLConnection connection,
			final JSONObject body) {
		OutputStream outputStream;
		try {
			outputStream = connection.getOutputStream();
		} catch (final IOException e1) {
			e1.printStackTrace();
			return;
		}
		final OutputStreamWriter outputStreamWriter = new OutputStreamWriter(
				outputStream);
		try {
			outputStreamWriter.write(body.toString());
			outputStreamWriter.flush();
		} catch (final IOException e) {
			e.printStackTrace();
		} finally {
			try {
				outputStream.close();
			} catch (final IOException e) {
				e.printStackTrace();
			}
		}
	}

	protected static String readResponseBody(final HttpURLConnection connection) {
		String urlContent = null;
		InputStream inputStream;
		try {
			inputStream = connection.getInputStream();
		} catch (final IOException e) {
			e.printStackTrace();
			return urlContent;
		}
		try {
			urlContent = downloadStream(inputStream);
		} catch (final IOException e) {
			e.printStackTrace();
		} finally {
			try {
				inputStream.close();
			} catch (final IOException e) {
				e.printStackTrace();
			}
		}
		return urlContent;
	}

	protected static String downloadStream(final InputStream in)
			throws IOException {

		final OutputStream out = new ByteArrayOutputStream();
		try {
			copy(in, out);
		} finally {
			in.close();
			out.close();
		}
		return out.toString();

	}

	protected static void copy(final InputStream in, final OutputStream out)
			throws IOException {

		final byte[] buffer = new byte[1024];
		while (true) {
			final int readCount = in.read(buffer);
			if (readCount == -1)
				break;
			out.write(buffer, 0, readCount);
		}
	}
}
