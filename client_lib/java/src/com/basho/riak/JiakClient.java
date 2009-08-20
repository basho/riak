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

public class JiakClient {

	private String ip;
	private String port;
	private String prefix;
	
	public JiakClient(String ip, String port) {
		this(ip, port, "/jiak/");
	}
	
	public JiakClient(String ip, String port, String prefix) {
		this.ip = ip;
		this.port = port;		
		this.prefix = prefix;
	}
	
	public void setBucketSchema(String bucket, 
								List<String> allowedFields, 
								List<String> writeMask, 
								List<String> readMask, 
								List<String> requiredFields) 
		throws JSONException, IOException, JiakException {
		
		if (requiredFields == null) requiredFields = new ArrayList<String>();
		if (writeMask == null) writeMask = new ArrayList<String>(allowedFields);
		if (readMask == null) readMask = new ArrayList<String>(allowedFields);
		
		JSONObject schema = new JSONObject();
		schema.put("allowed_fields", allowedFields);
		schema.put("required_fields", requiredFields);
		schema.put("read_mask", readMask);
		schema.put("write_mask", writeMask);
		JSONObject schemaReqBody = new JSONObject();
		schemaReqBody.put("schema", schema);
		HashMap<String, String> reqHeaders = new HashMap<String, String>();
		reqHeaders.put("Content-Type", "application/json");
		reqHeaders.put("Accept", "application/json");
		String reqURI = makeURI(bucket);
		HttpURLConnection requestConn = doRequest("PUT", 
				reqURI, schemaReqBody, reqHeaders);
		if (requestConn.getResponseCode() != 204) {
			throw new JiakException(requestConn.getResponseMessage());
		}
	}
	
	public JSONObject listBucket(String bucket) 
		throws IOException, JSONException, JiakException {
		HashMap<String, String> reqHeaders = new HashMap<String, String>();
		reqHeaders.put("Content-Type", "application/json");
		reqHeaders.put("Accept", "application/json");		
		HttpURLConnection requestConn = doRequest("GET", makeURI(bucket), null, reqHeaders);
		return expect(200, requestConn);
	}
	
	public void store(JiakObject object) 
		throws IOException, JSONException, JiakException {
		HashMap<String, String> reqHeaders = new HashMap<String, String>();
		reqHeaders.put("Content-Type", "application/json");
		reqHeaders.put("Accept", "application/json");
		String reqURI = makeURI(object.getBucket() + "/" + object.getKey() + "?returnbody=true");
		HttpURLConnection requestConn = doRequest("PUT", reqURI, object.toJSONObject(), reqHeaders);
		JSONObject updated = expect(200, requestConn);
		object.update(updated);
	}
	
	public JiakObject fetch(String bucket, String key) 
		throws IOException, JSONException, JiakException {
		HashMap<String, String> reqHeaders = new HashMap<String, String>();
		reqHeaders.put("Content-Type", "application/json");
		reqHeaders.put("Accept", "application/json");
		String reqURI = makeURI(bucket + "/" + key);
		HttpURLConnection requestConn = doRequest("GET", reqURI, null, reqHeaders);
		if (requestConn.getResponseCode() == 404) return null;
		JSONObject objData = expect(200, requestConn);
		JiakObject object = new JiakObject(bucket, key);
		object.update(objData);
		return object;
	}
	
	public void delete(String bucket, String key) throws IOException, JiakException {
		String reqURI = makeURI(bucket + "/" + key);
		HashMap<String, String> reqHeaders = new HashMap<String, String>();
		reqHeaders.put("Accept", "*/*");				
		HttpURLConnection requestConn = doRequest("DELETE", reqURI, null, reqHeaders);
		int responseCode = requestConn.getResponseCode();
		if ((responseCode != 404) && (responseCode != 204)) 
			throw new JiakException(requestConn.getResponseMessage());
	}
	
	protected HttpURLConnection doRequest(String method, 
							 			  String uri, 
							 			  JSONObject body, 
							 			  Map<String, String> headers) 
		throws IOException {
		URL requestURL = new URL(uri);
		HttpURLConnection requestConn = (HttpURLConnection)requestURL.openConnection();
		requestConn.setRequestMethod(method);
		if (headers != null) {
			for (String k : headers.keySet()) {
				requestConn.setRequestProperty(k, headers.get(k));
			}
		}
		if (body != null) {
			requestConn.setDoOutput(true);
			writeRequestBody(requestConn, body);
		}
		return requestConn;
	}

	protected JSONObject expect(int responseCode, HttpURLConnection connection) 
		throws JSONException, JiakException, IOException {
		if (connection.getResponseCode() == responseCode) {
			String responseBody = readResponseBody(connection);
			return new JSONObject(responseBody);
		}
		throw new JiakException(connection.getResponseMessage());
	}
	
	protected String makeURI(String path) {
		return "http://" + this.ip + ":" + this.port + this.prefix + path;
	}
	
	protected static void writeRequestBody(URLConnection connection, 
										   JSONObject body) {
		OutputStream outputStream;
		try {
			outputStream = connection.getOutputStream();
		} catch (IOException e1) {
			e1.printStackTrace();
			return;
		}
		OutputStreamWriter outputStreamWriter = new OutputStreamWriter(outputStream);
		try {
			outputStreamWriter.write(body.toString());
			outputStreamWriter.flush();
		} catch (IOException e) {
			e.printStackTrace();
		}
		finally {
			try {
				outputStream.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	protected static String readResponseBody(HttpURLConnection connection)  {
		String urlContent = null;
		InputStream inputStream;
		try {
			inputStream = connection.getInputStream();
		} catch (IOException e) {
			e.printStackTrace();
			return urlContent;
		}
		try {
			urlContent = downloadStream(inputStream);
		} 
		catch (IOException e) {
			e.printStackTrace();
		}
		finally {
			try {
				inputStream.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return urlContent;
	}

	public static String downloadStream(InputStream in) 
		throws IOException {

		OutputStream out = new ByteArrayOutputStream();
	    try {
	    	copy(in, out);
	    } finally {
	    	in.close();
	    	out.close();
	    }
	    return out.toString();
	     
	}
	
	private static void copy(InputStream in, OutputStream out) 
		throws IOException {
		
		byte[] buffer = new byte[1024];
	    while (true) {
	    	int readCount = in.read(buffer);
	    	if (readCount == -1) {
	    		break;
	    	}
	    	out.write(buffer, 0, readCount);
	    }
	}
}
