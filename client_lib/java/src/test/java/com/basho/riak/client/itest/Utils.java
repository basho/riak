package com.basho.riak.client.itest;

import static org.junit.Assert.*;

import org.apache.commons.httpclient.URIException;

import com.basho.riak.client.request.RequestMeta;
import com.basho.riak.client.response.HttpResponse;

public class Utils {

    public static RequestMeta WRITE_3_REPLICAS() { return RequestMeta.writeParams(3, 3); }

    public static void assertSuccess(HttpResponse response) {
        if (!response.isSuccess()) {
            StringBuilder msg = new StringBuilder("Failed ");
            msg.append(response.getHttpMethod().getName()).append(" ");
            try {
                msg.append(response.getHttpMethod().getURI().toString());
            } catch (URIException e) {
                msg.append(response.getHttpMethod().getPath());
            }
            msg.append(" -- ")
                .append(response.getHttpMethod().getStatusLine()).append("; ")
                .append("Response headers: ").append(response.getHttpHeaders().toString()).append("; ")
                .append("Response body: ").append(response.getBody());
            fail(msg.toString());
        }
    }

}
