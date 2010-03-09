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
package com.basho.riak.client.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Represents a multipart entity as described here:
 * 
 * http://tools.ietf.org/html/rfc2046#section-5.1
 */
public class Multipart {

    private static String HEADER_DELIM = "\n\n";

    /**
     * Parses a multipart message or a multipart subpart of a multipart message.
     * 
     * @return A list of the parts parsed into headers and body of this
     *         multipart message
     */
    public static List<Multipart.Part> parse(Map<String, String> headers, String body) {
        if (headers == null || body == null)
            return null;

        if (!body.startsWith("\n")) {
            // In order to parse the multipart efficiently, we want to treat the
            // first boundary identically to the others, so make sure that the
            // first boundary is preceded by a '\n' like the others
            body = "\n" + body;
        }

        String boundary = "\n--" + getBoundary(headers.get(Constants.HDR_CONTENT_TYPE));
        int boundarySize = boundary.length();
        if ("\n--".equals(boundary))
            return null;

        // While this parsing could be more efficiently done in one pass with a
        // hand written FSM, hopefully this method is more readable/intuitive.
        List<Part> parts = new ArrayList<Part>();
        int pos = body.indexOf(boundary);
        if (pos != -1) {
            while (pos < body.length()) {
                // first char of part
                int start = pos + boundarySize;
                // last char of part + 1
                int end = body.indexOf(boundary, start);
                // end of header section + 1
                int headerEnd = body.indexOf(HEADER_DELIM, pos);
                // start of body section
                int bodyStart = headerEnd + HEADER_DELIM.length();

                // check for end boundary, which is (boundary + "--")
                if (body.substring(start).startsWith("--")) {
                    break;
                }

                if (end == -1) {
                    end = body.length();
                }

                if (headerEnd == -1) {
                    headerEnd = body.length();
                    bodyStart = end;
                }

                if (bodyStart > end) {
                    bodyStart = end;
                }

                Map<String, String> partHeaders = parseHeaders(body.substring(start, headerEnd));
                String partBody = body.substring(bodyStart, end);
                parts.add(new Part(partHeaders, partBody));

                pos = end;
            }
        }

        return parts;
    }

    /**
     * Parse a block of header lines as defined here:
     * 
     * http://tools.ietf.org/html/rfc822#section-3.2
     * 
     * @param s
     *            The header blob
     * @return Map of header names to values
     */
    private static Map<String, String> parseHeaders(String s) {
        // "unfold" header lines (http://tools.ietf.org/html/rfc822#section-3.1)
        s.replaceAll("\n\\s+", " ");

        String[] headers = s.split("\n");
        Map<String, String> parsedHeaders = new HashMap<String, String>();
        for (String header : headers) {
            // Split header line into name and value
            String[] nv = header.split("\\s*:\\s*", 2);
            if (nv.length > 1) {
                parsedHeaders.put(nv[0].trim().toLowerCase(), nv[1].trim());
            }
        }
        return parsedHeaders;
    }

    /**
     * Given a content type value, get the "boundary" parameter
     * 
     * @param contentType
     *            Content type value with boundary parameter. Should be of the
     *            form "type/subtype; boundary=foobar; param=value"
     * @return Value of the boundary parameter
     */
    private static String getBoundary(String contentType) {
        String[] params = contentType.split("\\s*;\\s*");
        for (String param : params) {
            String[] nv = param.split("\\s*=\\s*", 2);
            if (nv.length > 1) {
                if ("boundary".equals(nv[0].toLowerCase()))
                    return ClientUtils.unquoteString(nv[1]);
            }
        }
        return "";
    }

    /**
     * A single part of a multipart entity
     */
    public static class Part {
        private Map<String, String> headers;
        private String body;

        public Part(Map<String, String> headers, String body) {
            super();
            this.headers = headers;
            this.body = body;
        }

        /**
         * Headers defined in the part
         */
        public Map<String, String> getHeaders() {
            return headers;
        }

        /**
         * Body of this part
         */
        public String getBody() {
            return body;
        }

    }
}
