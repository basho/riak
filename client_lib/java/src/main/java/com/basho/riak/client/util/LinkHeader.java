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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parses the HTTP Link header as described here:
 * 
 * http://tools.ietf.org/html/draft-nottingham-http-link-header
 * 
 * This implementation is more or less a direct port of mnot's Python
 * implementation here:
 * 
 * http://gist.github.com/210535
 * 
 * @author jlee <jonjlee@gmail.com>
 */
public class LinkHeader {

    private static String TOKEN = "(?:[^\\(\\)<>@,;:\\\\\"/\\[\\]\\?={} \\t]+?)";
    private static String QUOTED_STRING = "(?:\"(?:\\\\\"|[^\"])*\")";
    private static String PARAMETER = String.format("(?:%s(?:=(?:%s|%s))?)", TOKEN, TOKEN, QUOTED_STRING);
    private static String LINK = "<[^>]*>\\s*(?:;\\s*" + PARAMETER + "?\\s*)*";
    private static String COMMA = "(?:\\s*(?:,\\s*)+)";
    private static String SEMICOLON = "(?:\\s*(?:;\\s*)+)";
    private static String LINK_SPLIT = LINK + "(?=" + COMMA + "|\\s*$)";
    private static String PARAM_SPLIT = PARAMETER + "(?=" + SEMICOLON + "|\\s*$)";
    private static Pattern LINK_SPLITTER = Pattern.compile(LINK_SPLIT);
    private static Pattern PARAM_SPLITTER = Pattern.compile(PARAM_SPLIT);

    /**
     * Returns a map of links to their parameters. Parameters are a map of
     * parameter name to value.
     * 
     * @param header
     *            Value of the Link header in the format described here:
     * 
     *            http://tools.ietf.org/html/draft-nottingham-http-link-header
     * 
     *            e.g. {@literal </path/to/resource1>; param="value",
     *            </path/to/resource2>}
     * 
     * @return A map of links to their parameters. Parameters are a map of
     *         parameter name to value.
     */
    public static Map<String, Map<String, String>> parse(String header) {
        Map<String, Map<String, String>> out = new LinkedHashMap<String, Map<String, String>>();

        if (header == null || header.length() == 0)
            return out;

        Matcher m = LINK_SPLITTER.matcher(header);
        while (m.find()) {
            String link = m.group().trim();
            String[] urlandparams = link.split(">", 2);
            String url = urlandparams[0].substring(1);
            Map<String, String> parsedLink = new HashMap<String, String>();

            if (urlandparams.length > 1) {
                String params = urlandparams[1];
                for (String param : splitParams(params)) {
                    String[] parts = param.split("=", 2);
                    if (parts.length > 1) {
                        parsedLink.put(parts[0].toLowerCase(), ClientUtils.unquoteString(parts[1]));
                    } else {
                        parsedLink.put(parts[0].toLowerCase(), null);
                    }
                }
            }
            out.put(url, parsedLink);
        }

        return out;
    }

    private static List<String> splitParams(String s) {

        List<String> items = new ArrayList<String>();
        if (s == null || s.length() == 0)
            return items;

        Matcher m = PARAM_SPLITTER.matcher(s);
        while (m.find()) {
            items.add(m.group().trim());
        }

        return items;
    }

}
