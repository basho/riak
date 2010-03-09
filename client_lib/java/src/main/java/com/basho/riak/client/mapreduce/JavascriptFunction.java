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
package com.basho.riak.client.mapreduce;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * Represents a Javascript function used in a map or reduce phase
 * of a map/reduce job
 *
 */
public class JavascriptFunction implements MapReduceFunction {
   
   private String source;
   private MapReduceFunction.Types type;
   
   /**
    * Shortcut for creating a reference to a named
    * Javascript function
    * @param functionName Name of Javascript function ("Riak.mapValuesJson")
    */
   public static JavascriptFunction named(String functionName) {
      return new JavascriptFunction(MapReduceFunction.Types.NAMED, functionName);
   }

   /**
    * Shortcut for creating a reference to an anonymous
    * Javascript function
    * @param functionSource Javascript function source ("function(v) { return [v]; }") 
    */
   public static JavascriptFunction anon(String functionSource) {
      return new JavascriptFunction(MapReduceFunction.Types.ANONYMOUS, functionSource);
   }

   /**
    * Converts the function definition to JSON
    */
   public JSONObject toJson() {
      try {
          JSONObject retval = new JSONObject();
          retval.put("language", "javascript");
          if (type == MapReduceFunction.Types.NAMED) {
             retval.put("name", this.source);
          }
          else {
             retval.put("source", this.source);
          }

          return retval;
      } catch (JSONException e) {
          throw new RuntimeException("Can always map a string to a string");
      }
   }
   
   private JavascriptFunction(MapReduceFunction.Types functionType,
         String functionSource) {
      this.type = functionType;
      this.source = functionSource; 
   }

}
