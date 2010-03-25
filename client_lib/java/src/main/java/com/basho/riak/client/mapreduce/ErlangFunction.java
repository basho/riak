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
 * Represents an Erlang function used in a map or reduce phase
 * of a map/reduce job
 *
 */
public class ErlangFunction implements MapReduceFunction {
   
   private String module;
   private String function;

   /**
    * Constructs a new ErlangFunction instance
    * @param module Erlang module name
    * @param functionName Erlang function name
    */
   public ErlangFunction(String module, String functionName) {
      this.module = module;
      this.function = functionName;
   }
   
   /**
    * Converts the function definition to JSON
    */
   public JSONObject toJson() {
      try {
          JSONObject retval = new JSONObject();
          retval.put("language", "erlang");
          retval.put("module", this.module);
          retval.put("function", this.function);
          return retval;
      } catch (JSONException e) {
          throw new RuntimeException("Can always map a string to a string");
      }
   }

}
