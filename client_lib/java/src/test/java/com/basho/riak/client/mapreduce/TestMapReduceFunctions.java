package com.basho.riak.client.mapreduce;

import static org.junit.Assert.assertEquals;

import org.json.JSONException;
import org.junit.Test;

public class TestMapReduceFunctions {

   @Test public void erlangFunction_generatesCorrectJson() throws JSONException {
      ErlangFunction f = new ErlangFunction("testing", "doit");
      String json = f.toJson().toString();
      assertEquals("{\"module\":\"testing\",\"language\":\"erlang\",\"function\":\"doit\"}", json);
   }
   
   @Test public void namedJSFunction_generatesCorrectJson() throws JSONException {
      JavascriptFunction f = JavascriptFunction.named("Riak.mapValuesJson");
      String json = f.toJson().toString();
      assertEquals("{\"name\":\"Riak.mapValuesJson\",\"language\":\"javascript\"}", json);
   }

   @Test public void anonJSFunction_generatesCorrectJson() throws JSONException {
      JavascriptFunction f = JavascriptFunction.anon("function(v) { return [v]; }");
      String json = f.toJson().toString();
      assertEquals("{\"source\":\"function(v) { return [v]; }\",\"language\":\"javascript\"}", json);
   }
   
   @Test public void anonJSFunction_escapesQuotes() throws JSONException {
      JavascriptFunction f = JavascriptFunction.anon("function(v) { return [{\"value\": v}]; }");
      String json = f.toJson().toString();
      assertEquals("{\"source\":\"function(v) { return [{\\\"value\\\": v}]; }\",\"language\":\"javascript\"}", json);
   }

}
