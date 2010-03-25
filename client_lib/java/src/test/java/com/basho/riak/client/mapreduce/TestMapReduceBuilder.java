package com.basho.riak.client.mapreduce;

import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.json.JSONException;
import org.junit.Test;

import com.basho.riak.client.RiakObject;
import com.basho.riak.client.request.MapReduceBuilder;

public class TestMapReduceBuilder {

   @Test public void canStoreBucket() {
      MapReduceBuilder builder = new MapReduceBuilder();
      builder.setBucket("foo");
      assertEquals(builder.getBucket(), "foo");
   }
   
   @Test public void canStoreObjects() {
      MapReduceBuilder builder = new MapReduceBuilder();
      builder.addRiakObject("foo", "bar");
      Map<String, Set<String>> objects = builder.getRiakObjects();
      assertEquals(objects.get("foo").size(), 1);
      assertTrue(objects.get("foo").contains("bar"));
      // Verify duplicates are not added
      builder.addRiakObject("foo", "bar");
      assertEquals(1, builder.getRiakObjects().get("foo").size());
   }
   
   @Test public void canRemoveObjects() {
      MapReduceBuilder builder = new MapReduceBuilder();
      builder.addRiakObject("foo", "bar");
      builder.addRiakObject("foo", "baz");
      assertEquals(2, builder.getRiakObjects().get("foo").size());
      builder.removeRiakObject("foo", "bar");
      assertEquals(1, builder.getRiakObjects().get("foo").size());
   }
   
   @Test public void nullParamClearsObject() {
       MapReduceBuilder builder = new MapReduceBuilder();
       builder.addRiakObject("foo", "bar");
       builder.setRiakObjects((Map<String, Set<String>>) null);
       assertEquals(0, builder.getRiakObjects().size());

       builder.addRiakObject("foo", "bar");
       builder.setRiakObjects((Collection<RiakObject>) null);
       assertEquals(0, builder.getRiakObjects().size());
   }
   
   @Test public void extractsRiakObjectInfo() {
       MapReduceBuilder builder = new MapReduceBuilder();
       List<RiakObject> riakObjects = Arrays.asList(new RiakObject[] { new RiakObject("foo", "bar"), 
                                                                   new RiakObject("foo", "baz")});
       builder.setRiakObjects(riakObjects);

       Map<String, Set<String>> objects = builder.getRiakObjects();
       assertEquals(objects.get("foo").size(), 2);
       assertTrue(objects.get("foo").contains("bar"));
       assertTrue(objects.get("foo").contains("baz"));
   }
   
   @Test(expected=IllegalStateException.class)
   public void canUseOnlyObjects() {
      MapReduceBuilder builder = new MapReduceBuilder();
      builder.addRiakObject("foo", "bar");
      builder.setBucket("wubba");
   }
   
   @Test(expected=IllegalStateException.class)
   public void canUseOnlyBucket() {
      MapReduceBuilder builder = new MapReduceBuilder();
      builder.setBucket("wubba");
      builder.addRiakObject("foo", "bar");
   }
   
   @Test public void canBuildJSMapOnlyJobWithBucket() throws JSONException {
      MapReduceBuilder builder = new MapReduceBuilder();
      builder.setBucket("wubba");
      builder.map(JavascriptFunction.named("Riak.mapValuesJson"), true);
      String json = builder.toJSON().toString();
      assertEquals("{\"inputs\":\"wubba\",\"query\":" + 
            "[{\"map\":{\"name\":\"Riak.mapValuesJson\",\"language\":\"javascript\",\"keep\":true}}]}",
            json);
   }
   
   @Test public void canBuildJSMapReduceJobWithBucket() throws JSONException {
      MapReduceBuilder builder = new MapReduceBuilder();
      builder.setBucket("wubba");
      builder.map(JavascriptFunction.anon("function(v) { return [v]; }"), false);
      builder.reduce(JavascriptFunction.named("Riak.reduceMin"), true);
      String json = builder.toJSON().toString();
      assertEquals("{\"inputs\":\"wubba\",\"query\":[{\"map\":{\"source\":" +
            "\"function(v) { return [v]; }\",\"language\":\"javascript\",\"keep\":false}}," + 
            "{\"reduce\":{\"name\":\"Riak.reduceMin\",\"language\":\"javascript\",\"keep\":true}}]}", json);
   }

   
   @Test public void canBuildErlangMapOnlyJobWithBucket() throws JSONException {
      MapReduceBuilder builder = new MapReduceBuilder();
      builder.setBucket("wubba");
      builder.map(new ErlangFunction("foo", "bar"), true);
      String json = builder.toJSON().toString();
      assertEquals("{\"inputs\":\"wubba\",\"query\":[{\"map\":{\"module\":\"foo\"," +
            "\"language\":\"erlang\",\"keep\":true,\"function\":\"bar\"}}]}", json);
   }
   
   @Test public void canBuildErlangMapReduceJobWithBucket() throws JSONException {
      MapReduceBuilder builder = new MapReduceBuilder();
      builder.setBucket("wubba");
      builder.map(new ErlangFunction("foo", "bar"), false);
      builder.reduce(new ErlangFunction("baz", "quux"), true);
      String json =  builder.toJSON().toString();
      assertEquals("{\"inputs\":\"wubba\",\"query\":[{\"map\":{\"module\":\"foo\"," +
            "\"language\":\"erlang\",\"keep\":false,\"function\":\"bar\"}}," +
            "{\"reduce\":{\"module\":\"baz\",\"language\":\"erlang\",\"keep\":true,\"function\":\"quux\"}}]}", json);
   }

   @Test public void canBuildJSMapOnlyJobWithObjects() throws JSONException {
      MapReduceBuilder builder = new MapReduceBuilder();
      builder.addRiakObject("first", "key1");
      builder.map(JavascriptFunction.named("Riak.mapValuesJson"), true);
      String json = builder.toJSON().toString();
      assertEquals("{\"inputs\":[[\"first\",\"key1\"]]," +
            "\"query\":[{\"map\":{\"name\":\"Riak.mapValuesJson\",\"language\":\"javascript\",\"keep\":true}}]}",
            json);
   }
   
   @Test public void canBuildJSMapReduceJobWithObjects() throws JSONException {
      MapReduceBuilder builder = new MapReduceBuilder();
      builder.addRiakObject("first", "key2");
      builder.map(JavascriptFunction.anon("function(v) { return [v]; }"), false);
      builder.reduce(JavascriptFunction.named("Riak.reduceMin"), true);
      String json = builder.toJSON().toString();
      assertEquals("{\"inputs\":[[\"first\",\"key2\"]],\"query\":[{\"map\":{\"source\":" +
            "\"function(v) { return [v]; }\",\"language\":\"javascript\",\"keep\":false}}," + 
            "{\"reduce\":{\"name\":\"Riak.reduceMin\",\"language\":\"javascript\",\"keep\":true}}]}", json);
   }

   
   @Test public void canBuildErlangMapOnlyJobWithObjects() throws JSONException {
      MapReduceBuilder builder = new MapReduceBuilder();
      builder.addRiakObject("first", "key1");
      builder.addRiakObject("second", "key1");
      builder.map(new ErlangFunction("foo", "bar"), true);
      String json = builder.toJSON().toString();
      assertEquals("{\"inputs\":[[\"second\",\"key1\"],[\"first\",\"key1\"]]," +
            "\"query\":[{\"map\":{\"module\":\"foo\",\"language\":\"erlang\",\"keep\":true,\"function\":\"bar\"}}]}",
            json);
   }
   
   @Test public void canBuildErlangMapReduceJobWitObjects() throws JSONException {
      MapReduceBuilder builder = new MapReduceBuilder();
      builder.setBucket("wubba");
      builder.map(new ErlangFunction("foo", "bar"), false);
      builder.reduce(new ErlangFunction("baz", "quux"), true);
      String json =  builder.toJSON().toString();
      assertEquals("{\"inputs\":\"wubba\",\"query\":[{\"map\":{\"module\":\"foo\"," +
            "\"language\":\"erlang\",\"keep\":false,\"function\":\"bar\"}}," +
            "{\"reduce\":{\"module\":\"baz\",\"language\":\"erlang\",\"keep\":true,\"function\":\"quux\"}}]}", json);
   }
   
   @Test public void canBuildLinkMapReduceJob() throws JSONException {
      MapReduceBuilder builder = new MapReduceBuilder();
      builder.setBucket("wubba");
      builder.link("foo", false);
      builder.map(JavascriptFunction.named("Riak.mapValuesJson"), true);
      String json = builder.toJSON().toString();
      assertEquals("{\"inputs\":\"wubba\",\"query\":[{\"link\":{\"bucket\":\"foo\",\"keep\":false}}," +
            "{\"map\":{\"name\":\"Riak.mapValuesJson\",\"language\":\"javascript\",\"keep\":true}}]}", json);
   }

}
