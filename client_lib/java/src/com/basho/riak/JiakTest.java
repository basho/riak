package com.basho.riak;

import java.util.ArrayList;
import java.util.List;

import org.json.JSONArray;

public class JiakTest {

	public static void main(String[] args) throws Exception {
		JiakClient client = new JiakClient("127.0.0.1", "8098");
		ArrayList<String> allKeys = new ArrayList<String>();
		allKeys.add("testkey");
		allKeys.add("jroot");
		allKeys.add("jleaf1");
		allKeys.add("jleaf2");
		allKeys.add("jleaf3");
		for (String k : allKeys) {
			try {
				client.delete("jiak_example", k);
			}
			catch (Exception e) {}
		}
		JiakObject jo = new JiakObject("jiak_example", "testkey");
		jo.set("foo", 2);
		client.store(jo);
		jo = client.fetch("jiak_example", "testkey");
		assert(jo.get("foo").equals(2));
		JiakObject jRoot = new JiakObject("jiak_example", "jroot");
		jRoot.set("foo", 0);
		JiakObject jLeaf1 = new JiakObject("jiak_example", "jleaf1");
		jLeaf1.set("foo", "in results");
		JiakObject jLeaf2 = new JiakObject("jiak_example", "jleaf2");
		jLeaf2.set("foo", "in results");
		JiakObject jLeaf3 = new JiakObject("jiak_example", "jleaf3");
		jLeaf3.set("foo", "not in results");
		JSONArray links = new JSONArray();
		links.put(new String[]{"jiak_example", "jleaf1", "tag_one"});
		links.put(new String[]{"jiak_example", "jleaf2", "tag_one"});		
		links.put(new String[]{"jiak_example", "jleaf3", "tag_other"});				
		jRoot.setLinks(links);
		client.store(jRoot);
		client.store(jLeaf1);
		client.store(jLeaf2);
		client.store(jLeaf3);
		ArrayList<ArrayList<JiakObject>> res = client.walk("jiak_example", "jroot", "jiak_example,tag_one,1");
		for (ArrayList<JiakObject> i : res) {
			for (JiakObject j: i) {
				assert(j.get("foo").equals("in results"));
			}
		}
		for (String k : allKeys) {
			try {
				client.delete("jiak_example", k);
			}
			catch (Exception e) {}
		}
		System.out.println("all tests passed");
	}
}
