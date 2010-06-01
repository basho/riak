package com.basho.riak.jmx;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

import org.json.JSONException;
import org.json.JSONObject;

public class Riak implements RiakMBean {
	private class RiakStatsFetcher {
		private String host;
		private int port;
		private String statsURL;

		public RiakStatsFetcher(String host, int port) {
			super();
			this.host = host;
			this.port = port;
			this.statsURL = "http://" + this.host + ":" + this.port + "/stats";
		}

		public void update() throws Exception {
			JSONObject stats = fetch();
			Riak.this.setCPUNProcs(stats.getInt("cpu_nprocs"));
			Riak.this.setMemAllocated((float) stats.getDouble("mem_allocated"));
			Riak.this.setMemTotal((float) stats.getDouble("mem_total"));
			Riak.this.setNodeGets(stats.getInt("node_gets"));
			Riak.this.setNodeGetsTotal(stats.getInt("node_gets_total"));
			Riak.this.setNodePuts(stats.getInt("node_puts"));
			Riak.this.setNodePutsTotal(stats.getInt("node_puts_total"));
			Riak.this.setVnodeGets(stats.getInt("vnode_gets"));
			Riak.this.setVnodeGetsTotal(stats.getInt("vnode_gets_total"));
			Riak.this.setVnodePuts(stats.getInt("vnode_puts"));
			Riak.this.setVnodePutsTotal(stats.getInt("vnode_puts_total"));
			Riak.this.setPbcActive(stats.getInt("pbc_active"));
			Riak.this.setPbcConnects(stats.getInt("pbc_connects"));
			Riak.this.setPbcConnectsTotal(stats.getInt("pbc_connects_total"));
		}
		
		protected JSONObject fetch() throws IOException, JSONException {
            URL url = new URL(this.statsURL);
            StringBuilder sb = new StringBuilder();
            HttpURLConnection conn = (HttpURLConnection)url.openConnection();
            InputStream input = conn.getInputStream();
            BufferedReader reader = new BufferedReader(new InputStreamReader(input));
            String json = null;
            while ((json = reader.readLine()) != null) {
                    sb.append(json);
            }
            //System.out.println(new JSONObject(sb.toString()).toString(4));
            return new JSONObject(sb.toString());
		}
	}

	private RiakStatsFetcher fetcher;
	
	// JMX variables
	int cpuNProcs;
	float memAllocated;
	float memTotal;
	int nodeGets;
	int nodeGetsTotal;
	int nodePuts;
	int nodePutsTotal;
	int vnodeGets;
	int vnodeGetsTotal;
	int vnodePuts;
	int vnodePutsTotal;
	int pbcActive;
	int pbcConnects;
	int pbcConnectsTotal;
	
	public Riak(String host, int port) throws Exception {
		super();
		this.fetcher = new RiakStatsFetcher(host, port);
		this.update();
	}
	
	public void update() {
		try {
			this.fetcher.update();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public String getClusterName() {
		return "default";
	}

	@Override
	synchronized public void setClusterName(String clusterName) {
		
	}

	@Override
	public int getCPUNProcs() {
		return cpuNProcs;
	}

	@Override
	synchronized public void setCPUNProcs(int nprocs) {
		this.cpuNProcs = nprocs;
	}	
	
	@Override
	public float getMemAllocated() {
		return memAllocated;
	}

	@Override
	synchronized public void setMemAllocated(float memAllocated) {
		this.memAllocated = memAllocated;
	}

	@Override
	synchronized public void setNodeGets(int nodeGets) {
		this.nodeGets = nodeGets;
	}

	@Override
	public int getNodeGets() {
		return nodeGets;
	}

	@Override
	synchronized public void setNodeGetsTotal(int nodeGetsTotal) {
		this.nodeGetsTotal = nodeGetsTotal;
	}

	@Override
	public int getNodeGetsTotal() {
		return nodeGetsTotal;
	}

	@Override
	synchronized public void setNodePuts(int nodePuts) {
		this.nodePuts = nodePuts;
	}

	@Override
	public int getNodePuts() {
		return nodePuts;
	}

	@Override
	synchronized public void setNodePutsTotal(int nodePutsTotal) {
		this.nodePutsTotal = nodePutsTotal;
	}

	@Override
	public int getNodePutsTotal() {
		return nodePutsTotal;
	}

	@Override
	synchronized public void setMemTotal(float memTotal) {
		this.memTotal = memTotal;
	}

	@Override
	public float getMemTotal() {
		return memTotal;
	}
	
	@Override
	public int getVnodeGets() {
		return vnodeGets;
	}

	@Override
	synchronized public void setVnodeGets(int vnodeGets) {
		this.vnodeGets = vnodeGets;
	}

	@Override
	public int getVnodeGetsTotal() {
		return vnodeGetsTotal;
	}

	@Override
	synchronized public void setVnodeGetsTotal(int vnodeGetsTotal) {
		this.vnodeGetsTotal = vnodeGetsTotal;
	}

	@Override
	public int getVnodePuts() {
		return vnodePuts;
	}

	@Override
	synchronized public void setVnodePuts(int vnodePuts) {
		this.vnodePuts = vnodePuts;
	}

	@Override
	public int getVnodePutsTotal() {
		return vnodePutsTotal;
	}

	@Override
	synchronized public void setVnodePutsTotal(int vnodePutsTotal) {
		this.vnodePutsTotal = vnodePutsTotal;
	}

	@Override
	public int getPbcActive() {
		return pbcActive;
	}

	@Override
	synchronized public void setPbcActive(int pbcActive) {
		this.pbcActive = pbcActive;
	}

	@Override
	public int getPbcConnects() {
		return pbcConnects;
	}

	@Override
	synchronized public void setPbcConnects(int pbcConnects) {
		this.pbcConnects = pbcConnects;
	}

	@Override
	public int getPbcConnectsTotal() {
		return pbcConnectsTotal;
	}

	@Override
	synchronized public void setPbcConnectsTotal(int pbcConnectsTotal) {
		this.pbcConnectsTotal = pbcConnectsTotal;
	}

}
