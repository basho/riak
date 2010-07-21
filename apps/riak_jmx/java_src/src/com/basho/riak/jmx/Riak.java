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
			Riak.this.setNodeName(stats.getString("nodename"));
			Riak.this.setRingCreationSize(stats.getInt("ring_creation_size"));
			Riak.this.setCpuAvg1(stats.getInt("cpu_avg1"));
			Riak.this.setCpuAvg5(stats.getInt("cpu_avg5"));			
			Riak.this.setCpuAvg15(stats.getInt("cpu_avg15"));
			Riak.this.setNodeGetFsmTime95(getStat(stats, "node_get_fsm_time_95"));
			Riak.this.setNodeGetFsmTime99(getStat(stats, "node_get_fsm_time_99"));
			Riak.this.setNodeGetFsmTimeMax(getStat(stats, "node_get_fsm_time_100"));			
			Riak.this.setNodeGetFsmTimeMean(getStat(stats, "node_get_fsm_time_mean"));						
			Riak.this.setNodeGetFsmTimeMedian(getStat(stats, "node_get_fsm_time_median"));			
			Riak.this.setNodePutFsmTime95(getStat(stats, "node_put_fsm_time_95"));
			Riak.this.setNodePutFsmTime99(getStat(stats, "node_put_fsm_time_99"));
			Riak.this.setNodePutFsmTimeMax(getStat(stats, "node_put_fsm_time_100"));			
			Riak.this.setNodePutFsmTimeMean(getStat(stats, "node_put_fsm_time_mean"));						
			Riak.this.setReadRepairs(stats.getInt("read_repairs"));			
			Riak.this.setReadRepairsTotal(stats.getInt("read_repairs_total"));						

		}
		
		private float getStat(JSONObject obj, String key) throws Exception {
			Object val = obj.get(key);
			if (val.equals("undefined")) {
				return -1;
			}
			return Float.parseFloat(val.toString());
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
	String nodeName;
	int ringCreationSize;
	int cpuAvg1;
	int cpuAvg5;
	int cpuAvg15;
	float nodeGetFsmTimeMax;
	float nodeGetFsmTimeMean;
	float nodeGetFsmTimeMedian;
	float nodeGetFsmTime95;
	float nodeGetFsmTime99;
	float nodePutFsmTimeMax;
	float nodePutFsmTimeMean;
	float nodePutFsmTimeMedian;
	float nodePutFsmTime95;
	float nodePutFsmTime99;	
        int   readRepairs;
        int   readRepairsTotal;	

	public Riak(String host, int port) throws Exception {
		super();
		this.fetcher = new RiakStatsFetcher(host, port);
		this.update();
	}
	
	public void update() {
		try {
			this.fetcher.update();
		} catch (Exception e) {
			System.exit(1);
		}
	}

	
	public int getCPUNProcs() {
		return cpuNProcs;
	}

	
	synchronized public void setCPUNProcs(int nprocs) {
		this.cpuNProcs = nprocs;
	}	
	
	
	public float getMemAllocated() {
		return memAllocated;
	}

	
	synchronized public void setMemAllocated(float memAllocated) {
		this.memAllocated = memAllocated;
	}

	
	synchronized public void setNodeGets(int nodeGets) {
		this.nodeGets = nodeGets;
	}

	
	public int getNodeGets() {
		return nodeGets;
	}

	
	synchronized public void setNodeGetsTotal(int nodeGetsTotal) {
		this.nodeGetsTotal = nodeGetsTotal;
	}

	
	public int getNodeGetsTotal() {
		return nodeGetsTotal;
	}

	
	synchronized public void setNodePuts(int nodePuts) {
		this.nodePuts = nodePuts;
	}

	
	public int getNodePuts() {
		return nodePuts;
	}

	
	synchronized public void setNodePutsTotal(int nodePutsTotal) {
		this.nodePutsTotal = nodePutsTotal;
	}

	
	public int getNodePutsTotal() {
		return nodePutsTotal;
	}

	
	synchronized public void setMemTotal(float memTotal) {
		this.memTotal = memTotal;
	}

	
	public float getMemTotal() {
		return memTotal;
	}
	
	
	public int getVnodeGets() {
		return vnodeGets;
	}

	
	synchronized public void setVnodeGets(int vnodeGets) {
		this.vnodeGets = vnodeGets;
	}

	
	public int getVnodeGetsTotal() {
		return vnodeGetsTotal;
	}

	
	synchronized public void setVnodeGetsTotal(int vnodeGetsTotal) {
		this.vnodeGetsTotal = vnodeGetsTotal;
	}

	
	public int getVnodePuts() {
		return vnodePuts;
	}

	
	synchronized public void setVnodePuts(int vnodePuts) {
		this.vnodePuts = vnodePuts;
	}

	
	public int getVnodePutsTotal() {
		return vnodePutsTotal;
	}

	
	synchronized public void setVnodePutsTotal(int vnodePutsTotal) {
		this.vnodePutsTotal = vnodePutsTotal;
	}

	
	public int getPbcActive() {
		return pbcActive;
	}

	
	synchronized public void setPbcActive(int pbcActive) {
		this.pbcActive = pbcActive;
	}

	
	public int getPbcConnects() {
		return pbcConnects;
	}

	
	synchronized public void setPbcConnects(int pbcConnects) {
		this.pbcConnects = pbcConnects;
	}

	
	public int getPbcConnectsTotal() {
		return pbcConnectsTotal;
	}

	
	synchronized public void setPbcConnectsTotal(int pbcConnectsTotal) {
		this.pbcConnectsTotal = pbcConnectsTotal;
	}

	
	public String getNodeName() {
		return nodeName;
	}

	
	synchronized public void setNodeName(String nodeName) {
		this.nodeName = nodeName;
	}

	
	public int getRingCreationSize() {
		return ringCreationSize;
	}

	
	synchronized public void setRingCreationSize(int ringCreationSize) {
		this.ringCreationSize = ringCreationSize;
	}
	
	
	public int getCpuAvg1() {
		return cpuAvg1;
	}

	
	synchronized public void setCpuAvg1(int cpuAvg1) {
		this.cpuAvg1 = cpuAvg1;
	}

	
	public int getCpuAvg5() {
		return cpuAvg5;
	}

	
	synchronized public void setCpuAvg5(int cpuAvg5) {
		this.cpuAvg5 = cpuAvg5;
	}

	
	public int getCpuAvg15() {
		return cpuAvg15;
	}

	
	synchronized public void setCpuAvg15(int cpuAvg15) {
		this.cpuAvg15 = cpuAvg15;
	}
	
	
	public float getNodeGetFsmTimeMax() {
		return nodeGetFsmTimeMax;
	}

	
	synchronized public void setNodeGetFsmTimeMax(float nodeGetFsmTimeMax) {
		this.nodeGetFsmTimeMax = nodeGetFsmTimeMax;
	}

	
	public float getNodeGetFsmTimeMean() {
		return nodeGetFsmTimeMean;
	}

	
	synchronized public void setNodeGetFsmTimeMean(float nodeGetFsmTimeMean) {
		this.nodeGetFsmTimeMean = nodeGetFsmTimeMean;
	}

	
	public float getNodeGetFsmTimeMedian() {
		return nodeGetFsmTimeMedian;
	}

	
	synchronized public void setNodeGetFsmTimeMedian(float nodeGetFsmTimeMedian) {
		this.nodeGetFsmTimeMedian = nodeGetFsmTimeMedian;
	}

	
	public float getNodeGetFsmTime95() {
		return nodeGetFsmTime95;
	}

	
	synchronized public void setNodeGetFsmTime95(float nodeGetFsmTime95) {
		this.nodeGetFsmTime95 = nodeGetFsmTime95;
	}

	
	public float getNodeGetFsmTime99() {
		return nodeGetFsmTime99;
	}
	
	
	synchronized public void setNodeGetFsmTime99(float nodeGetFsmTime99) {
		this.nodeGetFsmTime99 = nodeGetFsmTime99;
	}
	
	
	public float getNodePutFsmTimeMax() {
		return nodePutFsmTimeMax;
	}

	
	synchronized public void setNodePutFsmTimeMax(float nodePutFsmTimeMax) {
		this.nodePutFsmTimeMax = nodePutFsmTimeMax;
	}

	
	public float getNodePutFsmTimeMean() {
		return nodePutFsmTimeMean;
	}
	
	
	synchronized public void setNodePutFsmTimeMean(float nodePutFsmTimeMean) {
		this.nodePutFsmTimeMean = nodePutFsmTimeMean;
	}

	
	public float getNodePutFsmTimeMedian() {
		return nodePutFsmTimeMedian;
	}

	
	synchronized public void setNodePutFsmTimeMedian(float nodePutFsmTimeMedian) {
		this.nodePutFsmTimeMedian = nodePutFsmTimeMedian;
	}

	
	public float getNodePutFsmTime95() {
		return nodePutFsmTime95;
	}

	
	synchronized public void setNodePutFsmTime95(float nodePutFsmTime95) {
		this.nodePutFsmTime95 = nodePutFsmTime95;
	}

	
	public float getNodePutFsmTime99() {
		return nodePutFsmTime99;
	}
	
	
	synchronized public void setNodePutFsmTime99(float nodePutFsmTime99) {
		this.nodePutFsmTime99 = nodePutFsmTime99;
	}

	public int getReadRepairs() {
                return readRepairs;
	}
	
	
        synchronized public void setReadRepairs(int readRepairs) {
                this.readRepairs = readRepairs;
	}

	public int getReadRepairsTotal() {
                return readRepairsTotal;
	}
	
	
        synchronized public void setReadRepairsTotal(int readRepairsTotal) {
                this.readRepairsTotal = readRepairsTotal;
	}
}
