package com.basho.riak.jmx;

public interface RiakMBean {

	public String getClusterName();
	public void setClusterName(String clusterName);

	public int getCPUNProcs();
	public void setCPUNProcs(int nprocs);
	
	public float getMemAllocated();
	public void setMemAllocated(float memAllocated);
	
	public void setNodeGets(int nodeGets);
	public int getNodeGets();
	
	public void setNodeGetsTotal(int nodeGetsTotal);
	public int getNodeGetsTotal();
	
	public void setNodePuts(int nodePuts);
	public int getNodePuts();
	
	public void setNodePutsTotal(int nodePutsTotal);
	public int getNodePutsTotal();
	
	public void setMemTotal(float memTotal);
	public float getMemTotal();
	
	public int getVnodeGets();
	public void setVnodeGets(int vnodeGets);
	
	public int getVnodeGetsTotal();
	public void setVnodeGetsTotal(int vnodeGetsTotal);
	
	public int getVnodePuts();
	public void setVnodePuts(int vnodePuts);
	
	public int getVnodePutsTotal();
	public void setVnodePutsTotal(int vnodePutsTotal);
	
	public int getPbcActive();
	public void setPbcActive(int pbcActive);
	
	public int getPbcConnects();
	public void setPbcConnects(int pbcConnects);
	
	public int getPbcConnectsTotal();
	public void setPbcConnectsTotal(int pbcConnectsTotal);

}
