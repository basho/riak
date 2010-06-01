package com.basho.riak.jmx;

public interface RiakMBean {

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
	
	public String getNodeName();
	public void setNodeName(String nodeName);
	
	public int getRingCreationSize();
	public void setRingCreationSize(int ringCreationSize);
	
	public int getCpuAvg1();
	public void setCpuAvg1(int cpuAvg1);
	
	public int getCpuAvg5();
	public void setCpuAvg5(int cpuAvg5);
	
	public int getCpuAvg15();
	public void setCpuAvg15(int cpuAvg15);
	
	public float getNodeGetFsmTimeMax();
	public void setNodeGetFsmTimeMax(float nodeGetFsmTimeMax);
	
	public float getNodeGetFsmTimeMean();
	public void setNodeGetFsmTimeMean(float nodeGetFsmTimeMean);
	
	public float getNodeGetFsmTimeMedian();
	public void setNodeGetFsmTimeMedian(float nodeGetFsmTimeMedian);
	
	public float getNodeGetFsmTime95();
	public void setNodeGetFsmTime95(float nodeGetFsmTime95);
	
	public float getNodeGetFsmTime99();
	public void setNodeGetFsmTime99(float nodeGetFsmTime99);

	public float getNodePutFsmTimeMax();	
	public void setNodePutFsmTimeMax(float nodePutFsmTimeMax);

	public float getNodePutFsmTimeMean();
	public void setNodePutFsmTimeMean(float nodePutFsmTimeMean);

	public float getNodePutFsmTimeMedian();
	public void setNodePutFsmTimeMedian(float nodePutFsmTimeMedian);
	
	public float getNodePutFsmTime95();
	public void setNodePutFsmTime95(float nodePutFsmTime95);

	public float getNodePutFsmTime99();
	public void setNodePutFsmTime99(float nodePutFsmTime99);


}
