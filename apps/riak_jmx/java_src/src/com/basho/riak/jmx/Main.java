package com.basho.riak.jmx;

import java.lang.management.ManagementFactory;

import javax.management.MBeanServer;
import javax.management.ObjectName;

public class Main {

	public static void main(String[] args) throws Exception {
		String host = args[0];
		int port = Integer.decode(args[1]).intValue();
		MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
		ObjectName name = new ObjectName("com.basho.riak:type=Riak");
		RiakMBean mbean = new Riak(host, port);
		mbs.registerMBean(mbean, name);
		Riak r = (Riak)mbean;
		while (true) {
			Thread.sleep(30000);
			r.update();
		}
	}
	
}
