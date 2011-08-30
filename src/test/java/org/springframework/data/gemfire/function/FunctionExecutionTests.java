/*
 * Copyright 2002-2011 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.data.gemfire.ForkUtil;
import org.springframework.data.gemfire.GemfireTemplate;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.cache.client.ClientRegionFactory;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolFactory;
import com.gemstone.gemfire.cache.client.PoolManager;
import com.gemstone.gemfire.cache.execute.Function;
import com.gemstone.gemfire.cache.execute.FunctionService;

/**
 * @author David Turanski
 *
 */
public class FunctionExecutionTests {
	
	 
	private static ClientCache cache = null;

	private static Pool pool = null;
	
	private static Region<String,Integer>  clientRegion = null;
	
	@BeforeClass
	public static void startUp() throws Exception {
		ForkUtil.cacheServer();

		Properties props = new Properties();
		props.put("mcast-port", "0");
		props.put("name", "cq-client");
		props.put("log-level", "warning");

		ClientCacheFactory ccf = new ClientCacheFactory(props);
		ccf.setPoolSubscriptionEnabled(true);
		cache = ccf.create();

		PoolFactory pf = PoolManager.createFactory();
		pf.addServer("localhost", 40404);
		pf.setSubscriptionEnabled(true); 
		pool = pf.create("client");
		
		ClientRegionFactory<String, Integer> crf = cache.createClientRegionFactory(ClientRegionShortcut.LOCAL);
		crf.setPoolName("client");
		clientRegion = crf.create("test-cq");
		
		ForkUtil.sendSignal();
		Thread.sleep(500);
		
	 
	}
	
	@AfterClass
	public static void cleanUp() {
		ForkUtil.sendSignal();
		if (clientRegion != null) {
			clientRegion.destroyRegion();
		}
		if (pool != null) {
			pool.destroy();
			pool = null;
		}

		if (cache != null) {
			cache.close();
		}
		cache = null;
	}

	
	@Test
	public void testRegionExecution() {
		RemoteMethodInvocation invocation = new RemoteMethodInvocation(Foo.class, "oneArg", "one");
		RegionFunctionExecution execution = new RegionFunctionExecution(clientRegion,new MethodInvokingFunction(), invocation);
		
		ArrayList<Integer> result = (ArrayList<Integer>)execution.execute();
		assertEquals(1,result.get(0).intValue());
	}
	
//	@Test
//	public void testMembersExecution() {
//		MembersFunctionExecution execution = new MembersFunctionExecution(cache.getDistributedSystem(),new MethodInvokingFunction(Foo.class,"oneArg"),"one");
//		ArrayList<Integer> result = (ArrayList<Integer>)execution.execute();
//		assertEquals(1,result.get(0).intValue());
//	}
	
	public static class Foo {

		private Map<String, Integer> dataSet;

		public Foo(Map<String, Integer> dataSet) {
			this.dataSet = dataSet;
		}
		
		public Foo() {
			
		}
		
 
		public Integer oneArg(String key) {
			 
			return dataSet.get(key);
		}

		public Integer twoArg(String akey, String bkey) {
			if (dataSet.get(akey) != null && dataSet.get(bkey) != null ) {
				return dataSet.get(akey) + dataSet.get(bkey);
			}
			else {
				return null;
			}
		}

	 
		public List<Integer> collections(List<Integer> args) {
			return args;
		}

		 
		public Map<String, Integer> getMapWithNoArgs() {
			if (dataSet.size() == 0) {
				return null;
			}

			return new HashMap<String, Integer>(dataSet);
		}

	}

}
