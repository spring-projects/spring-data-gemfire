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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.data.gemfire.ForkUtil;
import org.springframework.data.gemfire.fork.FunctionCacheServerProcess;
import org.springframework.data.gemfire.function.foo.Foo;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.cache.client.ClientRegionFactory;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolFactory;
import com.gemstone.gemfire.cache.client.PoolManager;

/**
 * @author David Turanski
 * 
 */
public class FunctionExecutionTests {

	private static ClientCache cache = null;

	private static Pool pool = null;

	private static Region<String, Integer> clientRegion = null;

	@BeforeClass
	public static void startUp() throws Exception {
		ForkUtil.cacheServer(FunctionCacheServerProcess.class);

		Properties props = new Properties();
		props.put("mcast-port", "0");
		props.put("name", "function-client");
		props.put("log-level", "warning");

		ClientCacheFactory ccf = new ClientCacheFactory(props);
		ccf.setPoolSubscriptionEnabled(true);
		cache = ccf.create();

		PoolFactory pf = PoolManager.createFactory();
		pf.addServer("localhost", 40404);
		pf.setSubscriptionEnabled(true);
		pool = pf.create("client");

		ClientRegionFactory<String, Integer> crf = cache.createClientRegionFactory(ClientRegionShortcut.PROXY);
		crf.setPoolName("client");
		clientRegion = crf.create("test-function");
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
		RegionFunctionExecution<Integer> execution = new RegionFunctionExecution<Integer>(clientRegion,
				new MethodInvokingFunction(), invocation);

		int result = execution.executeAndExtract();
		assertEquals(1, result);
	}

	@Test
	public void testRegionExecutionWithRegisteredFunction() {
		RemoteMethodInvocation invocation = new RemoteMethodInvocation(Foo.class, "oneArg", "one");
		RegionFunctionExecution<Integer> execution = new RegionFunctionExecution<Integer>(clientRegion,
				new MethodInvokingFunction().getId(), invocation);
		int result = execution.executeAndExtract();
		assertEquals(1, result);
	}

	// TODO: Filter only works on partitioned region. No effect here, but server
	// won't start with a partitioned region. Probably because no locator
	@Test
	public void testRegionExecutionWithFilter() {
		RemoteMethodInvocation invocation = new RemoteMethodInvocation(Foo.class, "oneArg", "one");
		Set<String> keys = new HashSet<String>();
		keys.add("two");
		RegionFunctionExecution<Integer> execution = new RegionFunctionExecution<Integer>(clientRegion,
				new MethodInvokingFunction().getId(), invocation);
		execution.setKeys(keys);
		Integer result = execution.executeAndExtract();
		// assertEquals(null,result.get(0));
		assertEquals(1, result.intValue());
	}

	@Test
	public void testRegionExecutionForMap() {
		RemoteMethodInvocation invocation = new RemoteMethodInvocation(Foo.class, "getMapWithNoArgs");
		RegionFunctionExecution<Map<String, Integer>> execution = new RegionFunctionExecution<Map<String, Integer>>(
				clientRegion, new MethodInvokingFunction().getId(), invocation);
		execution.execute();

		Map<String, Integer> result = execution.executeAndExtract();
		assertTrue(result.containsKey("one"));
		assertEquals(1, result.get("one").intValue());
	}

	@Test
	public void testServersExecutionWithRegisteredFunction() {
		assertNull(clientRegion.get("four"));
		ServersFunctionExecution execution = new ServersFunctionExecution(cache, "serverFunction", "four", new Integer(
				4));
		execution.execute();
		assertNotNull(clientRegion.get("four"));
	}

	@Test
	public void testServerExecutionWithRegisteredFunction() {
		assertNull(clientRegion.get("five"));
		ServerFunctionExecution execution = new ServerFunctionExecution(cache, "serverFunction", "five", new Integer(5));
		Object result = execution.executeAndExtract();
		assertNull(result);
		assertNotNull(clientRegion.get("five"));
	}

}
