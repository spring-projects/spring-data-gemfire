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
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.data.gemfire.ForkUtil;
import org.springframework.data.gemfire.GemfireCallback;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.function.foo.Foo;

import com.gemstone.bp.edu.emory.mathcs.backport.java.util.Arrays;
import com.gemstone.gemfire.GemFireCheckedException;
import com.gemstone.gemfire.GemFireException;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.cache.client.ClientRegionFactory;
import com.gemstone.gemfire.cache.client.ClientRegionShortcut;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolFactory;
import com.gemstone.gemfire.cache.client.PoolManager;
import com.gemstone.gemfire.cache.execute.Execution;
import com.gemstone.gemfire.cache.execute.FunctionService;
import com.gemstone.gemfire.cache.execute.ResultCollector;

/**
 * @author David Turanski
 * 
 */
public class MethodInvokingFunctionTests {
	private static ClientCache cache = null;

	private static MethodInvokingFunction methodInvokingFunction = new MethodInvokingFunction();
	private static Pool pool = null;
	
	private static Region<String,Integer>  clientRegion = null;

	private static GemfireTemplate gemfireTemplate;
	
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
		
		gemfireTemplate = new GemfireTemplate(clientRegion);
		
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
	public void testInvokeRemoteFunctionOneArg() throws Exception {
		
		gemfireTemplate.setExposeNativeRegion(true);

		Integer val = gemfireTemplate.execute(new GemfireCallback<Integer>() {

			public Integer doInGemfire(@SuppressWarnings("rawtypes") Region region) throws GemFireCheckedException, GemFireException {
			 
				RemoteMethodInvocation invocation = new RemoteMethodInvocation(Foo.class,"oneArg","one");
				Execution execution = FunctionService.onRegion(region);
				ResultCollector<?, ?> resultsCollector = execution.withArgs(invocation).execute(methodInvokingFunction);
				ArrayList<?> result = (ArrayList<?>)resultsCollector.getResult();		
				return (Integer)result.get(0);
			}
		});
		
		assertEquals(1,val.intValue());	  
	}
	
	@Test
	public void testInvokeRemoteFunctionTwoArgs() throws Exception {
		
		gemfireTemplate.setExposeNativeRegion(true);

		Integer val = gemfireTemplate.execute(new GemfireCallback<Integer>() {

			public Integer doInGemfire(@SuppressWarnings("rawtypes") Region region) throws GemFireCheckedException, GemFireException {
				
				RemoteMethodInvocation invocation = new RemoteMethodInvocation(Foo.class,"twoArg","one","three");
				Execution execution = FunctionService.onRegion(region);
				ResultCollector<?, ?> resultsCollector = execution.withArgs(invocation).execute(methodInvokingFunction);
				ArrayList<?> result = (ArrayList<?>)resultsCollector.getResult();		
				return (Integer)result.get(0);
			}
		});
		
		assertEquals(4,val.intValue());
	  
	}
	
	@Test
	public void testInvokeRemoteFunctionCollections() throws Exception {
		
		gemfireTemplate.setExposeNativeRegion(true);

		List<Integer> val = gemfireTemplate.execute(new GemfireCallback<List<Integer>>() {

			@SuppressWarnings("unchecked")
			public List<Integer> doInGemfire(@SuppressWarnings("rawtypes") Region region) throws GemFireCheckedException, GemFireException {
				ArrayList<Integer> list = new ArrayList<Integer>(Arrays.asList(new Integer[]{1,2,3,4,5}));
				RemoteMethodInvocation invocation = new RemoteMethodInvocation(Foo.class,"collections",list);
				Execution execution = FunctionService.onRegion(region);
				
				ResultCollector<?, ?> resultsCollector = execution.withArgs(invocation).execute(methodInvokingFunction);
				ArrayList<?> result = (ArrayList<?>)resultsCollector.getResult();
				//If result type is a list, Gemfire merges it into the results. 
				return (List<Integer>)result;
			}
		});
		
		assertEquals(5,val.size());
		for (int i=0; i<5; i++) {
			assertEquals(i+1,val.get(i).intValue());
		}
	}
	
	@Test
	public void testInvokeRemoteFunctionMap() throws Exception {
		
		gemfireTemplate.setExposeNativeRegion(true);

		Map<String, Integer> val = gemfireTemplate.execute(new GemfireCallback<Map<String, Integer>>() {

			@SuppressWarnings("unchecked")
			public Map<String, Integer> doInGemfire(@SuppressWarnings("rawtypes") Region region) throws GemFireCheckedException, GemFireException {
				RemoteMethodInvocation invocation = new RemoteMethodInvocation(Foo.class,"getMapWithNoArgs");
				Execution execution = FunctionService.onRegion(region);
				ResultCollector<?, ?> resultsCollector = execution.withArgs(invocation).execute(methodInvokingFunction);
				ArrayList<?> result = (ArrayList<?>)resultsCollector.getResult();
				return (Map<String, Integer>)result.get(0);
			}
		});
		
		assertEquals(3,val.size());
		for (int i=0; i<3; i++) {
			assertTrue(val.values().contains(i+1));
		}
	}
}
