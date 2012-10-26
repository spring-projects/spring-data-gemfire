/*
 * Copyright 2011 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.fork;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Properties;

 
import org.springframework.data.gemfire.ForkUtil;
import org.springframework.data.gemfire.function.MethodInvokingFunction;

import com.gemstone.gemfire.cache.AttributesFactory;
import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.Scope;
import com.gemstone.gemfire.cache.execute.FunctionAdapter;
import com.gemstone.gemfire.cache.execute.FunctionContext;
import com.gemstone.gemfire.cache.execute.FunctionService;
import com.gemstone.gemfire.cache.server.CacheServer;
import com.gemstone.gemfire.distributed.DistributedSystem;

/**
 * @author Costin Leau
 */
public class FunctionCacheServerProcess {
	static Region testRegion;
	public static void main(String[] args) throws Exception {

		Properties props = new Properties();
		props.setProperty("name", "CqServer");
		props.setProperty("log-level", "config");

		System.out.println("\nConnecting to the distributed system and creating the cache.");
		DistributedSystem ds = DistributedSystem.connect(props);
		Cache cache = CacheFactory.create(ds);

		// Create region.
		AttributesFactory factory = new AttributesFactory();
		factory.setDataPolicy(DataPolicy.REPLICATE);
		factory.setScope(Scope.DISTRIBUTED_ACK);
		testRegion = cache.createRegion("test-function", factory.create());
		System.out.println("Test region, " + testRegion.getFullPath() + ", created in cache.");

		// Start Cache Server.
		CacheServer server = cache.addCacheServer();
		server.setPort(40404);
		server.setNotifyBySubscription(true);
		server.start(); 
		System.out.println("Server started");

		 
		testRegion.put("one", 1);
		testRegion.put("two", 2);
		testRegion.put("three", 3);	
		
		System.out.println("Registering ServerFunction");
		FunctionService.registerFunction(new ServerFunction());
		System.out.println("Registered ServerFunction");
		 
		FunctionService.registerFunction(new MethodInvokingFunction());
		System.out.println("Registered MethodInvokingFunction");
		
		ForkUtil.createControlFile(FunctionCacheServerProcess.class.getName());
		
		System.out.println("Waiting for shutdown");
		BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(System.in));
		bufferedReader.readLine();
	}
	
	static class ServerFunction extends FunctionAdapter {
		 
		/* (non-Javadoc)
		 * @see com.gemstone.gemfire.cache.execute.FunctionAdapter#execute(com.gemstone.gemfire.cache.execute.FunctionContext)
		 */
		@Override
		public void execute(FunctionContext functionContext) {
			Object[] args = (Object[])functionContext.getArguments();
			testRegion.put(args[0], args[1]);
			functionContext.getResultSender().lastResult(null);
		}

		/* (non-Javadoc)
		 * @see com.gemstone.gemfire.cache.execute.FunctionAdapter#getId()
		 */
		@Override
		public String getId() {
			return "serverFunction"; 
		}
 
	}
	
	
}
