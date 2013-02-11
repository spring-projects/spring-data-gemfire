/*
 * Copyright 2012 the original author or authors.
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

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheFactory;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionFactory;
import com.gemstone.gemfire.cache.Scope;
import com.gemstone.gemfire.cache.execute.FunctionAdapter;
import com.gemstone.gemfire.cache.execute.FunctionContext;
import com.gemstone.gemfire.cache.execute.FunctionService;
import com.gemstone.gemfire.cache.server.CacheServer;

/**
 * @author Costin Leau
 * @author David Turanski
 */
public class FunctionCacheServerProcess {
	static Region<Object,Object> testRegion;
	public static void main(String[] args) throws Exception {

		Properties props = new Properties();
		props.setProperty("name", "FunctionServer");
		props.setProperty("log-level", "config");
		props.setProperty("groups","g1,g2,g3");
		 
		 
		Cache cache = new CacheFactory(props).create();

		// Create region.
		RegionFactory<Object,Object> factory = cache.createRegionFactory();
		factory.setDataPolicy(DataPolicy.REPLICATE);
		factory.setScope(Scope.DISTRIBUTED_ACK);
		testRegion = factory.create("test-function");
		System.out.println("Test region, " + testRegion.getFullPath() + ", created in cache.");

		// Start Cache Server.
		CacheServer server = cache.addCacheServer();
		server.setPort(40404);
		server.start(); 
		System.out.println("Server started");

		 
		testRegion.put("one", 1);
		testRegion.put("two", 2);
		testRegion.put("three", 3);	
		
		System.out.println("Registering ServerFunction");
		FunctionService.registerFunction(new ServerFunction());
		System.out.println("Registered ServerFunction");
		
		System.out.println("Registering EchoFunction");
		FunctionService.registerFunction(new EchoFunction());
		System.out.println("Registered EchoFunction");
		
		ForkUtil.createControlFile(FunctionCacheServerProcess.class.getName());
		
		System.out.println("Waiting for shutdown");
		BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(System.in));
		bufferedReader.readLine();
	}
	
	@SuppressWarnings("serial")
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
	
	@SuppressWarnings("serial")
	static class EchoFunction extends FunctionAdapter {
		 
		/* (non-Javadoc)
		 * @see com.gemstone.gemfire.cache.execute.FunctionAdapter#execute(com.gemstone.gemfire.cache.execute.FunctionContext)
		 */
		@Override
		public void execute(FunctionContext functionContext) {
			Object[] args = (Object[])functionContext.getArguments();
			for (int i=0; i< args.length; i++){
				if (i == args.length-1){
					functionContext.getResultSender().lastResult(args[i]);
				} else {
					functionContext.getResultSender().sendResult(args[i]);
				}
			}
		 
			
		}
		@Override
		public String getId() {
			return "echoFunction"; 
		}
 
	}
	
	
}
