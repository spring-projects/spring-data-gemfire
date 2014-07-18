/*
 * Copyright 2011-2013 the original author or authors.
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
import com.gemstone.gemfire.cache.server.CacheServer;

/**
 * @author Costin Leau
 * @author John Blum
 */
@SuppressWarnings("unchecked")
public class CacheServerProcess {

	public static void main(final String[] args) throws Exception {
		Properties gemfireProperties = new Properties();
		gemfireProperties.setProperty("name", "CqServer");
		gemfireProperties.setProperty("mcast-port", "0");
		gemfireProperties.setProperty("log-level", "warning");

		Cache cache = new CacheFactory(gemfireProperties).create();

		RegionFactory<String, Integer> regionFactory = cache.createRegionFactory();
		regionFactory.setDataPolicy(DataPolicy.REPLICATE);
		regionFactory.setScope(Scope.DISTRIBUTED_ACK);

		Region<String, Integer> testRegion = regionFactory.create("test-cq");

		System.out.printf("Test Region '%1$s' created in Cache '%2$s.%n", testRegion.getFullPath(), cache.getName());

		CacheServer server = cache.addCacheServer();
		server.setPort(40404);
		server.start();

		ForkUtil.createControlFile(CacheServerProcess.class.getName());

		System.out.println("Waiting for signal...");

		BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(System.in));
		bufferedReader.readLine();

		System.out.println("Signal received!");

		testRegion.put("one", 1);
		testRegion.put("two", 2);
		testRegion.put("three", 3);

		System.out.println("Waiting for shutdown...");

		bufferedReader.readLine();

		System.out.println("Shutting down!");
	}

}
