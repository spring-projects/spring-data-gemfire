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

package org.springframework.data.gemfire.listener;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.data.gemfire.ForkUtil;
import org.springframework.data.gemfire.listener.adapter.ContinuousQueryListenerAdapter;

import com.gemstone.gemfire.cache.RegionService;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.query.CqEvent;

/**
 * @author Costin Leau
 */
public class ListenerContainerTests {

	private final List<CqEvent> bag = new ArrayList<CqEvent>();
	protected ContinuousQueryListenerContainer container;

	private static RegionService cache = null;
	private static Pool pool = null;

	private final Object handler = new Object() {
		public void handleEvent(CqEvent event) {
			bag.add(event);
		}
	};

	private final ContinuousQueryListenerAdapter adapter = new ContinuousQueryListenerAdapter(handler);

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

		// not really used but here just for future tests :)
		//		PoolFactory pf = PoolManager.createFactory();
		//		pf.addServer("localhost", 40404);
		//		pf.setSubscriptionEnabled(true);
		//		Pool pool = pf.create("client");
	}


	@AfterClass
	public static void cleanUp() {
		ForkUtil.sendSignal();

		if (pool != null) {
			pool.destroy();
			pool = null;
		}

		if (cache != null) {
			cache.close();
		}
		cache = null;
	}


	@Before
	public void setUp() throws Exception {
		String query = "SELECT * from /test-cq";

		container = new ContinuousQueryListenerContainer();
		container.setCache(cache);
		//container.setPoolName("client");
		container.setBeanName("container");
		container.afterPropertiesSet();
		container.addListener(new ContinuousQueryDefinition("test", query, adapter));
	}

	@Test
	public void testContainer() throws Exception {
		ForkUtil.sendSignal();
		Thread.sleep(3000);
		System.out.println("Bag is " + bag);
		ForkUtil.sendSignal();
	}
}