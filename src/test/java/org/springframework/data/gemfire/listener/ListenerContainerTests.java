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

package org.springframework.data.gemfire.listener;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Properties;
import java.util.concurrent.BlockingDeque;
import java.util.concurrent.LinkedBlockingDeque;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.ForkUtil;
import org.springframework.data.gemfire.listener.adapter.QueryListenerAdapter;

import com.gemstone.gemfire.cache.RegionService;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolFactory;
import com.gemstone.gemfire.cache.client.PoolManager;
import com.gemstone.gemfire.cache.query.CqEvent;

/**
 * @author Costin Leau
 */
public class ListenerContainerTests {

	private final BlockingDeque<CqEvent> bag = new LinkedBlockingDeque<CqEvent>();
	protected QueryListenerContainer container;

	private static RegionService cache = null;
	private static Pool pool = null;
	private static OutputStream os = null;

	private final Object handler = new Object() {
		public void handleEvent(CqEvent event) {
			bag.add(event);
		}
	};

	private final QueryListenerAdapter adapter = new QueryListenerAdapter(handler);

	@BeforeClass
	public static void startUp() throws Exception {
		os = ForkUtil.cacheServer();

		Properties props = new Properties();
		props.put("mcast-port", "0");
		props.put("name", "cq-client");
		props.put("log-level", "warning");

		CacheFactoryBean cacheFB = new CacheFactoryBean();
		cacheFB.setBeanName("gemfire-cache");
		cacheFB.setUseBeanFactoryLocator(false);
		cacheFB.setProperties(props);
		cacheFB.afterPropertiesSet();

		cache = cacheFB.getObject();


		PoolFactory pf = PoolManager.createFactory();
		pf.addServer("localhost", 40404);
		pf.setSubscriptionEnabled(true);
		pool = pf.create("client");
	}


	@AfterClass
	public static void cleanUp() {
		sendSignal();

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
		String query = "SELECT * from /test";

		container = new QueryListenerContainer();
		container.setQueryService(pool.getQueryService());
		container.setBeanName("container");
		container.addListener(new CqQueryDefinition("test", query, adapter));
		container.afterPropertiesSet();
	}

	private static void sendSignal() {
		try {
			os.write("\n".getBytes());
			os.flush();
		} catch (IOException ex) {
			throw new IllegalStateException("Cannot communicate with forked VM", ex);
		}
	}

	@Test
	public void testContainer() throws Exception {
		sendSignal();
		Thread.sleep(3000);
		System.out.println("Bag is " + bag);
		sendSignal();
	}
}