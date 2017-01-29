/*
 * Copyright 2011-2018 the original author or authors.
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

import static org.assertj.core.api.Assertions.assertThat;

import java.util.EventListener;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;

import org.apache.geode.cache.CacheClosedException;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientCacheFactory;
import org.apache.geode.cache.query.CqEvent;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.data.gemfire.fork.CqCacheServerProcess;
import org.springframework.data.gemfire.listener.adapter.ContinuousQueryListenerAdapter;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;

/**
 * @author Costin Leau
 */
public class ListenerContainerIntegrationTests extends ClientServerIntegrationTestsSupport {

	private static int availablePort;

	private static ProcessWrapper gemfireServer;

	private ClientCache gemfireCache = null;

	private final ContinuousQueryListenerAdapter adapter = new ContinuousQueryListenerAdapter(new EventListener() {
		public void handleEvent(CqEvent event) {
			cqEvents.add(event);
		}
	});

	private ContinuousQueryListenerContainer container;

	private final List<CqEvent> cqEvents = new CopyOnWriteArrayList<>();

	@BeforeClass
	public static void startGemFireServer() throws Exception {
		availablePort = findAvailablePort();

		gemfireServer = run(CqCacheServerProcess.class,
			String.format("-D%s=%d", GEMFIRE_CACHE_SERVER_PORT_PROPERTY, availablePort));

		waitForServerToStart(DEFAULT_HOSTNAME, availablePort);

		System.setProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY, String.valueOf(availablePort));
	}

	@AfterClass
	public static void stopGemFireServer() {
		System.clearProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY);
		stop(gemfireServer);
	}

	@Before
	public void setupGemFireClient() throws Exception {
		gemfireCache = new ClientCacheFactory()
			.set("name", "ListenerContainerIntegrationTests")
			.set("log-level", "warning")
			.setPoolSubscriptionEnabled(true)
			.addPoolServer(DEFAULT_HOSTNAME, availablePort)
			.create();

		String query = "SELECT * from /test-cq";

		container = new ContinuousQueryListenerContainer();
		container.setBeanName("cqListenerContainer");
		container.setCache(gemfireCache);
		container.afterPropertiesSet();
		container.addListener(new ContinuousQueryDefinition("test", query, adapter));
		container.start();
	}

	@After
	public void tearDownGemFireClient() {
		if (gemfireCache != null) {
			try {
				gemfireCache.close();
			}
			catch (CacheClosedException ignore) {
			}
		}
	}

	@Test
	public void testContainer() throws Exception {
		gemfireServer.signal();
		waitOn(() -> cqEvents.size() == 3, TimeUnit.SECONDS.toMillis(5));
		assertThat(cqEvents.size()).isEqualTo(3);
	}
}
