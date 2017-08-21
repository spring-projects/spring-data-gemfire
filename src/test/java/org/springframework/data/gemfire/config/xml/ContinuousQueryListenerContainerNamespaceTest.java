/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.Executor;

import javax.annotation.Resource;

import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.query.CqListener;
import org.apache.geode.cache.query.CqQuery;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.fork.CqCacheServerProcess;
import org.springframework.data.gemfire.listener.ContinuousQueryListener;
import org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer;
import org.springframework.data.gemfire.listener.GemfireMDP;
import org.springframework.data.gemfire.listener.adapter.ContinuousQueryListenerAdapter;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.util.ErrorHandler;

/**
 * Integration tests for the configuration and initialization of the SDG {@link ContinuousQueryListenerContainer}
 * using the SDG XML namespace.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.query.CqListener
 * @see org.apache.geode.cache.query.CqQuery
 * @see org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 1.4.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class ContinuousQueryListenerContainerNamespaceTest extends ClientServerIntegrationTestsSupport {

	private static ProcessWrapper gemfireServer;

	@BeforeClass
	public static void startGemFireServer() throws Exception {

		int availablePort = findAvailablePort();

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

	@Autowired
	private ClientCache gemfireCache;

	@Autowired
	private ContinuousQueryListenerContainer container;

	@Resource(name = "testErrorHandler")
	private ErrorHandler testErrorHandler;

	@Resource(name = "testTaskExecutor")
	private Executor testTaskExecutor;

	@Test
	public void testContainerConfiguration() throws Exception {

		assertNotNull("The ContinuousQueryListenerContainer was not properly configured!", container);
		assertTrue("The CQ Listener Container should be active (initialized)!", container.isActive());
		assertFalse("The CQ Listener container should not be configured to auto-start!", container.isAutoStartup());
		assertFalse("The CQ Listener Container should not be running!", container.isRunning());
		assertEquals(4, container.getPhase());
		assertNotNull(testErrorHandler);
		assertSame(testErrorHandler, TestUtils.readField("errorHandler", container));
		assertNotNull(testTaskExecutor);
		assertSame(testTaskExecutor, TestUtils.readField("taskExecutor", container));

		CqQuery[] queries = gemfireCache.getQueryService().getCqs();

		assertNotNull(queries);
		assertEquals(3, queries.length);

		List<String> actualNames = new ArrayList<>(3);

		for (CqQuery query : queries) {

			actualNames.add(query.getName());

			assertEquals("SELECT * FROM /test-cq", query.getQueryString());
			assertEquals("Q3".equalsIgnoreCase(query.getName()), query.isDurable());

			CqListener cqListener = query.getCqAttributes().getCqListener();

			// The CqListener should be an instance of o.s.d.g.listener.ContinuousQueryListenerContainer.EventDispatcherAdapter
			// So, get the SDG "ContinuousQueryListener"
			ContinuousQueryListener listener = TestUtils.readField("listener", cqListener);

			assertTrue(listener instanceof ContinuousQueryListenerAdapter);
			assertTrue(((ContinuousQueryListenerAdapter) listener).getDelegate() instanceof GemfireMDP);

			if ("Q2".equalsIgnoreCase(query.getName())) {
				assertEquals("handleQuery", TestUtils.readField("defaultListenerMethod", listener));
			}
		}

		actualNames.containsAll(Arrays.asList("Q1", "Q2", "Q3"));
	}
}
