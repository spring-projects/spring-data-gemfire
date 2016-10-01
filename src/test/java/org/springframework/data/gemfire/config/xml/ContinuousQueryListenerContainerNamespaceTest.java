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

import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.query.CqListener;
import com.gemstone.gemfire.cache.query.CqQuery;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.ForkUtil;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.listener.ContinuousQueryListener;
import org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer;
import org.springframework.data.gemfire.listener.GemfireMDP;
import org.springframework.data.gemfire.listener.adapter.ContinuousQueryListenerAdapter;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.ErrorHandler;

/**
 * The ContinuousQueryListenerContainerNamespaceTest class is a test suite of test cases testing the SDG XML namespace
 * for proper configuration and initialization of a ContinuousQueryListenerContainer bean component
 * in the Spring context.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see com.gemstone.gemfire.cache.client.ClientCache
 * @see com.gemstone.gemfire.cache.query.CqListener
 * @see com.gemstone.gemfire.cache.query.CqQuery
 * @since 1.4.0
 */
@ContextConfiguration
@RunWith(SpringJUnit4ClassRunner.class)
@SuppressWarnings("unused")
public class ContinuousQueryListenerContainerNamespaceTest {

	@BeforeClass
	public static void setupBeforeClass() {
		ForkUtil.cacheServer();
	}

	@AfterClass
	public static void tearDownAfterClass() {
		ForkUtil.sendSignal();
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

		List<String> actualNames = new ArrayList<String>(3);

		for (CqQuery query : queries) {
			actualNames.add(query.getName());
			assertEquals("SELECT * FROM /test-cq", query.getQueryString());
			assertEquals("Q3".equalsIgnoreCase(query.getName()), query.isDurable());

			CqListener cqListener = query.getCqAttributes().getCqListener();

			assertNotNull(cqListener);

			// the CqListener object should be an instance of...
			// org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer.EventDispatcherAdapter
			// So, get the SDG "ContinuousQueryListener"...
			ContinuousQueryListener listener = TestUtils.readField("delegate", cqListener);

			assertTrue(listener instanceof ContinuousQueryListenerAdapter);
			assertTrue(((ContinuousQueryListenerAdapter) listener).getDelegate() instanceof GemfireMDP);

			if ("Q2".equalsIgnoreCase(query.getName())) {
				assertEquals("handleQuery", TestUtils.readField("defaultListenerMethod", listener));
			}
		}

		actualNames.containsAll(Arrays.asList("Q1", "Q2", "Q3"));
	}

}
