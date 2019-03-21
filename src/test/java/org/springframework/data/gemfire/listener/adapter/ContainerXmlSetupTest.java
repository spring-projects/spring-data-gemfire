/*
 * Copyright 2011-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.listener.adapter;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.ForkUtil;
import org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.query.CqQuery;

/**
 * @author Costin Leau
 * @author John Blum
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("/org/springframework/data/gemfire/listener/container.xml")
@SuppressWarnings("unused")
public class ContainerXmlSetupTest {

	@BeforeClass
	public static void init() {
		ForkUtil.cacheServer();
	}

	@AfterClass
	public static void cleanUp() {
		ForkUtil.sendSignal();
	}

	@Autowired
	private ApplicationContext applicationContext;

	@Test
	public void containerSetup() throws Exception {
		ContinuousQueryListenerContainer container = applicationContext.getBean(
			ContinuousQueryListenerContainer.class);

		assertNotNull(container);
		assertTrue(container.isRunning());

		// test getting container listener by bean ID
		ContinuousQueryListenerContainer container2 = applicationContext.getBean("testContainerId",
			ContinuousQueryListenerContainer.class);

		assertSame(container, container2);

		Cache cache = applicationContext.getBean("gemfireCache", Cache.class);
		Pool pool = applicationContext.getBean("client", Pool.class);

		CqQuery[] cqs = cache.getQueryService().getCqs();
		CqQuery[] poolCqs = pool.getQueryService().getCqs();

		assertTrue(pool.getQueryService().getCq("test-bean-1") != null);
		assertEquals(3, cqs.length);
		assertEquals(3, poolCqs.length);
	}

}
