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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.apache.geode.cache.server.CacheServer;
import org.apache.geode.cache.server.ClientSubscriptionConfig;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.util.StringUtils;

/**
 * The CacheServerNamespaceTest class is a test suite of test cases testing the functionality of the SDG XML namespace
 * when configuring a GemFire Cache Servers and Client Subscription.
 *
 * @author Costin Leau
 * @author David Turanski
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.apache.geode.cache.server.ClientSubscriptionConfig
 */
@ContextConfiguration(locations="server-ns.xml", initializers=GemfireTestApplicationContextInitializer.class)
@RunWith(SpringJUnit4ClassRunner.class)
@SuppressWarnings("unused")
public class CacheServerNamespaceTest {

	@Autowired
	private ApplicationContext context;

	@Test
	@SuppressWarnings("deprecation")
	public void testBasicCacheServer() throws Exception {
		CacheServer cacheServer = context.getBean("advanced-config", CacheServer.class);

		assertNotNull(cacheServer);
		assertEquals(1, cacheServer.getGroups().length);
		assertEquals("localhost", cacheServer.getBindAddress());
		assertTrue(cacheServer.getPort() != 0);
		assertEquals("localhost", cacheServer.getHostnameForClients());
		assertEquals("test-server", cacheServer.getGroups()[0]);
		assertEquals(2000l, cacheServer.getLoadPollInterval());
		assertEquals(22, cacheServer.getMaxConnections());
		assertEquals(16, cacheServer.getMaxThreads());
		assertEquals(1000, cacheServer.getMaximumMessageCount());
		assertEquals(30000, cacheServer.getMaximumTimeBetweenPings());
		assertTrue(cacheServer.isRunning());

		ClientSubscriptionConfig clientSubscriptionConfig = cacheServer.getClientSubscriptionConfig();

		assertNotNull(clientSubscriptionConfig);
		assertEquals(1000, clientSubscriptionConfig.getCapacity());
		assertTrue("ENTRY".equalsIgnoreCase(clientSubscriptionConfig.getEvictionPolicy()));
		assertTrue(String.format("Expected empty DiskStoreName; but was (%1$s)", clientSubscriptionConfig.getDiskStoreName()),
			StringUtils.isEmpty(clientSubscriptionConfig.getDiskStoreName()));
	}

}
