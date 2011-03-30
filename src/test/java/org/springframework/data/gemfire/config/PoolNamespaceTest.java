/*
 * Copyright 2010-2011 the original author or authors.
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

package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Collection;
import java.util.Iterator;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.client.PoolConnection;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.client.PoolManager;

/**
 * @author Costin Leau
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("pool-ns.xml")
public class PoolNamespaceTest {

	@Autowired
	private ApplicationContext context;

	@Test
	public void testBasicClient() throws Exception {
		assertTrue(context.containsBean("gemfire-pool"));
		assertEquals(context.getBean("gemfire-pool"), PoolManager.find("gemfire-pool"));
		PoolFactoryBean pfb = (PoolFactoryBean) context.getBean("&gemfire-pool");
		Collection<PoolConnection> locators = TestUtils.readField("locators", pfb);
		assertEquals(1, locators.size());
		PoolConnection locator = locators.iterator().next();
		assertEquals("localhost", locator.getHost());
		assertEquals(40403, locator.getPort());
	}

	@Test
	public void testComplexPool() throws Exception {
		assertTrue(context.containsBean("complex"));
		PoolFactoryBean pfb = (PoolFactoryBean) context.getBean("&complex");
		assertEquals(30, TestUtils.readField("retryAttempts", pfb));
		assertEquals(6000, TestUtils.readField("freeConnectionTimeout", pfb));
		assertEquals(5000l, TestUtils.readField("pingInterval", pfb));
		assertTrue((Boolean) TestUtils.readField("subscriptionEnabled", pfb));

		Collection<PoolConnection> servers = TestUtils.readField("servers", pfb);
		assertEquals(2, servers.size());
		Iterator<PoolConnection> iterator = servers.iterator();
		PoolConnection server = iterator.next();
		assertEquals("localhost", server.getHost());
		assertEquals(40404, server.getPort());

		server = iterator.next();
		assertEquals("localhost", server.getHost());
		assertEquals(40405, server.getPort());
	}
}
