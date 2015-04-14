/*
 * Copyright 2010-2013 the original author or authors.
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
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.net.InetSocketAddress;
import java.util.Collection;
import java.util.Iterator;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.client.PoolManager;

/**
 * @author Costin Leau
 * @author John Blum
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations="pool-ns.xml", initializers=GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class PoolNamespaceTest {

	@Autowired
	private ApplicationContext context;

	protected void assertSocketAddress(InetSocketAddress socketAddress, String expectedHost, int expectedPort) {
		assertNotNull(socketAddress);
		assertEquals(expectedHost, socketAddress.getHostName());
		assertEquals(expectedPort, socketAddress.getPort());
	}

	@Test
	public void testBasicClient() throws Exception {
		assertTrue(context.containsBean("DEFAULT"));
		assertTrue(context.containsBean("gemfirePool"));
		assertTrue(context.containsBean("gemfire-pool"));
		assertEquals(context.getBean("gemfirePool"), PoolManager.find("DEFAULT"));

		PoolFactoryBean poolFactoryBean = context.getBean("&gemfirePool", PoolFactoryBean.class);
		Collection<InetSocketAddress> locators = TestUtils.readField("locators", poolFactoryBean);

		assertNotNull(locators);
		assertEquals(1, locators.size());

		assertSocketAddress(locators.iterator().next(), "localhost", 40403);
	}

	@Test
	public void testComplexPool() throws Exception {
		assertTrue(context.containsBean("complex"));

		PoolFactoryBean poolFactoryBean = context.getBean("&complex", PoolFactoryBean.class);

		assertEquals(30, TestUtils.readField("retryAttempts", poolFactoryBean));
		assertEquals(6000, TestUtils.readField("freeConnectionTimeout", poolFactoryBean));
		assertEquals(5000l, TestUtils.readField("pingInterval", poolFactoryBean));
		assertTrue((Boolean) TestUtils.readField("subscriptionEnabled", poolFactoryBean));
		assertFalse((Boolean) TestUtils.readField("multiUserAuthentication", poolFactoryBean));
		assertTrue((Boolean) TestUtils.readField("prSingleHopEnabled", poolFactoryBean));

		Collection<InetSocketAddress> servers = TestUtils.readField("servers", poolFactoryBean);

		assertNotNull(servers);
		assertEquals(2, servers.size());

		Iterator<InetSocketAddress> serversIterator = servers.iterator();

		assertSocketAddress(serversIterator.next(), "localhost", 40404);
		assertSocketAddress(serversIterator.next(), "localhost", 40405);
	}

	@Test
	public void testComboLocatorPool() throws Exception {
		assertTrue(context.containsBean("combo-locators"));

		PoolFactoryBean poolFactoryBean = context.getBean("&combo-locators", PoolFactoryBean.class);
		Collection<InetSocketAddress> locators = TestUtils.readField("locators", poolFactoryBean);

		assertNotNull(locators);
		assertEquals(3, locators.size());

		Iterator<InetSocketAddress> locatorIterator = locators.iterator();

		assertSocketAddress(locatorIterator.next(), "foobar", 55421);
		assertSocketAddress(locatorIterator.next(), "lavatube", 11235);
		assertSocketAddress(locatorIterator.next(), "zod", 10334);
	}

	@Test
	public void testComboServerPool() throws Exception {
		assertTrue(context.containsBean("combo-servers"));

		PoolFactoryBean poolFactoryBean = context.getBean("&combo-servers", PoolFactoryBean.class);
		Collection<InetSocketAddress> locators = TestUtils.readField("servers", poolFactoryBean);

		Collection<InetSocketAddress> servers = TestUtils.readField("servers", poolFactoryBean);

		assertNotNull(servers);
		assertEquals(2, servers.size());

		Iterator<InetSocketAddress> serverIterator = locators.iterator();

		assertSocketAddress(serverIterator.next(), "scorch", 21480);
		assertSocketAddress(serverIterator.next(), "skullbox", 9110);
	}

}
