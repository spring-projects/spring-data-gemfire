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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import java.util.Iterator;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
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

	protected void assertConnectionEndpoint(ConnectionEndpoint connectionEndpoint, String expectedHost, int expectedPort) {
		assertThat(connectionEndpoint, is(notNullValue()));
		assertThat(connectionEndpoint.getHost(), is(equalTo(expectedHost)));
		assertThat(connectionEndpoint.getPort(), is(equalTo(expectedPort)));
	}

	@Test
	public void testBasicClient() throws Exception {
		assertThat(context.containsBean("DEFAULT"), is(true));
		assertThat(context.containsBean("gemfirePool"), is(true));
		assertThat(context.containsBean("gemfire-pool"), is(true));
		assertThat(PoolManager.find("DEFAULT"), is(equalTo(context.getBean("gemfirePool"))));

		PoolFactoryBean poolFactoryBean = context.getBean("&gemfirePool", PoolFactoryBean.class);

		ConnectionEndpointList locators = TestUtils.readField("locators", poolFactoryBean);

		assertThat(locators, is(notNullValue()));
		assertThat(locators.size(), is(equalTo(1)));

		assertConnectionEndpoint(locators.iterator().next(), "localhost", 40403);
	}

	@Test
	public void testSimplePool() throws Exception {
		assertThat(context.containsBean("simple"), is(true));

		PoolFactoryBean poolFactoryBean = context.getBean("&simple", PoolFactoryBean.class);

		ConnectionEndpointList locators = TestUtils.readField("locators", poolFactoryBean);

		assertThat(locators, is(notNullValue()));
		assertThat(locators.size(), is(equalTo(1)));

		assertConnectionEndpoint(locators.iterator().next(), PoolParser.DEFAULT_HOST, PoolParser.DEFAULT_LOCATOR_PORT);

		ConnectionEndpointList servers = TestUtils.readField("servers", poolFactoryBean);

		assertThat(servers, is(notNullValue()));
		assertThat(servers.isEmpty(), is(true));
	}

	@Test
	public void testLocatorPool() throws Exception {
		assertThat(context.containsBean("locator"), is(true));

		PoolFactoryBean poolFactoryBean = context.getBean("&locator", PoolFactoryBean.class);

		ConnectionEndpointList locators = TestUtils.readField("locators", poolFactoryBean);

		assertThat(locators, is(notNullValue()));
		assertThat(locators.size(), is(equalTo(2)));

		Iterator<ConnectionEndpoint> it = locators.iterator();

		assertConnectionEndpoint(it.next(), "skullbox", PoolParser.DEFAULT_LOCATOR_PORT);
		assertConnectionEndpoint(it.next(), "yorktown", 12480);

		ConnectionEndpointList servers = TestUtils.readField("servers", poolFactoryBean);

		assertThat(servers, is(notNullValue()));
	}

	@Test
	public void testComplexPool() throws Exception {
		assertTrue(context.containsBean("complex"));

		PoolFactoryBean poolFactoryBean = context.getBean("&complex", PoolFactoryBean.class);

		assertEquals(2000, TestUtils.readField("freeConnectionTimeout", poolFactoryBean));
		assertEquals(20000l, TestUtils.readField("idleTimeout", poolFactoryBean));
		assertEquals(10000, TestUtils.readField("loadConditioningInterval", poolFactoryBean));
		assertEquals(true, TestUtils.readField("keepAlive", poolFactoryBean));
		assertEquals(100, TestUtils.readField("maxConnections", poolFactoryBean));
		assertEquals(5, TestUtils.readField("minConnections", poolFactoryBean));
		assertEquals(5, TestUtils.readField("minConnections", poolFactoryBean));
		assertTrue((Boolean) TestUtils.readField("multiUserAuthentication", poolFactoryBean));
		assertEquals(5000l, TestUtils.readField("pingInterval", poolFactoryBean));
		assertFalse((Boolean) TestUtils.readField("prSingleHopEnabled", poolFactoryBean));
		assertEquals(500, TestUtils.readField("readTimeout", poolFactoryBean));
		assertEquals(5, TestUtils.readField("retryAttempts", poolFactoryBean));
		assertEquals("TestGroup", TestUtils.readField("serverGroup", poolFactoryBean));
		assertEquals(65536, TestUtils.readField("socketBufferSize", poolFactoryBean));
		assertEquals(5000, TestUtils.readField("statisticInterval", poolFactoryBean));
		assertEquals(250, TestUtils.readField("subscriptionAckInterval", poolFactoryBean));
		assertTrue((Boolean) TestUtils.readField("subscriptionEnabled", poolFactoryBean));
		assertEquals(30000, TestUtils.readField("subscriptionMessageTrackingTimeout", poolFactoryBean));
		assertEquals(2, TestUtils.readField("subscriptionRedundancy", poolFactoryBean));
		assertTrue((Boolean) TestUtils.readField("threadLocalConnections", poolFactoryBean));

		ConnectionEndpointList servers = TestUtils.readField("servers", poolFactoryBean);

		assertNotNull(servers);
		assertEquals(2, servers.size());

		Iterator<ConnectionEndpoint> serversIterator = servers.iterator();

		assertConnectionEndpoint(serversIterator.next(), "localhost", 40404);
		assertConnectionEndpoint(serversIterator.next(), "localhost", 40405);
	}

	@Test
	public void testComboLocatorPool() throws Exception {
		assertThat(context.containsBean("combo-locators"), is(true));

		PoolFactoryBean poolFactoryBean = context.getBean("&combo-locators", PoolFactoryBean.class);

		ConnectionEndpointList locators = TestUtils.readField("locators", poolFactoryBean);

		assertThat(locators, is(notNullValue()));
		assertThat(locators.size(), is(equalTo(3)));

		Iterator<ConnectionEndpoint> locatorIterator = locators.iterator();

		assertConnectionEndpoint(locatorIterator.next(), "foobar", 55421);
		assertConnectionEndpoint(locatorIterator.next(), "lavatube", 11235);
		assertConnectionEndpoint(locatorIterator.next(), "zod", 10334);
	}

	@Test
	public void testComboServerPool() throws Exception {
		assertThat(context.containsBean("combo-servers"), is(true));

		PoolFactoryBean poolFactoryBean = context.getBean("&combo-servers", PoolFactoryBean.class);

		ConnectionEndpointList servers = TestUtils.readField("servers", poolFactoryBean);

		assertThat(servers, is(notNullValue()));
		assertThat(servers.size(), is(equalTo(3)));

		Iterator<ConnectionEndpoint> serverIterator = servers.iterator();

		assertConnectionEndpoint(serverIterator.next(), "scorch", 21480);
		assertConnectionEndpoint(serverIterator.next(), "scorn", 51515);
		assertConnectionEndpoint(serverIterator.next(), "skullbox", 9110);
	}

}
