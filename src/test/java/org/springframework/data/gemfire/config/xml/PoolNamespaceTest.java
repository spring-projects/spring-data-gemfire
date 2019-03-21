/*
 * Copyright 2010-2019 the original author or authors.
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

package org.springframework.data.gemfire.config.xml;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Iterator;

import org.apache.geode.cache.client.Pool;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.client.PoolAdapter;
import org.springframework.data.gemfire.client.PoolFactoryBean;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.support.ConnectionEndpointList;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests for {@link PoolParser} and {@link PoolFactoryBean}.
 *
 * @author Costin Leau
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.client.PoolAdapter
 * @see org.springframework.data.gemfire.client.PoolFactoryBean
 * @see org.springframework.data.gemfire.config.xml.PoolParser
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.apache.geode.cache.client.Pool
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(locations = "pool-ns.xml", initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings("unused")
public class PoolNamespaceTest {

	@Autowired
	private ApplicationContext applicationContext;

	protected void assertConnectionEndpoint(ConnectionEndpointList connectionEndpoints,
			String expectedHost, int expectedPort) {

		assertThat(connectionEndpoints).isNotNull();
		assertThat(connectionEndpoints.size()).isEqualTo(1);
		assertConnectionEndpoint(connectionEndpoints.get(0), expectedHost, expectedPort);
	}

	protected void assertConnectionEndpoint(ConnectionEndpoint connectionEndpoint,
			String expectedHost, int expectedPort) {

		assertThat(connectionEndpoint).isNotNull();
		assertThat(connectionEndpoint.getHost()).isEqualTo(expectedHost);
		assertThat(connectionEndpoint.getPort()).isEqualTo(expectedPort);
	}

	protected void assertNoConnectionEndpoints(ConnectionEndpointList connectionEndpoints) {
		assertThat(connectionEndpoints).isNotNull();
		assertThat(connectionEndpoints.isEmpty()).isTrue();
	}

	@Test
	public void gemfirePoolIsConfiguredProperly() throws Exception {

		assertThat(applicationContext.containsBean("gemfirePool")).isTrue();
		assertThat(applicationContext.containsBean("gemfire-pool")).isTrue();

		PoolFactoryBean poolFactoryBean = applicationContext.getBean("&gemfirePool", PoolFactoryBean.class);

		ConnectionEndpointList locators = TestUtils.readField("locators", poolFactoryBean);

		assertConnectionEndpoint(locators, "localhost", 40403);
	}

	@Test
	public void simplePoolIsConfiguredProperly() throws Exception {

		assertThat(applicationContext.containsBean("simple")).isTrue();

		PoolFactoryBean poolFactoryBean = applicationContext.getBean("&simple", PoolFactoryBean.class);

		ConnectionEndpointList servers = TestUtils.readField("servers", poolFactoryBean);

		assertConnectionEndpoint(servers, PoolParser.DEFAULT_HOST, PoolParser.DEFAULT_SERVER_PORT);

		ConnectionEndpointList locators = TestUtils.readField("locators", poolFactoryBean);

		assertNoConnectionEndpoints(locators);
	}

	@Test
	public void locatorPoolIsConfiguredProperly() throws Exception {

		assertThat(applicationContext.containsBean("locator")).isTrue();

		PoolFactoryBean poolFactoryBean = applicationContext.getBean("&locator", PoolFactoryBean.class);

		ConnectionEndpointList locators = TestUtils.readField("locators", poolFactoryBean);

		assertThat(locators).isNotNull();
		assertThat(locators.size()).isEqualTo(2);

		Iterator<ConnectionEndpoint> it = locators.iterator();

		assertConnectionEndpoint(it.next(), "skullbox", PoolParser.DEFAULT_LOCATOR_PORT);
		assertConnectionEndpoint(it.next(), "ghostrider", 12480);

		ConnectionEndpointList servers = TestUtils.readField("servers", poolFactoryBean);

		assertNoConnectionEndpoints(servers);
	}

	@Test
	public void serverPoolIsConfiguredProperly() throws Exception {

		assertThat(applicationContext.containsBean("server")).isTrue();

		PoolFactoryBean poolFactoryBean = applicationContext.getBean("&server", PoolFactoryBean.class);
		Pool pool = poolFactoryBean.getPool();

		assertThat(pool).isInstanceOf(PoolAdapter.class);
		assertThat(pool.getFreeConnectionTimeout()).isEqualTo(2000);
		assertThat(pool.getIdleTimeout()).isEqualTo(20000L);
		assertThat(pool.getLoadConditioningInterval()).isEqualTo(10000);
		assertThat(Boolean.TRUE.equals(TestUtils.readField("keepAlive", poolFactoryBean))).isTrue();
		assertThat(pool.getMaxConnections()).isEqualTo(100);
		assertThat(pool.getMinConnections()).isEqualTo(5);
		assertThat(pool.getMultiuserAuthentication()).isTrue();
		assertThat(pool.getPingInterval()).isEqualTo(5000L);
		assertThat(pool.getPRSingleHopEnabled()).isFalse();
		assertThat(pool.getReadTimeout()).isEqualTo(500);
		assertThat(pool.getRetryAttempts()).isEqualTo(5);
		assertThat(pool.getServerGroup()).isEqualTo("TestGroup");
		assertThat(pool.getSocketBufferSize()).isEqualTo(65536);
		assertThat(pool.getStatisticInterval()).isEqualTo(250);
		assertThat(pool.getSubscriptionAckInterval()).isEqualTo(250);
		assertThat(pool.getSubscriptionEnabled()).isTrue();
		assertThat(pool.getSubscriptionMessageTrackingTimeout()).isEqualTo(30000);
		assertThat(pool.getSubscriptionRedundancy()).isEqualTo(2);
		assertThat(pool.getSubscriptionTimeoutMultiplier()).isEqualTo(3);
		assertThat(pool.getThreadLocalConnections()).isFalse();

		ConnectionEndpointList servers = TestUtils.readField("servers", poolFactoryBean);

		assertThat(servers).isNotNull();
		assertThat(servers.size()).isEqualTo(2);

		Iterator<ConnectionEndpoint> serversIterator = servers.iterator();

		assertConnectionEndpoint(serversIterator.next(), "localhost", 40404);
		assertConnectionEndpoint(serversIterator.next(), "localhost", 50505);
	}

	@Test
	public void locatorsPoolIsConfiguredProperly() throws Exception {

		assertThat(applicationContext.containsBean("locators")).isTrue();

		PoolFactoryBean poolFactoryBean = applicationContext.getBean("&locators", PoolFactoryBean.class);

		ConnectionEndpointList locators = TestUtils.readField("locators", poolFactoryBean);

		assertThat(locators).isNotNull();
		assertThat(locators.size()).isEqualTo(4);

		Iterator<ConnectionEndpoint> locatorIterator = locators.iterator();

		assertConnectionEndpoint(locatorIterator.next(), "earth", 54321);
		assertConnectionEndpoint(locatorIterator.next(), "venus", 11235);
		assertConnectionEndpoint(locatorIterator.next(), "mars", 10334);
		assertConnectionEndpoint(locatorIterator.next(), "localhost", 12480);
	}

	@Test
	public void serversPoolIsConfiguredProperly() throws Exception {

		assertThat(applicationContext.containsBean("servers")).isTrue();

		PoolFactoryBean poolFactoryBean = applicationContext.getBean("&servers", PoolFactoryBean.class);

		ConnectionEndpointList servers = TestUtils.readField("servers", poolFactoryBean);

		assertThat(servers).isNotNull();
		assertThat(servers.size()).isEqualTo(3);

		Iterator<ConnectionEndpoint> serverIterator = servers.iterator();

		assertConnectionEndpoint(serverIterator.next(), "duke", 21480);
		assertConnectionEndpoint(serverIterator.next(), "nukem", 51515);
		assertConnectionEndpoint(serverIterator.next(), "skullbox", 9110);
	}
}
