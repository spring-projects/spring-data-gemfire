/*
 * Copyright 2012-2020 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.client.support;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import java.net.InetSocketAddress;
import java.util.Collections;

import org.apache.geode.cache.client.PoolFactory;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import org.springframework.data.gemfire.GemfireUtils;

/**
 * Unit tests for {@link FactoryDefaultsPoolAdapter}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.springframework.data.gemfire.client.support.FactoryDefaultsPoolAdapter
 * @see org.apache.geode.cache.client.Pool
 * @see org.apache.geode.cache.client.PoolFactory
 * @since 1.8.0
 */
public class FactoryDefaultsPoolAdapterTest {

	protected static final int DEFAULT_CACHE_SERVER_PORT = GemfireUtils.DEFAULT_CACHE_SERVER_PORT;

	private FactoryDefaultsPoolAdapter poolAdapter = new FactoryDefaultsPoolAdapter() { };

	@Rule
	public ExpectedException exception = ExpectedException.none();

	protected InetSocketAddress newSocketAddress(String host, int port) {
		return new InetSocketAddress(host, port);
	}

	@Test
	public void defaultPoolAdapterConfigurationPropertiesReturnDefaultFactorySettings() {

		assertThat(this.poolAdapter.getFreeConnectionTimeout(), is(equalTo(PoolFactory.DEFAULT_FREE_CONNECTION_TIMEOUT)));
		assertThat(this.poolAdapter.getIdleTimeout(), is(equalTo(PoolFactory.DEFAULT_IDLE_TIMEOUT)));
		assertThat(this.poolAdapter.getLoadConditioningInterval(), is(equalTo(PoolFactory.DEFAULT_LOAD_CONDITIONING_INTERVAL)));
		assertThat(this.poolAdapter.getMaxConnections(), is(equalTo(PoolFactory.DEFAULT_MAX_CONNECTIONS)));
		assertThat(this.poolAdapter.getMinConnections(), is(equalTo(PoolFactory.DEFAULT_MIN_CONNECTIONS)));
		assertThat(this.poolAdapter.getMultiuserAuthentication(), is(equalTo(PoolFactory.DEFAULT_MULTIUSER_AUTHENTICATION)));
		assertThat(this.poolAdapter.getPRSingleHopEnabled(), is(equalTo(PoolFactory.DEFAULT_PR_SINGLE_HOP_ENABLED)));
		assertThat(this.poolAdapter.getPingInterval(), is(equalTo(PoolFactory.DEFAULT_PING_INTERVAL)));
		assertThat(this.poolAdapter.getReadTimeout(), is(equalTo(PoolFactory.DEFAULT_READ_TIMEOUT)));
		assertThat(this.poolAdapter.getRetryAttempts(), is(equalTo(PoolFactory.DEFAULT_RETRY_ATTEMPTS)));
		assertThat(this.poolAdapter.getServerGroup(), is(equalTo(PoolFactory.DEFAULT_SERVER_GROUP)));
		assertThat(this.poolAdapter.getSocketBufferSize(), is(equalTo(PoolFactory.DEFAULT_SOCKET_BUFFER_SIZE)));
		assertThat(this.poolAdapter.getSocketConnectTimeout(), is(equalTo(PoolFactory.DEFAULT_SOCKET_CONNECT_TIMEOUT)));
		assertThat(this.poolAdapter.getStatisticInterval(), is(equalTo(PoolFactory.DEFAULT_STATISTIC_INTERVAL)));
		assertThat(this.poolAdapter.getSubscriptionAckInterval(), is(equalTo(PoolFactory.DEFAULT_SUBSCRIPTION_ACK_INTERVAL)));
		assertThat(this.poolAdapter.getSubscriptionEnabled(), is(equalTo(PoolFactory.DEFAULT_SUBSCRIPTION_ENABLED)));
		assertThat(this.poolAdapter.getSubscriptionMessageTrackingTimeout(),
			is(equalTo(PoolFactory.DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT)));
		assertThat(this.poolAdapter.getSubscriptionRedundancy(), is(equalTo(PoolFactory.DEFAULT_SUBSCRIPTION_REDUNDANCY)));
		assertThat(this.poolAdapter.getSubscriptionTimeoutMultiplier(), is(equalTo(PoolFactory.DEFAULT_SUBSCRIPTION_TIMEOUT_MULTIPLIER)));
		assertThat(this.poolAdapter.getThreadLocalConnections(), is(equalTo(PoolFactory.DEFAULT_THREAD_LOCAL_CONNECTIONS)));
	}

	@Test
	public void locatorsReturnsEmptyList() {
		assertThat(this.poolAdapter.getLocators(), is(equalTo(Collections.<InetSocketAddress>emptyList())));
	}

	@Test
	public void nameReturnsDefault() {
		assertThat(this.poolAdapter.getName(), is(equalTo(FactoryDefaultsPoolAdapter.DEFAULT_POOL_NAME)));
	}

	@Test
	public void onlineLocatorsIsEmptyList() {
		assertThat(this.poolAdapter.getOnlineLocators(), is(equalTo(Collections.EMPTY_LIST)));
	}

	@Test
	public void queryServiceIsNull() {
		assertThat(this.poolAdapter.getQueryService(), is(nullValue()));
	}

	@Test
	public void serversReturnsLocalhostListeningOnDefaultCacheServerPort() {
		assertThat(this.poolAdapter.getServers(), is(equalTo(Collections.singletonList(
			newSocketAddress("localhost", DEFAULT_CACHE_SERVER_PORT)))));
	}

	@Test
	public void isDestroyedIsUnsupported() {

		exception.expect(UnsupportedOperationException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage(FactoryDefaultsPoolAdapter.NOT_IMPLEMENTED);

		this.poolAdapter.isDestroyed();
	}

	@Test
	public void getPendingEventCountIsUnsupported() {

		exception.expect(UnsupportedOperationException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage(FactoryDefaultsPoolAdapter.NOT_IMPLEMENTED);

		this.poolAdapter.getPendingEventCount();
	}

	@Test
	public void destroyedIsUnsupported() {

		exception.expect(UnsupportedOperationException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage(FactoryDefaultsPoolAdapter.NOT_IMPLEMENTED);

		this.poolAdapter.destroy();
	}

	@Test
	public void destroyedWithKeepAliveIsUnsupported() {

		exception.expect(UnsupportedOperationException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage(FactoryDefaultsPoolAdapter.NOT_IMPLEMENTED);

		this.poolAdapter.destroy(false);
	}

	@Test
	public void releaseThreadLocalConnectionsIsUnsupported() {

		exception.expect(UnsupportedOperationException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage(FactoryDefaultsPoolAdapter.NOT_IMPLEMENTED);

		this.poolAdapter.releaseThreadLocalConnection();
	}
}
