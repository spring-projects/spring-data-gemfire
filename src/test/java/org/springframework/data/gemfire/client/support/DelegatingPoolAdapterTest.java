/*
 * Copyright 2012-2018 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.client.support;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.net.InetSocketAddress;
import java.util.Collections;

import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.client.PoolFactory;
import org.apache.geode.cache.query.QueryService;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.data.gemfire.GemfireUtils;

/**
 * The DelegatingPoolAdapterTest class is a test suite of test cases testing the contract and functionality
 * of the {@link DelegatingPoolAdapter} class.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see DelegatingPoolAdapter
 * @see org.apache.geode.cache.client.Pool
 * @see org.apache.geode.cache.client.PoolFactory
 * @since 1.8.0
 */
@RunWith(MockitoJUnitRunner.class)
public class DelegatingPoolAdapterTest {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Mock
	private Pool mockPool;

	@Mock
	private QueryService mockQueryService;

	private InetSocketAddress newSocketAddress(String host, int port) {
		return new InetSocketAddress(host, port);
	}

	@Before
	public void setup() {

		when(this.mockPool.isDestroyed()).thenReturn(false);
		when(this.mockPool.getFreeConnectionTimeout()).thenReturn(10000);
		when(this.mockPool.getIdleTimeout()).thenReturn(120000L);
		when(this.mockPool.getLoadConditioningInterval()).thenReturn(300000);
		when(this.mockPool.getLocators()).thenReturn(Collections.singletonList(newSocketAddress("skullbox", 11235)));
		when(this.mockPool.getMaxConnections()).thenReturn(500);
		when(this.mockPool.getMinConnections()).thenReturn(50);
		when(this.mockPool.getMultiuserAuthentication()).thenReturn(true);
		when(this.mockPool.getName()).thenReturn("MockPool");
		when(this.mockPool.getOnlineLocators()).thenReturn(Collections.singletonList(newSocketAddress("trinity", 10101)));
		when(this.mockPool.getPendingEventCount()).thenReturn(2);
		when(this.mockPool.getPingInterval()).thenReturn(15000L);
		when(this.mockPool.getPRSingleHopEnabled()).thenReturn(true);
		when(this.mockPool.getQueryService()).thenReturn(this.mockQueryService);
		when(this.mockPool.getReadTimeout()).thenReturn(30000);
		when(this.mockPool.getRetryAttempts()).thenReturn(1);
		when(this.mockPool.getServerGroup()).thenReturn("TestGroup");
		when(this.mockPool.getServers()).thenReturn(Collections.singletonList(newSocketAddress("xghost", 12480)));
		when(this.mockPool.getSocketBufferSize()).thenReturn(16384);
		when(this.mockPool.getSocketConnectTimeout()).thenReturn(5000);
		when(this.mockPool.getStatisticInterval()).thenReturn(1000);
		when(this.mockPool.getSubscriptionAckInterval()).thenReturn(200);
		when(this.mockPool.getSubscriptionEnabled()).thenReturn(true);
		when(this.mockPool.getSubscriptionMessageTrackingTimeout()).thenReturn(60000);
		when(this.mockPool.getSubscriptionRedundancy()).thenReturn(2);
		when(this.mockPool.getSubscriptionTimeoutMultiplier()).thenReturn(3);
		when(this.mockPool.getThreadLocalConnections()).thenReturn(false);
	}

	@Test
	public void delegateEqualsMockPool() {
		assertThat(DelegatingPoolAdapter.from(this.mockPool).getDelegate(), is(equalTo(this.mockPool)));
	}

	@Test
	public void mockPoolDelegateUsesMockPool() {

		Pool pool = DelegatingPoolAdapter.from(this.mockPool);

		assertThat(pool.isDestroyed(), is(equalTo(false)));
		assertThat(pool.getFreeConnectionTimeout(), is(equalTo(10000)));
		assertThat(pool.getIdleTimeout(), is(equalTo(120000L)));
		assertThat(pool.getLoadConditioningInterval(), is(equalTo(300000)));
		assertThat(pool.getMaxConnections(), is(equalTo(500)));
		assertThat(pool.getMinConnections(), is(equalTo(50)));
		assertThat(pool.getMultiuserAuthentication(), is(equalTo(true)));
		assertThat(pool.getLocators(), is(equalTo(Collections.singletonList(newSocketAddress("skullbox", 11235)))));
		assertThat(pool.getName(), is(equalTo("MockPool")));
		assertThat(pool.getOnlineLocators(), is(equalTo(Collections.singletonList(newSocketAddress("trinity", 10101)))));
		assertThat(pool.getPendingEventCount(), is(equalTo(2)));
		assertThat(pool.getPingInterval(), is(equalTo(15000L)));
		assertThat(pool.getPRSingleHopEnabled(), is(equalTo(true)));
		assertThat(pool.getQueryService(), is(equalTo(this.mockQueryService)));
		assertThat(pool.getReadTimeout(), is(equalTo(30000)));
		assertThat(pool.getRetryAttempts(), is(equalTo(1)));
		assertThat(pool.getServerGroup(), is(equalTo("TestGroup")));
		assertThat(pool.getServers(), is(equalTo(Collections.singletonList(newSocketAddress("xghost", 12480)))));
		assertThat(pool.getSocketBufferSize(), is(equalTo(16384)));
		assertThat(pool.getSocketConnectTimeout(), is(equalTo(5000)));
		assertThat(pool.getStatisticInterval(), is(equalTo(1000)));
		assertThat(pool.getSubscriptionAckInterval(), is(equalTo(200)));
		assertThat(pool.getSubscriptionEnabled(), is(equalTo(true)));
		assertThat(pool.getSubscriptionMessageTrackingTimeout(), is(equalTo(60000)));
		assertThat(pool.getSubscriptionRedundancy(), is(equalTo(2)));
		assertThat(pool.getSubscriptionTimeoutMultiplier(), is(equalTo(3)));
		assertThat(pool.getThreadLocalConnections(), is(equalTo(false)));

		verify(this.mockPool, times(1)).isDestroyed();
		verify(this.mockPool, times(1)).getFreeConnectionTimeout();
		verify(this.mockPool, times(1)).getIdleTimeout();
		verify(this.mockPool, times(1)).getLoadConditioningInterval();
		verify(this.mockPool, times(1)).getLocators();
		verify(this.mockPool, times(1)).getMaxConnections();
		verify(this.mockPool, times(1)).getMinConnections();
		verify(this.mockPool, times(1)).getMultiuserAuthentication();
		verify(this.mockPool, times(1)).getName();
		verify(this.mockPool, times(1)).getPendingEventCount();
		verify(this.mockPool, times(1)).getPingInterval();
		verify(this.mockPool, times(1)).getPRSingleHopEnabled();
		verify(this.mockPool, times(1)).getQueryService();
		verify(this.mockPool, times(1)).getReadTimeout();
		verify(this.mockPool, times(1)).getRetryAttempts();
		verify(this.mockPool, times(1)).getServerGroup();
		verify(this.mockPool, times(1)).getServers();
		verify(this.mockPool, times(1)).getSocketBufferSize();
		verify(this.mockPool, times(1)).getSocketConnectTimeout();
		verify(this.mockPool, times(1)).getStatisticInterval();
		verify(this.mockPool, times(1)).getSubscriptionAckInterval();
		verify(this.mockPool, times(1)).getSubscriptionEnabled();
		verify(this.mockPool, times(1)).getSubscriptionMessageTrackingTimeout();
		verify(this.mockPool, times(1)).getSubscriptionRedundancy();
		verify(this.mockPool, times(1)).getSubscriptionTimeoutMultiplier();
		verify(this.mockPool, times(1)).getThreadLocalConnections();
	}

	@Test
	public void destroyWithDelegateCallsDestroy() {
		DelegatingPoolAdapter.from(this.mockPool).destroy();
		verify(this.mockPool, times(1)).destroy();
	}

	@Test
	public void destroyWithKeepAliveUsingDelegateCallsDestroy() {
		DelegatingPoolAdapter.from(this.mockPool).destroy(true);
		verify(this.mockPool, times(1)).destroy(eq(true));
	}

	@Test
	public void releaseThreadLocalConnectionWithDelegateCallsReleaseThreadLocalConnection() {
		DelegatingPoolAdapter.from(this.mockPool).releaseThreadLocalConnection();
		verify(this.mockPool, times(1)).releaseThreadLocalConnection();
	}

	@Test
	public void nullDelegateUsesDefaultFactorySettings() {

		Pool pool = DelegatingPoolAdapter.from(null);

		assertThat(pool.getFreeConnectionTimeout(), is(equalTo(PoolFactory.DEFAULT_FREE_CONNECTION_TIMEOUT)));
		assertThat(pool.getIdleTimeout(), is(equalTo(PoolFactory.DEFAULT_IDLE_TIMEOUT)));
		assertThat(pool.getLoadConditioningInterval(), is(equalTo(PoolFactory.DEFAULT_LOAD_CONDITIONING_INTERVAL)));
		assertThat(pool.getMaxConnections(), is(equalTo(PoolFactory.DEFAULT_MAX_CONNECTIONS)));
		assertThat(pool.getMinConnections(), is(equalTo(PoolFactory.DEFAULT_MIN_CONNECTIONS)));
		assertThat(pool.getMultiuserAuthentication(), is(equalTo(PoolFactory.DEFAULT_MULTIUSER_AUTHENTICATION)));
		assertThat(pool.getOnlineLocators(), is(equalTo(Collections.EMPTY_LIST)));
		assertThat(pool.getPingInterval(), is(equalTo(PoolFactory.DEFAULT_PING_INTERVAL)));
		assertThat(pool.getPRSingleHopEnabled(), is(equalTo(PoolFactory.DEFAULT_PR_SINGLE_HOP_ENABLED)));
		assertThat(pool.getReadTimeout(), is(equalTo(PoolFactory.DEFAULT_READ_TIMEOUT)));
		assertThat(pool.getRetryAttempts(), is(equalTo(PoolFactory.DEFAULT_RETRY_ATTEMPTS)));
		assertThat(pool.getServerGroup(), is(equalTo(PoolFactory.DEFAULT_SERVER_GROUP)));
		assertThat(pool.getSocketBufferSize(), is(equalTo(PoolFactory.DEFAULT_SOCKET_BUFFER_SIZE)));
		assertThat(pool.getSocketConnectTimeout(), is(equalTo(PoolFactory.DEFAULT_SOCKET_CONNECT_TIMEOUT)));
		assertThat(pool.getStatisticInterval(), is(equalTo(PoolFactory.DEFAULT_STATISTIC_INTERVAL)));
		assertThat(pool.getSubscriptionAckInterval(), is(equalTo(PoolFactory.DEFAULT_SUBSCRIPTION_ACK_INTERVAL)));
		assertThat(pool.getSubscriptionEnabled(), is(equalTo(PoolFactory.DEFAULT_SUBSCRIPTION_ENABLED)));
		assertThat(pool.getSubscriptionMessageTrackingTimeout(), is(equalTo(PoolFactory.DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT)));
		assertThat(pool.getSubscriptionRedundancy(), is(equalTo(PoolFactory.DEFAULT_SUBSCRIPTION_REDUNDANCY)));
		assertThat(pool.getSubscriptionTimeoutMultiplier(), is(equalTo(PoolFactory.DEFAULT_SUBSCRIPTION_TIMEOUT_MULTIPLIER)));
		assertThat(pool.getThreadLocalConnections(), is(equalTo(PoolFactory.DEFAULT_THREAD_LOCAL_CONNECTIONS)));

		verifyZeroInteractions(this.mockPool);
	}

	@Test
	public void destroyedWithNullIsUnsupported() {

		exception.expect(UnsupportedOperationException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage(is(equalTo(DelegatingPoolAdapter.NOT_IMPLEMENTED)));

		DelegatingPoolAdapter.from(null).isDestroyed();
	}

	@Test
	public void locatorsWithNullIsEqualToEmptyList() {
		assertThat(DelegatingPoolAdapter.from(null).getLocators(), is(equalTo(Collections.<InetSocketAddress>emptyList())));
	}

	@Test
	public void nameWithNullIsEqualToDefault() {
		assertThat(DelegatingPoolAdapter.from(null).getName(), is(equalTo(DelegatingPoolAdapter.DEFAULT_POOL_NAME)));
	}

	@Test
	public void pendingEventCountWithNullIsEqualToZero() {
		assertThat(DelegatingPoolAdapter.from(null).getPendingEventCount(), is(equalTo(0)));
	}

	@Test
	public void queryServiceWithNullIsNull() {
		assertThat(DelegatingPoolAdapter.from(null).getQueryService(), is(nullValue()));
	}

	@Test
	public void serversWithNullIsEqualToLocalhostListeningOnDefaultCacheServerPort() {
		assertThat(DelegatingPoolAdapter.from(null).getServers(), is(equalTo(Collections.singletonList(
			newSocketAddress("localhost", GemfireUtils.DEFAULT_CACHE_SERVER_PORT)))));
	}

	@Test
	public void destroyWithNullIsNoOp() {
		DelegatingPoolAdapter.from(null).destroy();
		verify(this.mockPool, never()).destroy();
	}

	@Test
	public void destroyWithKeepAliveUsingNullIsNoOp() {
		DelegatingPoolAdapter.from(null).destroy(false);
		verify(this.mockPool, never()).destroy(anyBoolean());
	}

	@Test
	public void releaseThreadLocalConnectionWithNullIsNoOp() {
		DelegatingPoolAdapter.from(null).releaseThreadLocalConnection();
		verify(this.mockPool, never()).releaseThreadLocalConnection();
	}
}
