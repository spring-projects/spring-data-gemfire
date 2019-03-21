/*
 * Copyright 2012-2019 the original author or authors.
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
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.net.InetSocketAddress;
import java.util.Collections;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;
import org.springframework.data.gemfire.GemfireUtils;

import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolFactory;
import com.gemstone.gemfire.cache.query.QueryService;

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
 * @see com.gemstone.gemfire.cache.client.Pool
 * @see com.gemstone.gemfire.cache.client.PoolFactory
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

	protected InetSocketAddress newSocketAddress(String host, int port) {
		return new InetSocketAddress(host, port);
	}

	@Before
	public void setup() {
		when(mockPool.isDestroyed()).thenReturn(false);
		when(mockPool.getFreeConnectionTimeout()).thenReturn(10000);
		when(mockPool.getIdleTimeout()).thenReturn(120000l);
		when(mockPool.getLoadConditioningInterval()).thenReturn(300000);
		when(mockPool.getLocators()).thenReturn(Collections.singletonList(newSocketAddress("skullbox", 11235)));
		when(mockPool.getMaxConnections()).thenReturn(500);
		when(mockPool.getMinConnections()).thenReturn(50);
		when(mockPool.getMultiuserAuthentication()).thenReturn(true);
		when(mockPool.getName()).thenReturn("MockPool");
		when(mockPool.getPendingEventCount()).thenReturn(2);
		when(mockPool.getPingInterval()).thenReturn(15000l);
		when(mockPool.getPRSingleHopEnabled()).thenReturn(true);
		when(mockPool.getQueryService()).thenReturn(mockQueryService);
		when(mockPool.getReadTimeout()).thenReturn(30000);
		when(mockPool.getRetryAttempts()).thenReturn(1);
		when(mockPool.getServerGroup()).thenReturn("TestGroup");
		when(mockPool.getServers()).thenReturn(Collections.singletonList(newSocketAddress("xghost", 12480)));
		when(mockPool.getSocketBufferSize()).thenReturn(16384);
		when(mockPool.getStatisticInterval()).thenReturn(1000);
		when(mockPool.getSubscriptionAckInterval()).thenReturn(200);
		when(mockPool.getSubscriptionEnabled()).thenReturn(true);
		when(mockPool.getSubscriptionMessageTrackingTimeout()).thenReturn(60000);
		when(mockPool.getSubscriptionRedundancy()).thenReturn(2);
		when(mockPool.getThreadLocalConnections()).thenReturn(false);
	}

	@Test
	public void delegateEqualsMockPool() {
		assertThat(DelegatingPoolAdapter.from(mockPool).getDelegate(), is(equalTo(mockPool)));
	}

	@Test
	public void mockPoolDelegateUsesMockPool() {
		Pool pool = DelegatingPoolAdapter.from(mockPool);

		assertThat(pool.isDestroyed(), is(equalTo(false)));
		assertThat(pool.getFreeConnectionTimeout(), is(equalTo(10000)));
		assertThat(pool.getIdleTimeout(), is(equalTo(120000l)));
		assertThat(pool.getLoadConditioningInterval(), is(equalTo(300000)));
		assertThat(pool.getMaxConnections(), is(equalTo(500)));
		assertThat(pool.getMinConnections(), is(equalTo(50)));
		assertThat(pool.getMultiuserAuthentication(), is(equalTo(true)));
		assertThat(pool.getLocators(), is(equalTo(Collections.singletonList(newSocketAddress("skullbox", 11235)))));
		assertThat(pool.getName(), is(equalTo("MockPool")));
		assertThat(pool.getPendingEventCount(), is(equalTo(2)));
		assertThat(pool.getPingInterval(), is(equalTo(15000l)));
		assertThat(pool.getPRSingleHopEnabled(), is(equalTo(true)));
		assertThat(pool.getQueryService(), is(equalTo(mockQueryService)));
		assertThat(pool.getReadTimeout(), is(equalTo(30000)));
		assertThat(pool.getRetryAttempts(), is(equalTo(1)));
		assertThat(pool.getServerGroup(), is(equalTo("TestGroup")));
		assertThat(pool.getServers(), is(equalTo(Collections.singletonList(newSocketAddress("xghost", 12480)))));
		assertThat(pool.getSocketBufferSize(), is(equalTo(16384)));
		assertThat(pool.getStatisticInterval(), is(equalTo(1000)));
		assertThat(pool.getSubscriptionAckInterval(), is(equalTo(200)));
		assertThat(pool.getSubscriptionEnabled(), is(equalTo(true)));
		assertThat(pool.getSubscriptionMessageTrackingTimeout(), is(equalTo(60000)));
		assertThat(pool.getSubscriptionRedundancy(), is(equalTo(2)));
		assertThat(pool.getThreadLocalConnections(), is(equalTo(false)));

		verify(mockPool, times(1)).isDestroyed();
		verify(mockPool, times(1)).getFreeConnectionTimeout();
		verify(mockPool, times(1)).getIdleTimeout();
		verify(mockPool, times(1)).getLoadConditioningInterval();
		verify(mockPool, times(1)).getLocators();
		verify(mockPool, times(1)).getMaxConnections();
		verify(mockPool, times(1)).getMinConnections();
		verify(mockPool, times(1)).getMultiuserAuthentication();
		verify(mockPool, times(1)).getName();
		verify(mockPool, times(1)).getPendingEventCount();
		verify(mockPool, times(1)).getPingInterval();
		verify(mockPool, times(1)).getPRSingleHopEnabled();
		verify(mockPool, times(1)).getQueryService();
		verify(mockPool, times(1)).getReadTimeout();
		verify(mockPool, times(1)).getRetryAttempts();
		verify(mockPool, times(1)).getServerGroup();
		verify(mockPool, times(1)).getServers();
		verify(mockPool, times(1)).getSocketBufferSize();
		verify(mockPool, times(1)).getStatisticInterval();
		verify(mockPool, times(1)).getSubscriptionAckInterval();
		verify(mockPool, times(1)).getSubscriptionEnabled();
		verify(mockPool, times(1)).getSubscriptionMessageTrackingTimeout();
		verify(mockPool, times(1)).getSubscriptionRedundancy();
		verify(mockPool, times(1)).getThreadLocalConnections();
	}

	@Test
	public void destroyWithDelegateCallsDestroy() {
		DelegatingPoolAdapter.from(mockPool).destroy();
		verify(mockPool, times(1)).destroy();
	}

	@Test
	public void destroyWithKeepAliveUsingDelegateCallsDestroy() {
		DelegatingPoolAdapter.from(mockPool).destroy(true);
		verify(mockPool, times(1)).destroy(eq(true));
	}

	@Test
	public void releaseThreadLocalConnectionWithDelegateCallsReleaseThreadLocalConnection() {
		DelegatingPoolAdapter.from(mockPool).releaseThreadLocalConnection();
		verify(mockPool, times(1)).releaseThreadLocalConnection();
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
		assertThat(pool.getPingInterval(), is(equalTo(PoolFactory.DEFAULT_PING_INTERVAL)));
		assertThat(pool.getPRSingleHopEnabled(), is(equalTo(PoolFactory.DEFAULT_PR_SINGLE_HOP_ENABLED)));
		assertThat(pool.getReadTimeout(), is(equalTo(PoolFactory.DEFAULT_READ_TIMEOUT)));
		assertThat(pool.getRetryAttempts(), is(equalTo(PoolFactory.DEFAULT_RETRY_ATTEMPTS)));
		assertThat(pool.getServerGroup(), is(equalTo(PoolFactory.DEFAULT_SERVER_GROUP)));
		assertThat(pool.getSocketBufferSize(), is(equalTo(PoolFactory.DEFAULT_SOCKET_BUFFER_SIZE)));
		assertThat(pool.getStatisticInterval(), is(equalTo(PoolFactory.DEFAULT_STATISTIC_INTERVAL)));
		assertThat(pool.getSubscriptionAckInterval(), is(equalTo(PoolFactory.DEFAULT_SUBSCRIPTION_ACK_INTERVAL)));
		assertThat(pool.getSubscriptionEnabled(), is(equalTo(PoolFactory.DEFAULT_SUBSCRIPTION_ENABLED)));
		assertThat(pool.getSubscriptionMessageTrackingTimeout(), is(equalTo(PoolFactory.DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT)));
		assertThat(pool.getSubscriptionRedundancy(), is(equalTo(PoolFactory.DEFAULT_SUBSCRIPTION_REDUNDANCY)));
		assertThat(pool.getThreadLocalConnections(), is(equalTo(PoolFactory.DEFAULT_THREAD_LOCAL_CONNECTIONS)));

		verifyZeroInteractions(mockPool);
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
		verify(mockPool, never()).destroy();
	}

	@Test
	public void destroyWithKeepAliveUsingNullIsNoOp() {
		DelegatingPoolAdapter.from(null).destroy(false);
		verify(mockPool, never()).destroy(anyBoolean());
	}

	@Test
	public void releaseThreadLocalConnectionWithNullIsNoOp() {
		DelegatingPoolAdapter.from(null).releaseThreadLocalConnection();
		verify(mockPool, never()).releaseThreadLocalConnection();
	}

}
