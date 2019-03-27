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
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.net.InetSocketAddress;
import java.util.Collections;
import java.util.List;
import java.util.function.Supplier;

import org.apache.geode.cache.client.Pool;
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
 * Additional unit tests for {@link DefaultableDelegatingPoolAdapter} testing defaults.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.client.support.DefaultableDelegatingPoolAdapter
 * @since 1.8.0
 */
@RunWith(MockitoJUnitRunner.class)
public class DefaultableDelegatingPoolAdapterTest {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	private DefaultableDelegatingPoolAdapter poolAdapter;

	@Mock
	private Pool mockPool;

	@Mock
	private QueryService mockQueryService;

	@Mock
	private Supplier mockSupplier;

	private static InetSocketAddress newSocketAddress(String host, int port) {
		return new InetSocketAddress(host, port);
	}

	@Before
	public void setupMockPool() {

		when(this.mockPool.getFreeConnectionTimeout()).thenReturn(5000);
		when(this.mockPool.getIdleTimeout()).thenReturn(120000L);
		when(this.mockPool.getLoadConditioningInterval()).thenReturn(300000);
		when(this.mockPool.getLocators()).thenReturn(Collections.emptyList());
		when(this.mockPool.getMaxConnections()).thenReturn(500);
		when(this.mockPool.getMinConnections()).thenReturn(50);
		when(this.mockPool.getMultiuserAuthentication()).thenReturn(true);
		when(this.mockPool.getName()).thenReturn("TestPool");
		when(this.mockPool.getPendingEventCount()).thenReturn(2);
		when(this.mockPool.getPingInterval()).thenReturn(15000L);
		when(this.mockPool.getPRSingleHopEnabled()).thenReturn(true);
		when(this.mockPool.getQueryService()).thenReturn(null);
		when(this.mockPool.getReadTimeout()).thenReturn(30000);
		when(this.mockPool.getRetryAttempts()).thenReturn(1);
		when(this.mockPool.getServerGroup()).thenReturn("TestGroup");
		when(this.mockPool.getServers()).thenReturn(Collections.singletonList(
			newSocketAddress("localhost", GemfireUtils.DEFAULT_CACHE_SERVER_PORT)));
		when(this.mockPool.getSocketBufferSize()).thenReturn(16384);
		when(this.mockPool.getSocketConnectTimeout()).thenReturn(5000);
		when(this.mockPool.getStatisticInterval()).thenReturn(1000);
		when(this.mockPool.getSubscriptionAckInterval()).thenReturn(200);
		when(this.mockPool.getSubscriptionEnabled()).thenReturn(true);
		when(this.mockPool.getSubscriptionMessageTrackingTimeout()).thenReturn(20000);
		when(this.mockPool.getSubscriptionRedundancy()).thenReturn(2);
		when(this.mockPool.getSubscriptionTimeoutMultiplier()).thenReturn(3);
		when(this.mockPool.getThreadLocalConnections()).thenReturn(false);

		setupPoolAdapter();
	}

	//@Before
	public void setupPoolAdapter() {
		this.poolAdapter = DefaultableDelegatingPoolAdapter.from(this.mockPool);
	}

	@Test
	public void fromMockPoolAsDelegate() {
		assertThat(this.poolAdapter.getDelegate(), is(sameInstance(this.mockPool)));
	}

	@Test
	public void fromNullAsDelegate() {

		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("Pool delegate must not be null");

		DefaultableDelegatingPoolAdapter.from(null);
	}

	@Test
	public void prefersDefaultsToPoolDelegateAndIsMutable() {

		assertThat(this.poolAdapter.getPreference(), is(equalTo(DefaultableDelegatingPoolAdapter.Preference.PREFER_POOL)));
		assertThat(this.poolAdapter.prefersPool(), is(true));

		this.poolAdapter.preferDefault();

		assertThat(this.poolAdapter.getPreference(), is(equalTo(DefaultableDelegatingPoolAdapter.Preference.PREFER_DEFAULT)));
		assertThat(this.poolAdapter.prefersDefault(), is(true));

		this.poolAdapter.preferPool();

		assertThat(this.poolAdapter.getPreference(), is(equalTo(DefaultableDelegatingPoolAdapter.Preference.PREFER_POOL)));
		assertThat(this.poolAdapter.prefersPool(), is(true));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfNullWhenPrefersDefaultUsesDefault() {

		this.poolAdapter = this.poolAdapter.preferDefault();

		assertThat(this.poolAdapter.prefersDefault(), is(true));
		assertThat(this.poolAdapter.defaultIfNull("default", this.mockSupplier),
			is(equalTo("default")));

		verifyZeroInteractions(this.mockSupplier);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfNullWhenPrefersDefaultUsesPoolValueWhenDefaultIsNull() {

		this.poolAdapter = this.poolAdapter.preferDefault();

		when(this.mockSupplier.get()).thenReturn("pool");

		assertThat(this.poolAdapter.prefersDefault(), is(true));
		assertThat(this.poolAdapter.defaultIfNull(null, this.mockSupplier),
			is(equalTo("pool")));

		verify(this.mockSupplier, times(1)).get();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfNullWhenPrefersPoolUsesPoolValue() {

		this.poolAdapter = this.poolAdapter.preferPool();

		when(mockSupplier.get()).thenReturn("pool");

		assertThat(this.poolAdapter.prefersPool(), is(true));
		assertThat(this.poolAdapter.defaultIfNull("default", this.mockSupplier),
			is(equalTo("pool")));

		verify(this.mockSupplier, times(1)).get();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfNullWhenPrefersPoolUsesDefaultWhenPoolValueIsNull() {

		this.poolAdapter = this.poolAdapter.preferPool();

		when(this.mockSupplier.get()).thenReturn(null);

		assertThat(this.poolAdapter.prefersPool(), is(true));
		assertThat(this.poolAdapter.defaultIfNull("default", this.mockSupplier),
			is(equalTo("default")));

		verify(this.mockSupplier, times(1)).get();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfEmptyWhenPrefersDefaultUsesDefault() {

		this.poolAdapter = this.poolAdapter.preferDefault();

		List<Object> defaultList = Collections.singletonList("default");

		assertThat(this.poolAdapter.prefersDefault(), is(true));
		assertThat((List<Object>) this.poolAdapter.defaultIfEmpty(defaultList, this.mockSupplier), is(equalTo(defaultList)));

		verifyZeroInteractions(this.mockSupplier);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfEmptyWhenPrefersDefaultUsesPoolValueWhenDefaultIsNull() {

		this.poolAdapter = this.poolAdapter.preferDefault();

		List<Object> poolList = Collections.singletonList("pool");

		when(this.mockSupplier.get()).thenReturn(poolList);

		assertThat(this.poolAdapter.prefersDefault(), is(true));
		assertThat((List<Object>) this.poolAdapter.defaultIfEmpty(null, this.mockSupplier), is(equalTo(poolList)));

		verify(this.mockSupplier, times(1)).get();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfEmptyWhenPrefersDefaultUsesPoolValueWhenDefaultIsEmpty() {

		this.poolAdapter = this.poolAdapter.preferDefault();

		List<Object> poolList = Collections.singletonList("pool");

		when(this.mockSupplier.get()).thenReturn(poolList);

		assertThat(this.poolAdapter.prefersDefault(), is(true));
		assertThat((List<Object>) this.poolAdapter.defaultIfEmpty(Collections.emptyList(), this.mockSupplier),
			is(equalTo(poolList)));

		verify(this.mockSupplier, times(1)).get();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfEmptyWhenPrefersPoolUsesPoolValue() {

		this.poolAdapter = this.poolAdapter.preferPool();

		List<Object> poolList = Collections.singletonList("pool");

		when(this.mockSupplier.get()).thenReturn(poolList);

		assertThat(this.poolAdapter.prefersPool(), is(true));
		assertThat((List<Object>) this.poolAdapter.defaultIfEmpty(Collections.<Object>singletonList("default"), this.mockSupplier),
			is(equalTo(poolList)));

		verify(this.mockSupplier, times(1)).get();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfEmptyWhenPrefersPoolUsesDefaultWhenPoolValueIsNull() {

		this.poolAdapter = this.poolAdapter.preferPool();

		when(this.mockSupplier.get()).thenReturn(null);

		List<Object> defaultList = Collections.singletonList("default");

		assertThat(this.poolAdapter.prefersPool(), is(true));
		assertThat((List<Object>) this.poolAdapter.defaultIfEmpty(defaultList, this.mockSupplier),
			is(equalTo(defaultList)));

		verify(this.mockSupplier, times(1)).get();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfEmptyWhenPrefersPoolUsesDefaultWhenPoolValueIsEmpty() {

		assertThat(this.poolAdapter.preferPool(), is(sameInstance(this.poolAdapter)));

		when(this.mockSupplier.get()).thenReturn(Collections.emptyList());

		List<Object> defaultList = Collections.singletonList("default");

		assertThat(this.poolAdapter.prefersPool(), is(true));
		assertThat((List<Object>) this.poolAdapter.defaultIfEmpty(defaultList, this.mockSupplier),
			is(equalTo(defaultList)));

		verify(this.mockSupplier, times(1)).get();
	}

	@Test
	public void poolAdapterPreferringDefaultsUsesNonNullDefaults() {

		assertThat(this.poolAdapter.preferDefault(), is(sameInstance(this.poolAdapter)));

		List<InetSocketAddress> defaultLocator = Collections.singletonList(newSocketAddress("boombox", 21668));
		List<InetSocketAddress> defaultServer = Collections.singletonList(newSocketAddress("skullbox", 42424));

		assertThat(this.poolAdapter.getDelegate(), is(equalTo(this.mockPool)));
		assertThat(this.poolAdapter.prefersDefault(), is(true));
		assertThat(this.poolAdapter.getFreeConnectionTimeout(10000), is(equalTo(10000)));
		assertThat(this.poolAdapter.getIdleTimeout(300000L), is(equalTo(300000L)));
		assertThat(this.poolAdapter.getLoadConditioningInterval(60000), is(equalTo(60000)));
		assertThat(this.poolAdapter.getLocators(defaultLocator), is(equalTo(defaultLocator)));
		assertThat(this.poolAdapter.getMaxConnections(100), is(equalTo(100)));
		assertThat(this.poolAdapter.getMinConnections(10), is(equalTo(10)));
		assertThat(this.poolAdapter.getMultiuserAuthentication(false), is(equalTo(false)));
		assertThat(this.poolAdapter.getName(), is(equalTo("TestPool")));
		assertThat(this.poolAdapter.getPendingEventCount(), is(equalTo(2)));
		assertThat(this.poolAdapter.getPingInterval(20000L), is(equalTo(20000L)));
		assertThat(this.poolAdapter.getPRSingleHopEnabled(false), is(equalTo(false)));
		assertThat(this.poolAdapter.getQueryService(this.mockQueryService), is(equalTo(this.mockQueryService)));
		assertThat(this.poolAdapter.getReadTimeout(20000), is(equalTo(20000)));
		assertThat(this.poolAdapter.getRetryAttempts(2), is(equalTo(2)));
		assertThat(this.poolAdapter.getServerGroup("MockGroup"), is(equalTo("MockGroup")));
		assertThat(this.poolAdapter.getServers(defaultServer), is(equalTo(defaultServer)));
		assertThat(this.poolAdapter.getSocketBufferSize(8192), is(equalTo(8192)));
		assertThat(this.poolAdapter.getSocketConnectTimeout(10000), is(equalTo(10000)));
		assertThat(this.poolAdapter.getStatisticInterval(2000), is(equalTo(2000)));
		assertThat(this.poolAdapter.getSubscriptionAckInterval(50), is(equalTo(50)));
		assertThat(this.poolAdapter.getSubscriptionEnabled(false), is(equalTo(false)));
		assertThat(this.poolAdapter.getSubscriptionMessageTrackingTimeout(15000), is(equalTo(15000)));
		assertThat(this.poolAdapter.getSubscriptionRedundancy(1), is(equalTo(1)));
		assertThat(this.poolAdapter.getSubscriptionTimeoutMultiplier(1), is(equalTo(1)));
		assertThat(this.poolAdapter.getThreadLocalConnections(true), is(equalTo(true)));

		verify(this.mockPool, times(1)).getName();
		verify(this.mockPool, times(1)).getPendingEventCount();
		verifyNoMoreInteractions(this.mockPool);
	}

	@Test
	public void poolAdapterPreferringDefaultsUsesPoolValuesWhenSomeDefaultValuesAreNull() {

		assertThat(this.poolAdapter.preferDefault(), is(sameInstance(this.poolAdapter)));

		List<InetSocketAddress> defaultLocator = Collections.singletonList(newSocketAddress("boombox", 21668));
		List<InetSocketAddress> poolServer = Collections.singletonList(newSocketAddress("localhost", 40404));

		assertThat(this.poolAdapter.getDelegate(), is(equalTo(this.mockPool)));
		assertThat(this.poolAdapter.prefersDefault(), is(true));
		assertThat(this.poolAdapter.getFreeConnectionTimeout(null), is(equalTo(5000)));
		assertThat(this.poolAdapter.getIdleTimeout(null), is(equalTo(120000L)));
		assertThat(this.poolAdapter.getLoadConditioningInterval(60000), is(equalTo(60000)));
		assertThat(this.poolAdapter.getLocators(defaultLocator), is(equalTo(defaultLocator)));
		assertThat(this.poolAdapter.getMaxConnections(null), is(equalTo(500)));
		assertThat(this.poolAdapter.getMinConnections(50), is(equalTo(50)));
		assertThat(this.poolAdapter.getMultiuserAuthentication(null), is(equalTo(true)));
		assertThat(this.poolAdapter.getName(), is(equalTo("TestPool")));
		assertThat(this.poolAdapter.getPendingEventCount(), is(equalTo(2)));
		assertThat(this.poolAdapter.getPingInterval(null), is(equalTo(15000L)));
		assertThat(this.poolAdapter.getPRSingleHopEnabled(true), is(equalTo(true)));
		assertThat(this.poolAdapter.getQueryService(null), is(nullValue()));
		assertThat(this.poolAdapter.getReadTimeout(20000), is(equalTo(20000)));
		assertThat(this.poolAdapter.getRetryAttempts(null), is(equalTo(1)));
		assertThat(this.poolAdapter.getServerGroup("MockGroup"), is(equalTo("MockGroup")));
		assertThat(this.poolAdapter.getServers(null), is(equalTo(poolServer)));
		assertThat(this.poolAdapter.getSocketBufferSize(32768), is(equalTo(32768)));
		assertThat(this.poolAdapter.getSocketConnectTimeout(null), is(equalTo(5000)));
		assertThat(this.poolAdapter.getStatisticInterval(null), is(equalTo(1000)));
		assertThat(this.poolAdapter.getSubscriptionAckInterval(50), is(equalTo(50)));
		assertThat(this.poolAdapter.getSubscriptionEnabled(true), is(equalTo(true)));
		assertThat(this.poolAdapter.getSubscriptionMessageTrackingTimeout(null), is(equalTo(20000)));
		assertThat(this.poolAdapter.getSubscriptionRedundancy(1), is(equalTo(1)));
		assertThat(this.poolAdapter.getSubscriptionTimeoutMultiplier(null), is(equalTo(3)));
		assertThat(this.poolAdapter.getThreadLocalConnections(null), is(equalTo(false)));

		verify(this.mockPool, times(1)).getFreeConnectionTimeout();
		verify(this.mockPool, times(1)).getIdleTimeout();
		verify(this.mockPool, times(1)).getMaxConnections();
		verify(this.mockPool, times(1)).getMultiuserAuthentication();
		verify(this.mockPool, times(1)).getName();
		verify(this.mockPool, times(1)).getPendingEventCount();
		verify(this.mockPool, times(1)).getPingInterval();
		verify(this.mockPool, times(1)).getQueryService();
		verify(this.mockPool, times(1)).getRetryAttempts();
		verify(this.mockPool, times(1)).getServers();
		verify(this.mockPool, times(1)).getSocketConnectTimeout();
		verify(this.mockPool, times(1)).getStatisticInterval();
		verify(this.mockPool, times(1)).getSubscriptionMessageTrackingTimeout();
		verify(this.mockPool, times(1)).getSubscriptionTimeoutMultiplier();
		verify(this.mockPool, times(1)).getThreadLocalConnections();
		verifyNoMoreInteractions(this.mockPool);
	}

	@Test
	public void poolAdapterPreferringDefaultsUsesPoolValuesExclusivelyWhenAllDefaultValuesAreNull() {

		assertThat(this.poolAdapter.preferDefault(), is(sameInstance(this.poolAdapter)));

		List<InetSocketAddress> poolServer = Collections.singletonList(newSocketAddress("localhost", 40404));

		assertThat(this.poolAdapter.getDelegate(), is(equalTo(this.mockPool)));
		assertThat(this.poolAdapter.prefersDefault(), is(true));
		assertThat(this.poolAdapter.getFreeConnectionTimeout(null), is(equalTo(5000)));
		assertThat(this.poolAdapter.getIdleTimeout(null), is(equalTo(120000L)));
		assertThat(this.poolAdapter.getLoadConditioningInterval(null), is(equalTo(300000)));
		assertThat(this.poolAdapter.getLocators(null), is(equalTo(Collections.<InetSocketAddress>emptyList())));
		assertThat(this.poolAdapter.getMaxConnections(null), is(equalTo(500)));
		assertThat(this.poolAdapter.getMinConnections(null), is(equalTo(50)));
		assertThat(this.poolAdapter.getMultiuserAuthentication(null), is(equalTo(true)));
		assertThat(this.poolAdapter.getName(), is(equalTo("TestPool")));
		assertThat(this.poolAdapter.getPendingEventCount(), is(equalTo(2)));
		assertThat(this.poolAdapter.getPingInterval(null), is(equalTo(15000L)));
		assertThat(this.poolAdapter.getPRSingleHopEnabled(null), is(equalTo(true)));
		assertThat(this.poolAdapter.getQueryService(null), is(nullValue()));
		assertThat(this.poolAdapter.getReadTimeout(null), is(equalTo(30000)));
		assertThat(this.poolAdapter.getRetryAttempts(null), is(equalTo(1)));
		assertThat(this.poolAdapter.getServerGroup(null), is(equalTo("TestGroup")));
		assertThat(this.poolAdapter.getServers(null), is(equalTo(poolServer)));
		assertThat(this.poolAdapter.getSocketBufferSize(null), is(equalTo(16384)));
		assertThat(this.poolAdapter.getSocketConnectTimeout(null), is(equalTo(5000)));
		assertThat(this.poolAdapter.getStatisticInterval(null), is(equalTo(1000)));
		assertThat(this.poolAdapter.getSubscriptionAckInterval(null), is(equalTo(200)));
		assertThat(this.poolAdapter.getSubscriptionEnabled(null), is(equalTo(true)));
		assertThat(this.poolAdapter.getSubscriptionMessageTrackingTimeout(null), is(equalTo(20000)));
		assertThat(this.poolAdapter.getSubscriptionRedundancy(null), is(equalTo(2)));
		assertThat(this.poolAdapter.getSubscriptionTimeoutMultiplier(null), is(equalTo(3)));
		assertThat(this.poolAdapter.getThreadLocalConnections(null), is(equalTo(false)));

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
		verifyNoMoreInteractions(this.mockPool);
	}

	@Test
	public void poolAdapterPreferringPoolUsesUseNonNullPoolValues() {

		assertThat(this.poolAdapter.preferPool(), is(sameInstance(this.poolAdapter)));

		List<InetSocketAddress> defaultServer = Collections.singletonList(newSocketAddress("jambox", 12480));
		List<InetSocketAddress> poolServer = Collections.singletonList(newSocketAddress("localhost", 40404));

		assertThat(this.poolAdapter.getFreeConnectionTimeout(15000), is(equalTo(5000)));
		assertThat(this.poolAdapter.getIdleTimeout(60000L), is(equalTo(120000L)));
		assertThat(this.poolAdapter.getLoadConditioningInterval(180000), is(equalTo(300000)));
		assertThat(this.poolAdapter.getLocators(Collections.emptyList()), is(equalTo(Collections.<InetSocketAddress>emptyList())));
		assertThat(this.poolAdapter.getMaxConnections(999), is(equalTo(500)));
		assertThat(this.poolAdapter.getMinConnections(99), is(equalTo(50)));
		assertThat(this.poolAdapter.getMultiuserAuthentication(false), is(equalTo(true)));
		assertThat(this.poolAdapter.getName(), is(equalTo("TestPool")));
		assertThat(this.poolAdapter.getPendingEventCount(), is(equalTo(2)));
		assertThat(this.poolAdapter.getPingInterval(20000L), is(equalTo(15000L)));
		assertThat(this.poolAdapter.getPRSingleHopEnabled(false), is(equalTo(true)));
		assertThat(this.poolAdapter.getQueryService(null), is(nullValue()));
		assertThat(this.poolAdapter.getReadTimeout(20000), is(equalTo(30000)));
		assertThat(this.poolAdapter.getRetryAttempts(4), is(equalTo(1)));
		assertThat(this.poolAdapter.getServerGroup("MockGroup"), is(equalTo("TestGroup")));
		assertThat(this.poolAdapter.getServers(defaultServer), is(equalTo(poolServer)));
		assertThat(this.poolAdapter.getSocketBufferSize(8192), is(equalTo(16384)));
		assertThat(this.poolAdapter.getSocketConnectTimeout(8192), is(equalTo(5000)));
		assertThat(this.poolAdapter.getStatisticInterval(2000), is(equalTo(1000)));
		assertThat(this.poolAdapter.getSubscriptionAckInterval(50), is(equalTo(200)));
		assertThat(this.poolAdapter.getSubscriptionEnabled(false), is(equalTo(true)));
		assertThat(this.poolAdapter.getSubscriptionMessageTrackingTimeout(30000), is(equalTo(20000)));
		assertThat(this.poolAdapter.getSubscriptionRedundancy(1), is(equalTo(2)));
		assertThat(this.poolAdapter.getSubscriptionTimeoutMultiplier(2), is(equalTo(3)));
		assertThat(this.poolAdapter.getThreadLocalConnections(true), is(equalTo(false)));

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
		verifyNoMoreInteractions(this.mockPool);
	}

	@Test
	public void poolAdapterDestroyUsesPoolRegardlessOfPreference() {

		assertThat(this.poolAdapter.preferDefault(), is(sameInstance(this.poolAdapter)));
		assertThat(this.poolAdapter.getDelegate(), is(equalTo(this.mockPool)));
		assertThat(this.poolAdapter.prefersDefault(), is(true));

		this.poolAdapter.destroy();

		assertThat(this.poolAdapter.preferPool(), is(sameInstance(this.poolAdapter)));
		assertThat(this.poolAdapter.getDelegate(), is(equalTo(this.mockPool)));
		assertThat(this.poolAdapter.prefersPool(), is(true));

		this.poolAdapter.destroy(true);

		verify(this.mockPool, times(1)).destroy();
		verify(this.mockPool, times(1)).destroy(anyBoolean());
	}

	@Test
	public void poolAdapterReleaseThreadLocalConnections() {

		assertThat(this.poolAdapter.preferDefault(), is(sameInstance(this.poolAdapter)));
		assertThat(this.poolAdapter.getDelegate(), is(equalTo(this.mockPool)));
		assertThat(this.poolAdapter.prefersDefault(), is(true));

		this.poolAdapter.releaseThreadLocalConnection();

		assertThat(this.poolAdapter.preferPool(), is(sameInstance(this.poolAdapter)));
		assertThat(this.poolAdapter.getDelegate(), is(equalTo(this.mockPool)));
		assertThat(this.poolAdapter.prefersPool(), is(true));

		this.poolAdapter.releaseThreadLocalConnection();

		verify(this.mockPool, times(2)).releaseThreadLocalConnection();
	}
}
