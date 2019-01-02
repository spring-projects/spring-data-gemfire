/*
 * Copyright 2012-2019 the original author or authors.
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
 * @see org.mockito.runners.MockitoJUnitRunner
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
	private DefaultableDelegatingPoolAdapter.ValueProvider mockValueProvider;

	private static InetSocketAddress newSocketAddress(String host, int port) {
		return new InetSocketAddress(host, port);
	}

	@Before
	public void setupMockPool() {
		when(mockPool.getFreeConnectionTimeout()).thenReturn(5000);
		when(mockPool.getIdleTimeout()).thenReturn(120000L);
		when(mockPool.getLoadConditioningInterval()).thenReturn(300000);
		when(mockPool.getLocators()).thenReturn(Collections.emptyList());
		when(mockPool.getMaxConnections()).thenReturn(500);
		when(mockPool.getMinConnections()).thenReturn(50);
		when(mockPool.getMultiuserAuthentication()).thenReturn(true);
		when(mockPool.getName()).thenReturn("TestPool");
		when(mockPool.getPendingEventCount()).thenReturn(2);
		when(mockPool.getPingInterval()).thenReturn(15000L);
		when(mockPool.getPRSingleHopEnabled()).thenReturn(true);
		when(mockPool.getQueryService()).thenReturn(null);
		when(mockPool.getReadTimeout()).thenReturn(30000);
		when(mockPool.getRetryAttempts()).thenReturn(1);
		when(mockPool.getServerGroup()).thenReturn("TestGroup");
		when(mockPool.getServers()).thenReturn(Collections.singletonList(
			newSocketAddress("localhost", GemfireUtils.DEFAULT_CACHE_SERVER_PORT)));
		when(mockPool.getSocketBufferSize()).thenReturn(16384);
		when(mockPool.getStatisticInterval()).thenReturn(1000);
		when(mockPool.getSubscriptionAckInterval()).thenReturn(200);
		when(mockPool.getSubscriptionEnabled()).thenReturn(true);
		when(mockPool.getSubscriptionMessageTrackingTimeout()).thenReturn(20000);
		when(mockPool.getSubscriptionRedundancy()).thenReturn(2);
		when(mockPool.getThreadLocalConnections()).thenReturn(false);

		setupPoolAdapter();
	}

	//@Before
	public void setupPoolAdapter() {
		poolAdapter = DefaultableDelegatingPoolAdapter.from(mockPool);
	}

	@Test
	public void fromMockPoolAsDelegate() {
		assertThat(poolAdapter.getDelegate(), is(sameInstance(mockPool)));
	}

	@Test
	public void fromNullAsDelegate() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("'delegate' must not be null");

		DefaultableDelegatingPoolAdapter.from(null);
	}

	@Test
	public void preferenceDefaultsToPoolDelegateAndIsMutable() {
		assertThat(poolAdapter.getPreference(), is(equalTo(DefaultableDelegatingPoolAdapter.Preference.PREFER_POOL)));
		assertThat(poolAdapter.prefersPool(), is(true));

		poolAdapter.preferDefault();

		assertThat(poolAdapter.getPreference(), is(equalTo(DefaultableDelegatingPoolAdapter.Preference.PREFER_DEFAULT)));
		assertThat(poolAdapter.prefersDefault(), is(true));

		poolAdapter.preferPool();

		assertThat(poolAdapter.getPreference(), is(equalTo(DefaultableDelegatingPoolAdapter.Preference.PREFER_POOL)));
		assertThat(poolAdapter.prefersPool(), is(true));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfNullWhenPrefersDefaultUsesDefault() {
		poolAdapter = poolAdapter.preferDefault();

		assertThat(poolAdapter.prefersDefault(), is(true));
		assertThat(poolAdapter.defaultIfNull("default", mockValueProvider), is(equalTo("default")));

		verifyZeroInteractions(mockValueProvider);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfNullWhenPrefersDefaultUsesPoolValueWhenDefaultIsNull() {
		poolAdapter = poolAdapter.preferDefault();

		when(mockValueProvider.getValue()).thenReturn("pool");

		assertThat(poolAdapter.prefersDefault(), is(true));
		assertThat(poolAdapter.defaultIfNull(null, mockValueProvider), is(equalTo("pool")));

		verify(mockValueProvider, times(1)).getValue();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfNullWhenPrefersPoolUsesPoolValue() {
		poolAdapter = poolAdapter.preferPool();

		when(mockValueProvider.getValue()).thenReturn("pool");

		assertThat(poolAdapter.prefersPool(), is(true));
		assertThat(poolAdapter.defaultIfNull("default", mockValueProvider), is(equalTo("pool")));

		verify(mockValueProvider, times(1)).getValue();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfNullWhenPrefersPoolUsesDefaultWhenPoolValueIsNull() {
		poolAdapter = poolAdapter.preferPool();

		when(mockValueProvider.getValue()).thenReturn(null);

		assertThat(poolAdapter.prefersPool(), is(true));
		assertThat(poolAdapter.defaultIfNull("default", mockValueProvider), is(equalTo("default")));

		verify(mockValueProvider, times(1)).getValue();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfEmptyWhenPrefersDefaultUsesDefault() {
		poolAdapter = poolAdapter.preferDefault();

		List<Object> defaultList = Collections.singletonList("default");

		assertThat(poolAdapter.prefersDefault(), is(true));
		assertThat((List<Object>) poolAdapter.defaultIfEmpty(defaultList, mockValueProvider), is(equalTo(defaultList)));

		verifyZeroInteractions(mockValueProvider);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfEmptyWhenPrefersDefaultUsesPoolValueWhenDefaultIsNull() {
		poolAdapter = poolAdapter.preferDefault();

		List<Object> poolList = Collections.singletonList("pool");

		when(mockValueProvider.getValue()).thenReturn(poolList);

		assertThat(poolAdapter.prefersDefault(), is(true));
		assertThat((List<Object>) poolAdapter.defaultIfEmpty(null, mockValueProvider), is(equalTo(poolList)));

		verify(mockValueProvider, times(1)).getValue();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfEmptyWhenPrefersDefaultUsesPoolValueWhenDefaultIsEmpty() {
		poolAdapter = poolAdapter.preferDefault();

		List<Object> poolList = Collections.singletonList("pool");

		when(mockValueProvider.getValue()).thenReturn(poolList);

		assertThat(poolAdapter.prefersDefault(), is(true));
		assertThat((List<Object>) poolAdapter.defaultIfEmpty(Collections.emptyList(), mockValueProvider),
			is(equalTo(poolList)));

		verify(mockValueProvider, times(1)).getValue();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfEmptyWhenPrefersPoolUsesPoolValue() {
		poolAdapter = poolAdapter.preferPool();

		List<Object> poolList = Collections.singletonList("pool");

		when(mockValueProvider.getValue()).thenReturn(poolList);

		assertThat(poolAdapter.prefersPool(), is(true));
		assertThat((List<Object>) poolAdapter.defaultIfEmpty(Collections.<Object>singletonList("default"), mockValueProvider),
			is(equalTo(poolList)));

		verify(mockValueProvider, times(1)).getValue();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfEmptyWhenPrefersPoolUsesDefaultWhenPoolValueIsNull() {
		poolAdapter = poolAdapter.preferPool();

		when(mockValueProvider.getValue()).thenReturn(null);

		List<Object> defaultList = Collections.singletonList("default");

		assertThat(poolAdapter.prefersPool(), is(true));
		assertThat((List<Object>) poolAdapter.defaultIfEmpty(defaultList, mockValueProvider),
			is(equalTo(defaultList)));

		verify(mockValueProvider, times(1)).getValue();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void defaultIfEmptyWhenPrefersPoolUsesDefaultWhenPoolValueIsEmpty() {
		assertThat(poolAdapter.preferPool(), is(sameInstance(poolAdapter)));

		when(mockValueProvider.getValue()).thenReturn(Collections.emptyList());

		List<Object> defaultList = Collections.singletonList("default");

		assertThat(poolAdapter.prefersPool(), is(true));
		assertThat((List<Object>) poolAdapter.defaultIfEmpty(defaultList, mockValueProvider),
			is(equalTo(defaultList)));

		verify(mockValueProvider, times(1)).getValue();
	}

	@Test
	public void poolAdapterPreferringDefaultsUsesNonNullDefaults() {
		assertThat(poolAdapter.preferDefault(), is(sameInstance(poolAdapter)));

		List<InetSocketAddress> defaultLocator = Collections.singletonList(newSocketAddress("boombox", 21668));
		List<InetSocketAddress> defaultServer = Collections.singletonList(newSocketAddress("skullbox", 42424));

		assertThat(poolAdapter.getDelegate(), is(equalTo(mockPool)));
		assertThat(poolAdapter.prefersDefault(), is(true));
		assertThat(poolAdapter.getFreeConnectionTimeout(10000), is(equalTo(10000)));
		assertThat(poolAdapter.getIdleTimeout(300000L), is(equalTo(300000L)));
		assertThat(poolAdapter.getLoadConditioningInterval(60000), is(equalTo(60000)));
		assertThat(poolAdapter.getLocators(defaultLocator), is(equalTo(defaultLocator)));
		assertThat(poolAdapter.getMaxConnections(100), is(equalTo(100)));
		assertThat(poolAdapter.getMinConnections(10), is(equalTo(10)));
		assertThat(poolAdapter.getMultiuserAuthentication(false), is(equalTo(false)));
		assertThat(poolAdapter.getName(), is(equalTo("TestPool")));
		assertThat(poolAdapter.getPendingEventCount(), is(equalTo(2)));
		assertThat(poolAdapter.getPingInterval(20000L), is(equalTo(20000L)));
		assertThat(poolAdapter.getPRSingleHopEnabled(false), is(equalTo(false)));
		assertThat(poolAdapter.getQueryService(mockQueryService), is(equalTo(mockQueryService)));
		assertThat(poolAdapter.getReadTimeout(20000), is(equalTo(20000)));
		assertThat(poolAdapter.getRetryAttempts(2), is(equalTo(2)));
		assertThat(poolAdapter.getServerGroup("MockGroup"), is(equalTo("MockGroup")));
		assertThat(poolAdapter.getServers(defaultServer), is(equalTo(defaultServer)));
		assertThat(poolAdapter.getSocketBufferSize(8192), is(equalTo(8192)));
		assertThat(poolAdapter.getStatisticInterval(2000), is(equalTo(2000)));
		assertThat(poolAdapter.getSubscriptionAckInterval(50), is(equalTo(50)));
		assertThat(poolAdapter.getSubscriptionEnabled(false), is(equalTo(false)));
		assertThat(poolAdapter.getSubscriptionMessageTrackingTimeout(15000), is(equalTo(15000)));
		assertThat(poolAdapter.getSubscriptionRedundancy(1), is(equalTo(1)));
		assertThat(poolAdapter.getThreadLocalConnections(true), is(equalTo(true)));

		verify(mockPool, times(1)).getName();
		verify(mockPool, times(1)).getPendingEventCount();
		verifyNoMoreInteractions(mockPool);
	}

	@Test
	public void poolAdapterPreferringDefaultsUsesPoolValuesWhenSomeDefaultValuesAreNull() {
		assertThat(poolAdapter.preferDefault(), is(sameInstance(poolAdapter)));

		List<InetSocketAddress> defaultLocator = Collections.singletonList(newSocketAddress("boombox", 21668));
		List<InetSocketAddress> poolServer = Collections.singletonList(newSocketAddress("localhost", 40404));

		assertThat(poolAdapter.getDelegate(), is(equalTo(mockPool)));
		assertThat(poolAdapter.prefersDefault(), is(true));
		assertThat(poolAdapter.getFreeConnectionTimeout(null), is(equalTo(5000)));
		assertThat(poolAdapter.getIdleTimeout(null), is(equalTo(120000L)));
		assertThat(poolAdapter.getLoadConditioningInterval(60000), is(equalTo(60000)));
		assertThat(poolAdapter.getLocators(defaultLocator), is(equalTo(defaultLocator)));
		assertThat(poolAdapter.getMaxConnections(null), is(equalTo(500)));
		assertThat(poolAdapter.getMinConnections(50), is(equalTo(50)));
		assertThat(poolAdapter.getMultiuserAuthentication(null), is(equalTo(true)));
		assertThat(poolAdapter.getName(), is(equalTo("TestPool")));
		assertThat(poolAdapter.getPendingEventCount(), is(equalTo(2)));
		assertThat(poolAdapter.getPingInterval(null), is(equalTo(15000L)));
		assertThat(poolAdapter.getPRSingleHopEnabled(true), is(equalTo(true)));
		assertThat(poolAdapter.getQueryService(null), is(nullValue()));
		assertThat(poolAdapter.getReadTimeout(20000), is(equalTo(20000)));
		assertThat(poolAdapter.getRetryAttempts(null), is(equalTo(1)));
		assertThat(poolAdapter.getServerGroup("MockGroup"), is(equalTo("MockGroup")));
		assertThat(poolAdapter.getServers(null), is(equalTo(poolServer)));
		assertThat(poolAdapter.getSocketBufferSize(32768), is(equalTo(32768)));
		assertThat(poolAdapter.getStatisticInterval(null), is(equalTo(1000)));
		assertThat(poolAdapter.getSubscriptionAckInterval(50), is(equalTo(50)));
		assertThat(poolAdapter.getSubscriptionEnabled(true), is(equalTo(true)));
		assertThat(poolAdapter.getSubscriptionMessageTrackingTimeout(null), is(equalTo(20000)));
		assertThat(poolAdapter.getSubscriptionRedundancy(null), is(equalTo(2)));
		assertThat(poolAdapter.getThreadLocalConnections(null), is(equalTo(false)));

		verify(mockPool, times(1)).getFreeConnectionTimeout();
		verify(mockPool, times(1)).getIdleTimeout();
		verify(mockPool, times(1)).getMaxConnections();
		verify(mockPool, times(1)).getMultiuserAuthentication();
		verify(mockPool, times(1)).getName();
		verify(mockPool, times(1)).getPendingEventCount();
		verify(mockPool, times(1)).getPingInterval();
		verify(mockPool, times(1)).getQueryService();
		verify(mockPool, times(1)).getRetryAttempts();
		verify(mockPool, times(1)).getServers();
		verify(mockPool, times(1)).getStatisticInterval();
		verify(mockPool, times(1)).getSubscriptionMessageTrackingTimeout();
		verify(mockPool, times(1)).getSubscriptionRedundancy();
		verify(mockPool, times(1)).getThreadLocalConnections();
		verifyNoMoreInteractions(mockPool);
	}

	@Test
	public void poolAdapterPreferringDefaultsUsesPoolValuesExclusivelyWhenAllDefaultValuesAreNull() {
		assertThat(poolAdapter.preferDefault(), is(sameInstance(poolAdapter)));

		List<InetSocketAddress> poolServer = Collections.singletonList(newSocketAddress("localhost", 40404));

		assertThat(poolAdapter.getDelegate(), is(equalTo(mockPool)));
		assertThat(poolAdapter.prefersDefault(), is(true));
		assertThat(poolAdapter.getFreeConnectionTimeout(null), is(equalTo(5000)));
		assertThat(poolAdapter.getIdleTimeout(null), is(equalTo(120000L)));
		assertThat(poolAdapter.getLoadConditioningInterval(null), is(equalTo(300000)));
		assertThat(poolAdapter.getLocators(null), is(equalTo(Collections.<InetSocketAddress>emptyList())));
		assertThat(poolAdapter.getMaxConnections(null), is(equalTo(500)));
		assertThat(poolAdapter.getMinConnections(null), is(equalTo(50)));
		assertThat(poolAdapter.getMultiuserAuthentication(null), is(equalTo(true)));
		assertThat(poolAdapter.getName(), is(equalTo("TestPool")));
		assertThat(poolAdapter.getPendingEventCount(), is(equalTo(2)));
		assertThat(poolAdapter.getPingInterval(null), is(equalTo(15000L)));
		assertThat(poolAdapter.getPRSingleHopEnabled(null), is(equalTo(true)));
		assertThat(poolAdapter.getQueryService(null), is(nullValue()));
		assertThat(poolAdapter.getReadTimeout(null), is(equalTo(30000)));
		assertThat(poolAdapter.getRetryAttempts(null), is(equalTo(1)));
		assertThat(poolAdapter.getServerGroup(null), is(equalTo("TestGroup")));
		assertThat(poolAdapter.getServers(null), is(equalTo(poolServer)));
		assertThat(poolAdapter.getSocketBufferSize(null), is(equalTo(16384)));
		assertThat(poolAdapter.getStatisticInterval(null), is(equalTo(1000)));
		assertThat(poolAdapter.getSubscriptionAckInterval(null), is(equalTo(200)));
		assertThat(poolAdapter.getSubscriptionEnabled(null), is(equalTo(true)));
		assertThat(poolAdapter.getSubscriptionMessageTrackingTimeout(null), is(equalTo(20000)));
		assertThat(poolAdapter.getSubscriptionRedundancy(null), is(equalTo(2)));
		assertThat(poolAdapter.getThreadLocalConnections(null), is(equalTo(false)));

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
		verifyNoMoreInteractions(mockPool);
	}

	@Test
	public void poolAdapterPreferringPoolUsesUseNonNullPoolValues() {
		assertThat(poolAdapter.preferPool(), is(sameInstance(poolAdapter)));

		List<InetSocketAddress> defaultServer = Collections.singletonList(newSocketAddress("jambox", 12480));
		List<InetSocketAddress> poolServer = Collections.singletonList(newSocketAddress("localhost", 40404));

		assertThat(poolAdapter.getFreeConnectionTimeout(15000), is(equalTo(5000)));
		assertThat(poolAdapter.getIdleTimeout(60000L), is(equalTo(120000L)));
		assertThat(poolAdapter.getLoadConditioningInterval(180000), is(equalTo(300000)));
		assertThat(poolAdapter.getLocators(Collections.emptyList()), is(equalTo(Collections.<InetSocketAddress>emptyList())));
		assertThat(poolAdapter.getMaxConnections(999), is(equalTo(500)));
		assertThat(poolAdapter.getMinConnections(99), is(equalTo(50)));
		assertThat(poolAdapter.getMultiuserAuthentication(false), is(equalTo(true)));
		assertThat(poolAdapter.getName(), is(equalTo("TestPool")));
		assertThat(poolAdapter.getPendingEventCount(), is(equalTo(2)));
		assertThat(poolAdapter.getPingInterval(20000L), is(equalTo(15000L)));
		assertThat(poolAdapter.getPRSingleHopEnabled(false), is(equalTo(true)));
		assertThat(poolAdapter.getQueryService(null), is(nullValue()));
		assertThat(poolAdapter.getReadTimeout(20000), is(equalTo(30000)));
		assertThat(poolAdapter.getRetryAttempts(4), is(equalTo(1)));
		assertThat(poolAdapter.getServerGroup("MockGroup"), is(equalTo("TestGroup")));
		assertThat(poolAdapter.getServers(defaultServer), is(equalTo(poolServer)));
		assertThat(poolAdapter.getSocketBufferSize(8192), is(equalTo(16384)));
		assertThat(poolAdapter.getStatisticInterval(2000), is(equalTo(1000)));
		assertThat(poolAdapter.getSubscriptionAckInterval(50), is(equalTo(200)));
		assertThat(poolAdapter.getSubscriptionEnabled(false), is(equalTo(true)));
		assertThat(poolAdapter.getSubscriptionMessageTrackingTimeout(30000), is(equalTo(20000)));
		assertThat(poolAdapter.getSubscriptionRedundancy(1), is(equalTo(2)));
		assertThat(poolAdapter.getThreadLocalConnections(true), is(equalTo(false)));

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
		verifyNoMoreInteractions(mockPool);
	}

	@Test
	public void poolAdapterDestroyUsesPoolRegardlessOfPreference() {
		assertThat(poolAdapter.preferDefault(), is(sameInstance(poolAdapter)));
		assertThat(poolAdapter.getDelegate(), is(equalTo(mockPool)));
		assertThat(poolAdapter.prefersDefault(), is(true));

		poolAdapter.destroy();

		assertThat(poolAdapter.preferPool(), is(sameInstance(poolAdapter)));
		assertThat(poolAdapter.getDelegate(), is(equalTo(mockPool)));
		assertThat(poolAdapter.prefersPool(), is(true));

		poolAdapter.destroy(true);

		verify(mockPool, times(1)).destroy();
		verify(mockPool, times(1)).destroy(anyBoolean());
	}

	@Test
	public void poolAdapterReleaseThreadLocalConnections() {
		assertThat(poolAdapter.preferDefault(), is(sameInstance(poolAdapter)));
		assertThat(poolAdapter.getDelegate(), is(equalTo(mockPool)));
		assertThat(poolAdapter.prefersDefault(), is(true));

		poolAdapter.releaseThreadLocalConnection();

		assertThat(poolAdapter.preferPool(), is(sameInstance(poolAdapter)));
		assertThat(poolAdapter.getDelegate(), is(equalTo(mockPool)));
		assertThat(poolAdapter.prefersPool(), is(true));

		poolAdapter.releaseThreadLocalConnection();

		verify(mockPool, times(2)).releaseThreadLocalConnection();
	}
}
