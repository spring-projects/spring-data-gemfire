/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.server;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.InterestRegistrationListener;
import org.apache.geode.cache.server.CacheServer;
import org.apache.geode.cache.server.ClientSubscriptionConfig;
import org.apache.geode.cache.server.ServerLoadProbe;

import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

/**
 * Unit tests for {@link CacheServerFactoryBean}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.springframework.data.gemfire.server.CacheServerFactoryBean
 * @since 1.6.0
 */
public class CacheServerFactoryBeanTest {

	@Test
	public void testGetObjectUninitialized() {
		assertNull(new CacheServerFactoryBean().getObject());
	}

	@Test
	public void testGetObjectType() {
		assertEquals(CacheServer.class, new CacheServerFactoryBean().getObjectType());
	}

	@Test
	public void testIsSingleton() {
		assertTrue(new CacheServerFactoryBean().isSingleton());
	}

	@Test
	@SuppressWarnings("deprecation")
	public void testAfterPropertiesSet() throws IOException {
		Cache mockCache = mock(Cache.class, "testAfterPropertiesSet.MockCache");

		CacheServer mockCacheServer = mock(CacheServer.class, "testAfterPropertiesSet.MockCacheServer");

		ClientSubscriptionConfig mockClientSubscriptionConfig = mock(ClientSubscriptionConfig.class,
			"testAfterPropertiesSet.MockClientSubscriptionConfig");

		InterestRegistrationListener mockInterestRegistrationListenerOne = mock(InterestRegistrationListener.class,
			"testAfterPropertiesSet.MockInterestRegistrationListener.One");

		InterestRegistrationListener mockInterestRegistrationListenerTwo = mock(InterestRegistrationListener.class,
			"testAfterPropertiesSet.MockInterestRegistrationListener.Two");

		ServerLoadProbe mockServerLoadProbe = mock(ServerLoadProbe.class, "testAfterPropertiesSet.MockServerLoadProbe");

		when(mockCache.addCacheServer()).thenReturn(mockCacheServer);
		when(mockCacheServer.getClientSubscriptionConfig()).thenReturn(mockClientSubscriptionConfig);

		CacheServerFactoryBean factoryBean = new CacheServerFactoryBean();

		factoryBean.setCache(mockCache);
		factoryBean.setBindAddress("10.124.62.5");
		factoryBean.setHostNameForClients("skullbox");
		factoryBean.setListeners(new HashSet<InterestRegistrationListener>(Arrays.asList(
			mockInterestRegistrationListenerOne, mockInterestRegistrationListenerTwo)));
		factoryBean.setLoadPollInterval(500l);
		factoryBean.setMaxConnections(200);
		factoryBean.setMaxMessageCount(1024);
		factoryBean.setMaxTimeBetweenPings(2000);
		factoryBean.setMaxThreads(50);
		factoryBean.setMessageTimeToLive(8192);
		factoryBean.setNotifyBySubscription(true);
		factoryBean.setPort(54321);
		factoryBean.setServerGroups(new String[] { "testGroup" });
		factoryBean.setServerLoadProbe(mockServerLoadProbe);
		factoryBean.setSocketBufferSize(4096);
		factoryBean.setSubscriptionCapacity(8196);
		factoryBean.setSubscriptionEvictionPolicy(SubscriptionEvictionPolicy.ENTRY);
		factoryBean.setSubscriptionDiskStore("toTheVoid");
		factoryBean.afterPropertiesSet();

		assertSame(mockCacheServer, factoryBean.getObject());
		assertEquals(mockCacheServer.getClass(), factoryBean.getObjectType());

		verify(mockCache, times(1)).addCacheServer();
		verify(mockCacheServer, times(1)).setBindAddress(eq("10.124.62.5"));
		verify(mockCacheServer, times(1)).setGroups(eq(new String[] { "testGroup" }));
		verify(mockCacheServer, times(1)).setHostnameForClients(eq("skullbox"));
		verify(mockCacheServer, times(1)).setLoadPollInterval(eq(500l));
		verify(mockCacheServer, times(1)).setLoadProbe(same(mockServerLoadProbe));
		verify(mockCacheServer, times(1)).setMaxConnections(eq(200));
		verify(mockCacheServer, times(1)).setMaximumMessageCount(eq(1024));
		verify(mockCacheServer, times(1)).setMaximumTimeBetweenPings(eq(2000));
		verify(mockCacheServer, times(1)).setMaxThreads(eq(50));
		verify(mockCacheServer, times(1)).setMessageTimeToLive(eq(8192));
		verify(mockCacheServer, times(1)).setNotifyBySubscription(eq(true));
		verify(mockCacheServer, times(1)).setPort(eq(54321));
		verify(mockCacheServer, times(1)).setSocketBufferSize(eq(4096));
		verify(mockCacheServer, times(1)).registerInterestRegistrationListener(
			same(mockInterestRegistrationListenerOne));
		verify(mockCacheServer, times(1)).registerInterestRegistrationListener(
			same(mockInterestRegistrationListenerTwo));
		verify(mockCacheServer, times(1)).getClientSubscriptionConfig();
		verify(mockClientSubscriptionConfig, times(1)).setCapacity(eq(8196));
		verify(mockClientSubscriptionConfig, times(1)).setDiskStoreName(eq("toTheVoid"));
		verify(mockClientSubscriptionConfig, times(1)).setEvictionPolicy("entry");
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAfterPropertiesSetWithNullCache() throws IOException {
		try {
			new CacheServerFactoryBean().afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Cache is required", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetAndGetAutoStartup() {
		CacheServerFactoryBean factoryBean = new CacheServerFactoryBean();

		assertTrue(factoryBean.isAutoStartup());

		factoryBean.setAutoStartup(false);

		assertFalse(factoryBean.isAutoStartup());
	}

	@Test
	public void testSetAndGetSubscriptionEvictionPolicy() {
		CacheServerFactoryBean factoryBean = new CacheServerFactoryBean();

		assertEquals(SubscriptionEvictionPolicy.DEFAULT, factoryBean.getSubscriptionEvictionPolicy());

		factoryBean.setSubscriptionEvictionPolicy(SubscriptionEvictionPolicy.MEM);

		assertEquals(SubscriptionEvictionPolicy.MEM, factoryBean.getSubscriptionEvictionPolicy());

		factoryBean.setSubscriptionEvictionPolicy(null);

		assertEquals(SubscriptionEvictionPolicy.DEFAULT, factoryBean.getSubscriptionEvictionPolicy());
	}

	@Test
	public void testCacheServerLifecycle() throws Exception {
		CacheServer mockCacheServer = mock(CacheServer.class, "testCacheServerLifecycle.MockCacheServer");

		final AtomicBoolean running = new AtomicBoolean(false);
		final AtomicBoolean called = new AtomicBoolean(false);

		doAnswer(new Answer() {
			@Override
			public Object answer(final InvocationOnMock invocationOnMock) throws Throwable {
				running.compareAndSet(false, true);
				return null;
			}
		}).when(mockCacheServer).start();

		doAnswer(new Answer() {
			@Override
			public Object answer(final InvocationOnMock invocationOnMock) throws Throwable {
				running.compareAndSet(true, false);
				return null;
			}
		}).when(mockCacheServer).stop();

		when(mockCacheServer.isRunning()).then(new Answer<Boolean>() {
			@Override
			public Boolean answer(final InvocationOnMock invocationOnMock) throws Throwable {
				return running.get();
			}
		});

		CacheServerFactoryBean factoryBean = new CacheServerFactoryBean();

		factoryBean.setCacheServer(mockCacheServer);

		assertSame(mockCacheServer, factoryBean.getObject());
		assertFalse(factoryBean.isRunning());

		factoryBean.start();

		assertTrue(factoryBean.isRunning());
		assertFalse(called.get());

		factoryBean.stop(new Runnable() {
			@Override public void run() {
				called.set(true);
			}
		});

		assertFalse(factoryBean.isRunning());
		assertTrue(called.get());
	}
}
