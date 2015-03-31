/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.client;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Properties;

import org.junit.Test;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.config.GemfireConstants;

import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.internal.lang.ClassUtils;

/**
 * The ClientCacheFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the SDG ClientCacheFactoryBean class.
 *
 * @author John Blum
 * @see org.mockito.Mockito
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
 * @since 1.7.0
 */
public class ClientCacheFactoryBeanTest {

	@Test
	public void testGetObjectType() {
		assertEquals(ClientCache.class, new ClientCacheFactoryBean().getObjectType());
	}

	@Test
	public void testIsSingleton() {
		assertTrue(new ClientCacheFactoryBean().isSingleton());
	}

	@Test
	public void testCreateCache() {
		BeanFactory mockBeanFactory = mock(BeanFactory.class, "MockSpringBeanFactory");
		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class, "MockGemFireClientCacheFactory");
		ClientCache mockClientCache = mock(ClientCache.class, "MockGemFireClientCache");
		Pool mockPool = mock(Pool.class, "MockGemFirePool");

		when(mockClientCacheFactory.create()).thenReturn(mockClientCache);
		when(mockBeanFactory.isTypeMatch(eq("testCreateCache.Pool"), eq(Pool.class))).thenReturn(true);
		when(mockBeanFactory.getBean(eq("testCreateCache.Pool"), eq(Pool.class))).thenReturn(mockPool);
		when(mockPool.getFreeConnectionTimeout()).thenReturn(30000);
		when(mockPool.getIdleTimeout()).thenReturn(60000l);
		when(mockPool.getLoadConditioningInterval()).thenReturn(45000);
		when(mockPool.getMaxConnections()).thenReturn(100);
		when(mockPool.getMinConnections()).thenReturn(10);
		when(mockPool.getMultiuserAuthentication()).thenReturn(true);
		when(mockPool.getPingInterval()).thenReturn(15000l);
		when(mockPool.getPRSingleHopEnabled()).thenReturn(true);
		when(mockPool.getReadTimeout()).thenReturn(20000);
		when(mockPool.getRetryAttempts()).thenReturn(10);
		when(mockPool.getServerGroup()).thenReturn("TestServerGroup");
		when(mockPool.getSocketBufferSize()).thenReturn(32768);
		when(mockPool.getStatisticInterval()).thenReturn(5000);
		when(mockPool.getSubscriptionAckInterval()).thenReturn(15000);
		when(mockPool.getSubscriptionEnabled()).thenReturn(true);
		when(mockPool.getSubscriptionMessageTrackingTimeout()).thenReturn(30000);
		when(mockPool.getSubscriptionRedundancy()).thenReturn(2);
		when(mockPool.getThreadLocalConnections()).thenReturn(false);

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setBeanFactory(mockBeanFactory);
		clientCacheFactoryBean.setPoolName("testCreateCache.Pool");
		clientCacheFactoryBean.setReadyForEvents(false);

		GemFireCache actualCache = clientCacheFactoryBean.createCache(mockClientCacheFactory);

		assertSame(mockClientCache, actualCache);

		verify(mockClientCacheFactory, times(1)).create();
		verify(mockBeanFactory, times(1)).isTypeMatch(eq("testCreateCache.Pool"), eq(Pool.class));
		verify(mockBeanFactory, times(1)).getBean(eq("testCreateCache.Pool"), eq(Pool.class));
		verify(mockBeanFactory, never()).getBean(eq(Pool.class));
		verify(mockClientCacheFactory, times(1)).setPoolFreeConnectionTimeout(eq(30000));
		verify(mockClientCacheFactory, times(1)).setPoolIdleTimeout(eq(60000l));
		verify(mockClientCacheFactory, times(1)).setPoolLoadConditioningInterval(eq(45000));
		verify(mockClientCacheFactory, times(1)).setPoolMaxConnections(eq(100));
		verify(mockClientCacheFactory, times(1)).setPoolMinConnections(eq(10));
		verify(mockClientCacheFactory, times(1)).setPoolMultiuserAuthentication(eq(true));
		verify(mockClientCacheFactory, times(1)).setPoolPingInterval(eq(15000l));
		verify(mockClientCacheFactory, times(1)).setPoolPRSingleHopEnabled(eq(true));
		verify(mockClientCacheFactory, times(1)).setPoolReadTimeout(eq(20000));
		verify(mockClientCacheFactory, times(1)).setPoolRetryAttempts(eq(10));
		verify(mockClientCacheFactory, times(1)).setPoolServerGroup(eq("TestServerGroup"));
		verify(mockClientCacheFactory, times(1)).setPoolSocketBufferSize(eq(32768));
		verify(mockClientCacheFactory, times(1)).setPoolStatisticInterval(eq(5000));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionAckInterval(eq(15000));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionEnabled(eq(true));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionMessageTrackingTimeout(eq(30000));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionRedundancy(eq(2));
		verify(mockClientCacheFactory, times(1)).setPoolThreadLocalConnections(eq(false));
	}

	@Test
	public void testCreateClientCacheFactory() {
		Properties gemfireProperties = new Properties();
		Object clientCacheFactoryReference = new ClientCacheFactoryBean().createFactory(gemfireProperties);

		assertTrue(gemfireProperties.isEmpty());
		assertTrue(clientCacheFactoryReference instanceof ClientCacheFactory);

		ClientCacheFactory clientCacheFactory = (ClientCacheFactory) clientCacheFactoryReference;

		clientCacheFactory.set("name", "TestCreateClientCacheFactory");

		assertTrue(gemfireProperties.containsKey("name"));
		assertEquals("TestCreateClientCacheFactory", gemfireProperties.get("name"));
	}

	@Test
	public void testResolvePoolWithUnresolvablePoolName() throws Exception {
		BeanFactory mockBeanFactory = mock(BeanFactory.class, "MockSpringBeanFactory");
		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class, "MockGemFireClientCacheFactory");
		ClientCache mockClientCache = mock(ClientCache.class, "MockGemFireClientCache");
		Pool mockPool = mock(Pool.class, "MockGemFirePool");

		when(mockBeanFactory.isTypeMatch(any(String.class), eq(Pool.class))).thenReturn(false);
		when(mockBeanFactory.getBean(eq(Pool.class))).thenReturn(mockPool);
		when(mockClientCacheFactory.create()).thenReturn(mockClientCache);
		when(mockPool.getFreeConnectionTimeout()).thenReturn(120000);
		when(mockPool.getIdleTimeout()).thenReturn(300000l);
		when(mockPool.getLoadConditioningInterval()).thenReturn(15000);
		when(mockPool.getMaxConnections()).thenReturn(50);
		when(mockPool.getMinConnections()).thenReturn(5);
		when(mockPool.getMultiuserAuthentication()).thenReturn(false);
		when(mockPool.getName()).thenReturn("MockGemFirePool");
		when(mockPool.getPingInterval()).thenReturn(12000l);
		when(mockPool.getPRSingleHopEnabled()).thenReturn(true);
		when(mockPool.getReadTimeout()).thenReturn(60000);
		when(mockPool.getRetryAttempts()).thenReturn(5);
		when(mockPool.getServerGroup()).thenReturn("MockServerGroup");
		when(mockPool.getSocketBufferSize()).thenReturn(16384);
		when(mockPool.getStatisticInterval()).thenReturn(1000);
		when(mockPool.getSubscriptionAckInterval()).thenReturn(500);
		when(mockPool.getSubscriptionEnabled()).thenReturn(true);
		when(mockPool.getSubscriptionMessageTrackingTimeout()).thenReturn(15000);
		when(mockPool.getSubscriptionRedundancy()).thenReturn(4);
		when(mockPool.getThreadLocalConnections()).thenReturn(false);

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setBeanFactory(mockBeanFactory);
		clientCacheFactoryBean.setPoolName("TestGemFirePool");
		clientCacheFactoryBean.setReadyForEvents(false);

		assertEquals("TestGemFirePool", TestUtils.readField("poolName", clientCacheFactoryBean));

		GemFireCache actualClientCache = clientCacheFactoryBean.createCache(mockClientCacheFactory);

		assertSame(mockClientCache, actualClientCache);
		assertEquals("MockGemFirePool", TestUtils.readField("poolName", clientCacheFactoryBean));

		verify(mockClientCacheFactory, times(1)).create();
		verify(mockBeanFactory, times(1)).isTypeMatch(eq("TestGemFirePool"), eq(Pool.class));
		verify(mockBeanFactory, never()).getBean(eq("TestGemFirePool"), eq(Pool.class));
		verify(mockBeanFactory, times(1)).getBean(eq(Pool.class));
		verify(mockPool, times(1)).getName();
		verify(mockClientCacheFactory, times(1)).setPoolFreeConnectionTimeout(eq(120000));
		verify(mockClientCacheFactory, times(1)).setPoolIdleTimeout(eq(300000l));
		verify(mockClientCacheFactory, times(1)).setPoolLoadConditioningInterval(eq(15000));
		verify(mockClientCacheFactory, times(1)).setPoolMaxConnections(eq(50));
		verify(mockClientCacheFactory, times(1)).setPoolMinConnections(eq(5));
		verify(mockClientCacheFactory, times(1)).setPoolMultiuserAuthentication(eq(false));
		verify(mockClientCacheFactory, times(1)).setPoolPingInterval(eq(12000l));
		verify(mockClientCacheFactory, times(1)).setPoolPRSingleHopEnabled(eq(true));
		verify(mockClientCacheFactory, times(1)).setPoolReadTimeout(eq(60000));
		verify(mockClientCacheFactory, times(1)).setPoolRetryAttempts(eq(5));
		verify(mockClientCacheFactory, times(1)).setPoolServerGroup(eq("MockServerGroup"));
		verify(mockClientCacheFactory, times(1)).setPoolSocketBufferSize(eq(16384));
		verify(mockClientCacheFactory, times(1)).setPoolStatisticInterval(eq(1000));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionAckInterval(eq(500));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionEnabled(eq(true));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionMessageTrackingTimeout(eq(15000));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionRedundancy(eq(4));
		verify(mockClientCacheFactory, times(1)).setPoolThreadLocalConnections(eq(false));
	}

	@Test(expected = BeanInitializationException.class)
	public void testResolveUnresolvablePool() {
		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class, "MockGemFireClientCacheFactory");

		try {
			BeanFactory mockBeanFactory = mock(BeanFactory.class, "MockSpringBeanFactory");

			when(mockBeanFactory.isTypeMatch(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME), eq(Pool.class)))
				.thenReturn(true);
			when(mockBeanFactory.getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME), eq(Pool.class)))
				.thenThrow(new IllegalArgumentException("TEST"));

			ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

			clientCacheFactoryBean.setBeanFactory(mockBeanFactory);
			clientCacheFactoryBean.setPoolName(GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME);
			clientCacheFactoryBean.setReadyForEvents(false);
			clientCacheFactoryBean.createCache(mockClientCacheFactory);
		}
		catch (BeanInitializationException expected) {
			assertTrue(expected.getMessage(), expected.getMessage().startsWith(String.format(
				"no Bean of type '%1$s' having name '%2$s' was found; a ClientCache requires a Pool",
				Pool.class.getName(), GemfireConstants.DEFAULT_GEMFIRE_POOL_NAME)));
			assertTrue(String.format("Cause was: %1$s!", ClassUtils.getClassName(expected.getCause())),
				expected.getCause() instanceof IllegalArgumentException);
			assertEquals("TEST", expected.getCause().getMessage());
			throw expected;
		}
		finally {
			verify(mockClientCacheFactory, never()).create();
		}
	}

	@Test
	public void testAutoReconnectDisabled() {
		assertFalse(new ClientCacheFactoryBean().getEnableAutoReconnect());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetPoolToNull() {
		try {
			new ClientCacheFactoryBean().setPool(null);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("GemFire Pool must not be null", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testSetPoolNameToInvalidValue() {
		try {
			new ClientCacheFactoryBean().setPoolName("  ");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Pool 'name' is required", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testSetAndGetReadyForEvents() {
		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		assertFalse(clientCacheFactoryBean.getReadyForEvents());

		clientCacheFactoryBean.setReadyForEvents(true);

		assertTrue(clientCacheFactoryBean.getReadyForEvents());

		clientCacheFactoryBean.setReadyForEvents(null);

		assertNull(clientCacheFactoryBean.getReadyForEvents());
	}

	@Test
	public void testUsesClusterConfiguration() {
		assertFalse(new ClientCacheFactoryBean().getUseClusterConfiguration());
	}

}
