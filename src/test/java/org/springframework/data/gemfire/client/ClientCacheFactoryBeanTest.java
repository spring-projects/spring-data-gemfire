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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Properties;

import org.junit.Test;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.config.GemfireConstants;
import org.springframework.data.gemfire.util.DistributedSystemUtils;

import com.gemstone.gemfire.cache.CacheClosedException;
import com.gemstone.gemfire.cache.GemFireCache;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.client.ClientCacheFactory;
import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.distributed.DistributedSystem;
import com.gemstone.gemfire.internal.lang.ClassUtils;
import com.gemstone.gemfire.pdx.PdxSerializer;

/**
 * The ClientCacheFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the SDG ClientCacheFactoryBean class.
 *
 * @author John Blum
 * @see org.mockito.Mockito
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
 * @see com.gemstone.gemfire.cache.client.ClientCache
 * @see com.gemstone.gemfire.cache.client.ClientCacheFactory
 * @since 1.7.0
 */
public class ClientCacheFactoryBeanTest {

	protected Properties createProperties(String key, String value) {
		return addProperty(null, key, value);
	}

	protected Properties addProperty(Properties properties, String key, String value) {
		properties = (properties != null ? properties : new Properties());
		properties.setProperty(key, value);
		return properties;
	}

	@Test
	public void getObjectType() {
		assertEquals(ClientCache.class, new ClientCacheFactoryBean().getObjectType());
	}

	@Test
	public void isSingleton() {
		assertTrue(new ClientCacheFactoryBean().isSingleton());
	}

	@Test
	public void resolvePropertiesWhenDistributedSystemIsConnected() {
		Properties gemfireProperties = createProperties("gf", "test");
		Properties distributedSystemProperties = createProperties("ds", "mock");

		final DistributedSystem mockDistributedSystem = mock(DistributedSystem.class, "MockDistributedSystem");

		when(mockDistributedSystem.isConnected()).thenReturn(true);
		when(mockDistributedSystem.getProperties()).thenReturn(distributedSystemProperties);

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@SuppressWarnings("unchecked") @Override <T extends DistributedSystem> T getDistributedSystem() {
				return (T) mockDistributedSystem;
			}
		};

		clientCacheFactoryBean.setProperties(gemfireProperties);

		Properties resolvedProperties = clientCacheFactoryBean.resolveProperties();

		assertThat(resolvedProperties, is(notNullValue()));
		assertThat(resolvedProperties, is(not(sameInstance(gemfireProperties))));
		assertThat(resolvedProperties, is(not(sameInstance(distributedSystemProperties))));
		assertThat(resolvedProperties.size(), is(equalTo(2)));
		assertThat(resolvedProperties.containsKey(DistributedSystemUtils.DURABLE_CLIENT_ID_PROPERTY_NAME), is(false));
		assertThat(resolvedProperties.containsKey(DistributedSystemUtils.DURABLE_CLIENT_TIMEOUT_PROPERTY_NAME), is(false));
		assertThat(resolvedProperties.getProperty("gf"), is(equalTo("test")));
		assertThat(resolvedProperties.getProperty("ds"), is(equalTo("mock")));

		verify(mockDistributedSystem, times(1)).isConnected();
		verify(mockDistributedSystem, times(1)).getProperties();
	}

	@Test
	public void resolvePropertiesWhenDistributedSystemIsConnectedAndClientIsDurable() {
		Properties gemfireProperties = DistributedSystemUtils.configureDurableClient(createProperties("gf", "test"),
			"123", 600);

		Properties distributedSystemProperties = DistributedSystemUtils.configureDurableClient(
			createProperties("ds", "mock"), "987", 300);

		final DistributedSystem mockDistributedSystem = mock(DistributedSystem.class, "MockDistributedSystem");

		when(mockDistributedSystem.isConnected()).thenReturn(true);
		when(mockDistributedSystem.getProperties()).thenReturn(distributedSystemProperties);

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@SuppressWarnings("unchecked") @Override <T extends DistributedSystem> T getDistributedSystem() {
				return (T) mockDistributedSystem;
			}
		};

		clientCacheFactoryBean.setProperties(gemfireProperties);

		Properties resolvedProperties = clientCacheFactoryBean.resolveProperties();

		assertThat(resolvedProperties, is(notNullValue()));
		assertThat(resolvedProperties, is(not(sameInstance(gemfireProperties))));
		assertThat(resolvedProperties, is(not(sameInstance(distributedSystemProperties))));
		assertThat(resolvedProperties.size(), is(equalTo(4)));
		assertThat(resolvedProperties.getProperty("gf"), is(equalTo("test")));
		assertThat(resolvedProperties.getProperty("ds"), is(equalTo("mock")));
		assertThat(resolvedProperties.getProperty(DistributedSystemUtils.DURABLE_CLIENT_ID_PROPERTY_NAME),
			is(equalTo("123")));
		assertThat(resolvedProperties.getProperty(DistributedSystemUtils.DURABLE_CLIENT_TIMEOUT_PROPERTY_NAME),
			is(equalTo("600")));

		verify(mockDistributedSystem, times(1)).isConnected();
		verify(mockDistributedSystem, times(1)).getProperties();
	}

	@Test
	public void resolvePropertiesWhenDistributedSystemIsDisconnected() {
		Properties gemfireProperties = createProperties("gf", "test");
		Properties distributedSystemProperties = createProperties("ds", "mock");

		final DistributedSystem mockDistributedSystem = mock(DistributedSystem.class, "MockDistributedSystem");

		when(mockDistributedSystem.isConnected()).thenReturn(false);
		when(mockDistributedSystem.getProperties()).thenReturn(distributedSystemProperties);

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@SuppressWarnings("unchecked") @Override <T extends DistributedSystem> T getDistributedSystem() {
				return (T) mockDistributedSystem;
			}
		};

		clientCacheFactoryBean.setProperties(gemfireProperties);

		Properties resolvedProperties = clientCacheFactoryBean.resolveProperties();

		assertSame(gemfireProperties, resolvedProperties);

		verify(mockDistributedSystem, times(1)).isConnected();
		verify(mockDistributedSystem, never()).getProperties();
	}

	@Test
	public void resolvePropertiesWhenDistributedSystemIsNull() {
		Properties gemfireProperties = createProperties("gf", "test");

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override <T extends DistributedSystem> T getDistributedSystem() {
				return null;
			}
		};

		clientCacheFactoryBean.setProperties(gemfireProperties);

		Properties resolvedProperties = clientCacheFactoryBean.resolveProperties();

		assertSame(gemfireProperties, resolvedProperties);
	}

	@Test
	public void createClientCacheFactory() {
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
	public void prepareClientCacheFactoryWithUnspecifiedPdxOptions() {
		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class, "MockGemFireClientCacheFactory");

		assertSame(mockClientCacheFactory, new ClientCacheFactoryBean().prepareFactory(mockClientCacheFactory));

		verify(mockClientCacheFactory, never()).setPdxSerializer(any(PdxSerializer.class));
		verify(mockClientCacheFactory, never()).setPdxDiskStore(any(String.class));
		verify(mockClientCacheFactory, never()).setPdxIgnoreUnreadFields(any(Boolean.class));
		verify(mockClientCacheFactory, never()).setPdxPersistent(any(Boolean.class));
		verify(mockClientCacheFactory, never()).setPdxReadSerialized(any(Boolean.class));
	}

	@Test
	public void prepareClientCacheFactoryWithPartialPdxOptions() {
		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setPdxSerializer(mock(PdxSerializer.class));
		clientCacheFactoryBean.setPdxReadSerialized(true);
		clientCacheFactoryBean.setPdxIgnoreUnreadFields(false);

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class, "MockGemFireClientCacheFactory");

		assertSame(mockClientCacheFactory, clientCacheFactoryBean.prepareFactory(mockClientCacheFactory));

		verify(mockClientCacheFactory, times(1)).setPdxSerializer(any(PdxSerializer.class));
		verify(mockClientCacheFactory, never()).setPdxDiskStore(any(String.class));
		verify(mockClientCacheFactory, times(1)).setPdxIgnoreUnreadFields(eq(false));
		verify(mockClientCacheFactory, never()).setPdxPersistent(any(Boolean.class));
		verify(mockClientCacheFactory, times(1)).setPdxReadSerialized(eq(true));
	}

	@Test
	public void prepareClientCacheFactoryWithAllPdxOptions() {
		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setPdxSerializer(mock(PdxSerializer.class));
		clientCacheFactoryBean.setPdxDiskStoreName("mockPdxDiskStoreName");
		clientCacheFactoryBean.setPdxIgnoreUnreadFields(false);
		clientCacheFactoryBean.setPdxPersistent(true);
		clientCacheFactoryBean.setPdxReadSerialized(true);

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class, "MockGemFireClientCacheFactory");

		assertSame(mockClientCacheFactory, clientCacheFactoryBean.prepareFactory(mockClientCacheFactory));

		verify(mockClientCacheFactory, times(1)).setPdxSerializer(any(PdxSerializer.class));
		verify(mockClientCacheFactory, times(1)).setPdxDiskStore(eq("mockPdxDiskStoreName"));
		verify(mockClientCacheFactory, times(1)).setPdxIgnoreUnreadFields(eq(false));
		verify(mockClientCacheFactory, times(1)).setPdxPersistent(eq(true));
		verify(mockClientCacheFactory, times(1)).setPdxReadSerialized(eq(true));
	}

	@Test(expected = IllegalArgumentException.class)
	public void prepareClientCacheFactoryWithInvalidTypeForPdxSerializer() {
		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class, "MockGemFireClientCacheFactory");

		try {
			ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

			clientCacheFactoryBean.setPdxSerializer(new Object());
			clientCacheFactoryBean.setPdxReadSerialized(true);
			clientCacheFactoryBean.setPdxIgnoreUnreadFields(true);
			clientCacheFactoryBean.prepareFactory(mockClientCacheFactory);
		}
		catch (IllegalArgumentException expected) {
			assertTrue(expected.getMessage().startsWith("Invalid pdx serializer used"));
			assertNull(expected.getCause());
			throw expected;
		}
		finally {
			verify(mockClientCacheFactory, never()).setPdxSerializer(any(PdxSerializer.class));
			verify(mockClientCacheFactory, never()).setPdxDiskStore(any(String.class));
			verify(mockClientCacheFactory, never()).setPdxIgnoreUnreadFields(any(Boolean.class));
			verify(mockClientCacheFactory, never()).setPdxPersistent(any(Boolean.class));
			verify(mockClientCacheFactory, never()).setPdxReadSerialized(any(Boolean.class));
		}
	}

	@Test
	public void createCache() {
		BeanFactory mockBeanFactory = mock(BeanFactory.class, "MockBeanFactory");

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class, "MockClientCacheFactory");

		ClientCache mockClientCache = mock(ClientCache.class, "MockClientCache");

		Pool mockPool = mock(Pool.class, "MockPool");

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
		verify(mockClientCacheFactory, never()).setPoolFreeConnectionTimeout(eq(30000));
		verify(mockClientCacheFactory, never()).setPoolIdleTimeout(eq(60000l));
		verify(mockClientCacheFactory, never()).setPoolLoadConditioningInterval(eq(45000));
		verify(mockClientCacheFactory, never()).setPoolMaxConnections(eq(100));
		verify(mockClientCacheFactory, never()).setPoolMinConnections(eq(10));
		verify(mockClientCacheFactory, never()).setPoolMultiuserAuthentication(eq(true));
		verify(mockClientCacheFactory, never()).setPoolPingInterval(eq(15000l));
		verify(mockClientCacheFactory, never()).setPoolPRSingleHopEnabled(eq(true));
		verify(mockClientCacheFactory, never()).setPoolReadTimeout(eq(20000));
		verify(mockClientCacheFactory, never()).setPoolRetryAttempts(eq(10));
		verify(mockClientCacheFactory, never()).setPoolServerGroup(eq("TestServerGroup"));
		verify(mockClientCacheFactory, never()).setPoolSocketBufferSize(eq(32768));
		verify(mockClientCacheFactory, never()).setPoolStatisticInterval(eq(5000));
		verify(mockClientCacheFactory, never()).setPoolSubscriptionAckInterval(eq(15000));
		verify(mockClientCacheFactory, never()).setPoolSubscriptionEnabled(eq(true));
		verify(mockClientCacheFactory, never()).setPoolSubscriptionMessageTrackingTimeout(eq(30000));
		verify(mockClientCacheFactory, never()).setPoolSubscriptionRedundancy(eq(2));
		verify(mockClientCacheFactory, never()).setPoolThreadLocalConnections(eq(false));
	}

	@Test
	public void resolvePoolWithUnresolvablePoolName() throws Exception {
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
		verify(mockClientCacheFactory, never()).setPoolFreeConnectionTimeout(eq(120000));
		verify(mockClientCacheFactory, never()).setPoolIdleTimeout(eq(300000l));
		verify(mockClientCacheFactory, never()).setPoolLoadConditioningInterval(eq(15000));
		verify(mockClientCacheFactory, never()).setPoolMaxConnections(eq(50));
		verify(mockClientCacheFactory, never()).setPoolMinConnections(eq(5));
		verify(mockClientCacheFactory, never()).setPoolMultiuserAuthentication(eq(false));
		verify(mockClientCacheFactory, never()).setPoolPingInterval(eq(12000l));
		verify(mockClientCacheFactory, never()).setPoolPRSingleHopEnabled(eq(true));
		verify(mockClientCacheFactory, never()).setPoolReadTimeout(eq(60000));
		verify(mockClientCacheFactory, never()).setPoolRetryAttempts(eq(5));
		verify(mockClientCacheFactory, never()).setPoolServerGroup(eq("MockServerGroup"));
		verify(mockClientCacheFactory, never()).setPoolSocketBufferSize(eq(16384));
		verify(mockClientCacheFactory, never()).setPoolStatisticInterval(eq(1000));
		verify(mockClientCacheFactory, never()).setPoolSubscriptionAckInterval(eq(500));
		verify(mockClientCacheFactory, never()).setPoolSubscriptionEnabled(eq(true));
		verify(mockClientCacheFactory, never()).setPoolSubscriptionMessageTrackingTimeout(eq(15000));
		verify(mockClientCacheFactory, never()).setPoolSubscriptionRedundancy(eq(4));
		verify(mockClientCacheFactory, never()).setPoolThreadLocalConnections(eq(false));
	}

	@Test(expected = BeanInitializationException.class)
	public void resolveUnresolvablePool() {
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
				"no bean of type '%1$s' having name '%2$s' was found; a ClientCache requires a Pool",
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
	@SuppressWarnings("unchecked")
	public void onApplicationEventCallsClientCacheReadyForEvents() {
		final ClientCache mockClientCache = mock(ClientCache.class, "MockClientCache");

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override public boolean isReadyForEvents() {
				return true;
			}

			@Override protected <T extends GemFireCache> T fetchCache() {
				return (T) mockClientCache;
			}
		};

		clientCacheFactoryBean.onApplicationEvent(mock(ContextRefreshedEvent.class, "MockContextRefreshedEvent"));

		verify(mockClientCache, times(1)).readyForEvents();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void onApplicationEventDoesNotCallClientCacheReadyForEventsWhenClientCacheFactoryBeanReadyForEventsIsFalse() {
		final ClientCache mockClientCache = mock(ClientCache.class, "MockClientCache");

		doThrow(new RuntimeException("test")).when(mockClientCache).readyForEvents();

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override public boolean isReadyForEvents() {
				return false;
			}

			@Override protected <T extends GemFireCache> T fetchCache() {
				return (T) mockClientCache;
			}
		};

		clientCacheFactoryBean.onApplicationEvent(mock(ContextRefreshedEvent.class, "MockContextRefreshedEvent"));

		verify(mockClientCache, never()).readyForEvents();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void onApplicationEventHandlesIllegalStateException() {
		final ClientCache mockClientCache = mock(ClientCache.class, "MockClientCache");

		doThrow(new IllegalStateException("non-durable client")).when(mockClientCache).readyForEvents();

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override public boolean isReadyForEvents() {
				return true;
			}

			@Override protected <T extends GemFireCache> T fetchCache() {
				return (T) mockClientCache;
			}
		};

		clientCacheFactoryBean.onApplicationEvent(mock(ContextRefreshedEvent.class, "MockContextRefreshedEvent"));

		verify(mockClientCache, times(1)).readyForEvents();
	}

	@Test
	public void onApplicationEventHandlesCacheClosedException() {
		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override public boolean isReadyForEvents() {
				return true;
			}

			@Override protected <T extends GemFireCache> T fetchCache() {
				throw new CacheClosedException("test");
			}
		};

		clientCacheFactoryBean.onApplicationEvent(mock(ContextRefreshedEvent.class, "MockContextRefreshedEvent"));
	}

	@Test
	public void closeClientCacheWithKeepAlive() {
		ClientCache mockClientCache = mock(ClientCache.class, "MockClientCache");

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setKeepAlive(true);

		assertThat(clientCacheFactoryBean.isKeepAlive(), is(true));

		clientCacheFactoryBean.close(mockClientCache);

		verify(mockClientCache, times(1)).close(eq(true));
	}

	@Test
	public void closeClientCacheWithoutKeepAlive() {
		ClientCache mockClientCache = mock(ClientCache.class, "MockClientCache");

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setKeepAlive(false);

		assertThat(clientCacheFactoryBean.isKeepAlive(), is(false));

		clientCacheFactoryBean.close(mockClientCache);

		verify(mockClientCache, times(1)).close(eq(false));
	}

	@Test
	public void autoReconnectDisabled() {
		assertFalse(new ClientCacheFactoryBean().getEnableAutoReconnect());
	}

	@Test(expected = UnsupportedOperationException.class)
	public void enableAutoReconnect() {
		new ClientCacheFactoryBean().setEnableAutoReconnect(true);
	}

	@Test(expected = IllegalArgumentException.class)
	public void setPoolToNull() {
		try {
			new ClientCacheFactoryBean().setPool(null);
		}
		catch (IllegalArgumentException expected) {
			assertEquals("GemFire Pool must not be null", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void setPoolNameWithAnIllegalArgument() {
		try {
			new ClientCacheFactoryBean().setPoolName("  ");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Pool 'name' is required", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void setAndGetReadyForEvents() {
		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		assertThat(clientCacheFactoryBean.getReadyForEvents(), is(nullValue()));

		clientCacheFactoryBean.setReadyForEvents(true);

		assertThat(clientCacheFactoryBean.getReadyForEvents(), is(true));

		clientCacheFactoryBean.setReadyForEvents(null);

		assertThat(clientCacheFactoryBean.getReadyForEvents(), is(nullValue()));

		clientCacheFactoryBean.setReadyForEvents(false);

		assertThat(clientCacheFactoryBean.getReadyForEvents(), is(false));
	}

	protected ClientCache mockClientCache(String durableClientId) {
		ClientCache mockClientCache = mock(ClientCache.class, "MockClientCache");

		DistributedSystem mockDistributedSystem = mock(DistributedSystem.class, "MockDistributedSystem");

		Properties gemfireProperties = new Properties();

		gemfireProperties.setProperty(DistributedSystemUtils.DURABLE_CLIENT_ID_PROPERTY_NAME, durableClientId);
		gemfireProperties.setProperty(DistributedSystemUtils.DURABLE_CLIENT_TIMEOUT_PROPERTY_NAME, "300");

		when(mockClientCache.getDistributedSystem()).thenReturn(mockDistributedSystem);
		when(mockDistributedSystem.isConnected()).thenReturn(true);
		when(mockDistributedSystem.getProperties()).thenReturn(gemfireProperties);

		return mockClientCache;
	}

	@Test
	@SuppressWarnings("unchecked")
	public void isReadyForEventsIsTrueWhenClientCacheFactoryBeanReadyForEventsIsTrue() {
		final ClientCache mockClientCache = mockClientCache("");

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override protected <T extends GemFireCache> T fetchCache() {
				return (T) mockClientCache;
			}
		};

		clientCacheFactoryBean.setReadyForEvents(true);

		assertThat(clientCacheFactoryBean.getReadyForEvents(), is(true));
		assertThat(clientCacheFactoryBean.isReadyForEvents(), is(true));

		verify(mockClientCache, never()).getDistributedSystem();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void isReadyForEventsIsFalseWhenClientCacheFactoryBeanReadyForEventsIsFalse() {
		final ClientCache mockClientCache = mockClientCache("TestDurableClientId");

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override protected <T extends GemFireCache> T fetchCache() {
				return (T) mockClientCache;
			}
		};

		clientCacheFactoryBean.setReadyForEvents(false);

		assertThat(clientCacheFactoryBean.getReadyForEvents(), is(false));
		assertThat(clientCacheFactoryBean.isReadyForEvents(), is(false));

		verify(mockClientCache, never()).getDistributedSystem();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void isReadyForEventsIsTrueWhenDurableClientIdSet() {
		final ClientCache mockClientCache = mockClientCache("TestDurableClientId");

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override protected <T extends GemFireCache> T fetchCache() {
				return (T) mockClientCache;
			}
		};

		assertThat(clientCacheFactoryBean.getReadyForEvents(), is(nullValue()));
		assertThat(clientCacheFactoryBean.isReadyForEvents(), is(true));

		verify(mockClientCache, times(1)).getDistributedSystem();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void isReadyForEventsIsFalseWhenDurableClientIdIsNotSet() {
		final ClientCache mockClientCache = mockClientCache("  ");

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override protected <T extends GemFireCache> T fetchCache() {
				return (T) mockClientCache;
			}
		};

		assertThat(clientCacheFactoryBean.getReadyForEvents(), is(nullValue()));
		assertThat(clientCacheFactoryBean.isReadyForEvents(), is(false));

		verify(mockClientCache, times(1)).getDistributedSystem();
	}

	@Test
	public void clusterConfigurationNotUsed() {
		assertFalse(new ClientCacheFactoryBean().getUseClusterConfiguration());
	}

	@Test(expected = UnsupportedOperationException.class)
	public void useClusterConfiguration() {
		new ClientCacheFactoryBean().setUseClusterConfiguration(true);
	}

}
