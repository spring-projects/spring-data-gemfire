/*
 * Copyright 2010-2019 the original author or authors.
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
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.net.InetSocketAddress;
import java.util.Arrays;
import java.util.Collections;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.geode.cache.CacheClosedException;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientCacheFactory;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.distributed.DistributedSystem;
import org.apache.geode.pdx.PdxSerializer;
import org.junit.Test;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.data.gemfire.util.DistributedSystemUtils;

/**
 * Unit tests for {@link ClientCacheFactoryBean}
 *
 * @author John Blum
 * @see java.net.InetSocketAddress
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.ClientCacheFactory
 * @see org.apache.geode.cache.client.Pool
 * @see org.apache.geode.distributed.DistributedSystem
 * @see org.apache.geode.pdx.PdxSerializer
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
 * @since 1.7.0
 */
public class ClientCacheFactoryBeanTest {

	private Properties createProperties(String key, String value) {
		return addProperty(null, key, value);
	}

	@SuppressWarnings("all")
	private Properties addProperty(Properties properties, String key, String value) {

		properties = Optional.ofNullable(properties).orElseGet(Properties::new);
		properties.setProperty(key, value);

		return properties;
	}

	private ConnectionEndpoint newConnectionEndpoint(String host, int port) {
		return new ConnectionEndpoint(host, port);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void getObjectType() {
		assertThat(new ClientCacheFactoryBean().getObjectType(), is(equalTo(ClientCache.class)));
	}

	@Test
	public void isSingleton() {
		assertThat(new ClientCacheFactoryBean().isSingleton(), is(true));
	}

	@Test
	public void resolvePropertiesWhenDistributedSystemIsConnected() {

		Properties gemfireProperties = createProperties("gf", "test");
		Properties distributedSystemProperties = createProperties("ds", "mock");

		DistributedSystem mockDistributedSystem = mock(DistributedSystem.class);

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

		Properties gemfireProperties = DistributedSystemUtils.configureDurableClient(
			createProperties("gf", "test"), "123", 600);

		Properties distributedSystemProperties = DistributedSystemUtils.configureDurableClient(
			createProperties("ds", "mock"), "987", 300);

		DistributedSystem mockDistributedSystem = mock(DistributedSystem.class);

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

		DistributedSystem mockDistributedSystem = mock(DistributedSystem.class);

		when(mockDistributedSystem.isConnected()).thenReturn(false);
		when(mockDistributedSystem.getProperties()).thenReturn(distributedSystemProperties);

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@SuppressWarnings("unchecked") @Override <T extends DistributedSystem> T getDistributedSystem() {
				return (T) mockDistributedSystem;
			}
		};

		clientCacheFactoryBean.setProperties(gemfireProperties);

		Properties resolvedProperties = clientCacheFactoryBean.resolveProperties();

		assertThat(resolvedProperties, is(sameInstance(gemfireProperties)));

		verify(mockDistributedSystem, times(1)).isConnected();
		verify(mockDistributedSystem, never()).getProperties();
	}

	@Test
	public void resolvePropertiesWhenDistributedSystemIsNull() {

		Properties gemfireProperties = createProperties("gf", "test");

		assertThat(gemfireProperties.size(), is(equalTo(1)));

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override <T extends DistributedSystem> T getDistributedSystem() {
				return null;
			}
		};

		clientCacheFactoryBean.setDurableClientId("123");
		clientCacheFactoryBean.setProperties(gemfireProperties);

		Properties resolvedProperties = clientCacheFactoryBean.resolveProperties();

		assertThat(resolvedProperties, is(sameInstance(gemfireProperties)));
		assertThat(resolvedProperties.size(), is(equalTo(2)));
		assertThat(resolvedProperties.getProperty("gf"), is(equalTo("test")));
		assertThat(resolvedProperties.getProperty(DistributedSystemUtils.DURABLE_CLIENT_ID_PROPERTY_NAME),
			is(equalTo("123")));
	}

	@Test
	public void createClientCacheFactory() {

		Properties gemfireProperties = new Properties();

		Object clientCacheFactoryReference = new ClientCacheFactoryBean().createFactory(gemfireProperties);

		assertThat(clientCacheFactoryReference, is(instanceOf(ClientCacheFactory.class)));
		assertThat(gemfireProperties.isEmpty(), is(true));

		ClientCacheFactory clientCacheFactory = (ClientCacheFactory) clientCacheFactoryReference;

		clientCacheFactory.set("testCase", "TestCreateClientCacheFactory");

		assertThat(gemfireProperties.containsKey("testCase"), is(true));
		assertThat(gemfireProperties.getProperty("testCase"), is(equalTo("TestCreateClientCacheFactory")));
	}

	@Test
	public void prepareClientCacheFactoryCallsInitializePdxAndInitializePool() {

		AtomicBoolean initializePdxCalled = new AtomicBoolean(false);
		AtomicBoolean initializePoolCalled = new AtomicBoolean(false);

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class);

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {

			@Override
			ClientCacheFactory configurePdx(ClientCacheFactory clientCacheFactory) {
				initializePdxCalled.set(true);
				return clientCacheFactory;
			}

			@Override
			ClientCacheFactory configurePool(final ClientCacheFactory clientCacheFactory) {
				initializePoolCalled.set(true);
				return clientCacheFactory;
			}
		};

		assertThat(clientCacheFactoryBean.configureFactory(mockClientCacheFactory),
			is(sameInstance(mockClientCacheFactory)));
		assertThat(initializePdxCalled.get(), is(true));
		assertThat(initializePoolCalled.get(), is(true));
	}

	@Test
	public void initializePdxWithAllPdxOptions() {

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		PdxSerializer mockPdxSerializer = mock(PdxSerializer.class);

		clientCacheFactoryBean.setPdxDiskStoreName("MockPdxDiskStoreName");
		clientCacheFactoryBean.setPdxIgnoreUnreadFields(false);
		clientCacheFactoryBean.setPdxPersistent(true);
		clientCacheFactoryBean.setPdxReadSerialized(false);
		clientCacheFactoryBean.setPdxSerializer(mockPdxSerializer);

		assertThat(clientCacheFactoryBean.getPdxDiskStoreName(), is(equalTo("MockPdxDiskStoreName")));
		assertThat(clientCacheFactoryBean.getPdxIgnoreUnreadFields(), is(false));
		assertThat(clientCacheFactoryBean.getPdxPersistent(), is(true));
		assertThat(clientCacheFactoryBean.getPdxReadSerialized(), is(false));
		assertThat(clientCacheFactoryBean.getPdxSerializer(), is(sameInstance(mockPdxSerializer)));

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class);

		assertThat(clientCacheFactoryBean.configurePdx(mockClientCacheFactory),
			is(sameInstance(mockClientCacheFactory)));

		verify(mockClientCacheFactory, times(1)).setPdxSerializer(eq(mockPdxSerializer));
		verify(mockClientCacheFactory, times(1)).setPdxDiskStore(eq("MockPdxDiskStoreName"));
		verify(mockClientCacheFactory, times(1)).setPdxIgnoreUnreadFields(eq(false));
		verify(mockClientCacheFactory, times(1)).setPdxPersistent(eq(true));
		verify(mockClientCacheFactory, times(1)).setPdxReadSerialized(eq(false));
	}

	@Test
	public void initializePdxWithPartialPdxOptions() {

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setPdxReadSerialized(true);
		clientCacheFactoryBean.setPdxIgnoreUnreadFields(true);

		assertThat(clientCacheFactoryBean.getPdxDiskStoreName(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPdxIgnoreUnreadFields(), is(true));
		assertThat(clientCacheFactoryBean.getPdxPersistent(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPdxReadSerialized(), is(true));
		assertThat(clientCacheFactoryBean.getPdxSerializer(), is(nullValue()));

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class);

		assertThat(clientCacheFactoryBean.configurePdx(mockClientCacheFactory),
			is(sameInstance(mockClientCacheFactory)));

		verify(mockClientCacheFactory, never()).setPdxDiskStore(anyString());
		verify(mockClientCacheFactory, times(1)).setPdxIgnoreUnreadFields(eq(true));
		verify(mockClientCacheFactory, never()).setPdxPersistent(anyBoolean());
		verify(mockClientCacheFactory, times(1)).setPdxReadSerialized(eq(true));
		verify(mockClientCacheFactory, never()).setPdxSerializer(any(PdxSerializer.class));
	}

	@Test
	public void initializePdxWithNoPdxOptions() {

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		assertThat(clientCacheFactoryBean.getPdxDiskStoreName(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPdxIgnoreUnreadFields(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPdxPersistent(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPdxReadSerialized(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPdxSerializer(), is(nullValue()));

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class);

		assertThat(clientCacheFactoryBean.configurePdx(mockClientCacheFactory),
			is(sameInstance(mockClientCacheFactory)));

		verifyZeroInteractions(mockClientCacheFactory);
	}

	@Test
	public void initializePoolWithPool() {

		Pool mockPool = mock(Pool.class);

		when(mockPool.getFreeConnectionTimeout()).thenReturn(10000);
		when(mockPool.getIdleTimeout()).thenReturn(120000L);
		when(mockPool.getLoadConditioningInterval()).thenReturn(30000);
		when(mockPool.getLocators()).thenReturn(Collections.emptyList());
		when(mockPool.getMaxConnections()).thenReturn(100);
		when(mockPool.getMinConnections()).thenReturn(10);
		when(mockPool.getMultiuserAuthentication()).thenReturn(true);
		when(mockPool.getPRSingleHopEnabled()).thenReturn(true);
		when(mockPool.getPingInterval()).thenReturn(15000L);
		when(mockPool.getReadTimeout()).thenReturn(20000);
		when(mockPool.getRetryAttempts()).thenReturn(1);
		when(mockPool.getServerGroup()).thenReturn("TestGroup");
		when(mockPool.getSocketBufferSize()).thenReturn(8192);
		when(mockPool.getSocketConnectTimeout()).thenReturn(5000);
		when(mockPool.getStatisticInterval()).thenReturn(5000);
		when(mockPool.getSubscriptionAckInterval()).thenReturn(500);
		when(mockPool.getSubscriptionEnabled()).thenReturn(true);
		when(mockPool.getSubscriptionMessageTrackingTimeout()).thenReturn(500);
		when(mockPool.getSubscriptionRedundancy()).thenReturn(2);
		when(mockPool.getThreadLocalConnections()).thenReturn(false);
		when(mockPool.getServers()).thenReturn(Arrays.asList(
			new InetSocketAddress("localhost", 11235), new InetSocketAddress("localhost", 12480)));

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setPool(mockPool);

		assertThat(clientCacheFactoryBean.getFreeConnectionTimeout(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getIdleTimeout(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getLoadConditioningInterval(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getLocators().isEmpty(), is(true));
		assertThat(clientCacheFactoryBean.getMaxConnections(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getMinConnections(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getMultiUserAuthentication(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPingInterval(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPool(), is(sameInstance(mockPool)));
		assertThat(clientCacheFactoryBean.getPoolName(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPrSingleHopEnabled(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getReadTimeout(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getRetryAttempts(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getServerGroup(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getServers().isEmpty(), is(true));
		assertThat(clientCacheFactoryBean.getSocketBufferSize(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getSocketConnectTimeout(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getStatisticsInterval(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getSubscriptionAckInterval(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getSubscriptionEnabled(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getSubscriptionMessageTrackingTimeout(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getSubscriptionRedundancy(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getThreadLocalConnections(), is(nullValue()));

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class);

		assertThat(clientCacheFactoryBean.configurePool(mockClientCacheFactory),
			is(sameInstance(mockClientCacheFactory)));

		verify(mockPool, times(1)).getFreeConnectionTimeout();
		verify(mockPool, times(1)).getIdleTimeout();
		verify(mockPool, times(1)).getLoadConditioningInterval();
		verify(mockPool, never()).getLocators();
		verify(mockPool, times(1)).getMaxConnections();
		verify(mockPool, times(1)).getMinConnections();
		verify(mockPool, times(1)).getMultiuserAuthentication();
		verify(mockPool, times(1)).getPRSingleHopEnabled();
		verify(mockPool, times(1)).getPingInterval();
		verify(mockPool, times(1)).getReadTimeout();
		verify(mockPool, times(1)).getRetryAttempts();
		verify(mockPool, times(1)).getServerGroup();
		verify(mockPool, times(1)).getServers();
		verify(mockPool, times(1)).getSocketBufferSize();
		verify(mockPool, times(1)).getSocketConnectTimeout();
		verify(mockPool, times(1)).getStatisticInterval();
		verify(mockPool, times(1)).getSubscriptionAckInterval();
		verify(mockPool, times(1)).getSubscriptionEnabled();
		verify(mockPool, times(1)).getSubscriptionMessageTrackingTimeout();
		verify(mockPool, times(1)).getSubscriptionRedundancy();
		verify(mockPool, times(1)).getThreadLocalConnections();
		verify(mockClientCacheFactory, times(1)).setPoolFreeConnectionTimeout(eq(10000));
		verify(mockClientCacheFactory, times(1)).setPoolIdleTimeout(eq(120000L));
		verify(mockClientCacheFactory, times(1)).setPoolLoadConditioningInterval(eq(30000));
		verify(mockClientCacheFactory, times(1)).setPoolMaxConnections(eq(100));
		verify(mockClientCacheFactory, times(1)).setPoolMinConnections(eq(10));
		verify(mockClientCacheFactory, times(1)).setPoolMultiuserAuthentication(eq(true));
		verify(mockClientCacheFactory, times(1)).setPoolPRSingleHopEnabled(eq(true));
		verify(mockClientCacheFactory, times(1)).setPoolPingInterval(eq(15000L));
		verify(mockClientCacheFactory, times(1)).setPoolReadTimeout(eq(20000));
		verify(mockClientCacheFactory, times(1)).setPoolRetryAttempts(eq(1));
		verify(mockClientCacheFactory, times(1)).setPoolServerGroup(eq("TestGroup"));
		verify(mockClientCacheFactory, times(1)).setPoolSocketBufferSize(eq(8192));
		verify(mockClientCacheFactory, times(1)).setPoolSocketConnectTimeout(eq(5000));
		verify(mockClientCacheFactory, times(1)).setPoolStatisticInterval(eq(5000));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionAckInterval(eq(500));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionEnabled(eq(true));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionMessageTrackingTimeout(eq(500));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionRedundancy(eq(2));
		verify(mockClientCacheFactory, times(1)).setPoolThreadLocalConnections(eq(false));
		verify(mockClientCacheFactory, times(1)).addPoolServer(eq("localhost"), eq(11235));
		verify(mockClientCacheFactory, times(1)).addPoolServer(eq("localhost"), eq(12480));
		verify(mockClientCacheFactory, never()).addPoolLocator(anyString(), anyInt());
	}

	@Test
	public void initializePoolWithFactory() {

		Pool mockPool = mock(Pool.class);

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setFreeConnectionTimeout(5000);
		clientCacheFactoryBean.setIdleTimeout(300000L);
		clientCacheFactoryBean.setLoadConditioningInterval(120000);
		clientCacheFactoryBean.setMaxConnections(99);
		clientCacheFactoryBean.setMinConnections(9);
		clientCacheFactoryBean.setMultiUserAuthentication(true);
		clientCacheFactoryBean.setPingInterval(15000L);
		clientCacheFactoryBean.setPool(mockPool);
		clientCacheFactoryBean.setPrSingleHopEnabled(true);
		clientCacheFactoryBean.setReadTimeout(20000);
		clientCacheFactoryBean.setRetryAttempts(2);
		clientCacheFactoryBean.setServerGroup("TestGroup");
		clientCacheFactoryBean.setSocketBufferSize(16384);
		clientCacheFactoryBean.setSocketConnectTimeout(5000);
		clientCacheFactoryBean.setStatisticsInterval(1000);
		clientCacheFactoryBean.setSubscriptionAckInterval(100);
		clientCacheFactoryBean.setSubscriptionEnabled(true);
		clientCacheFactoryBean.setSubscriptionMessageTrackingTimeout(500);
		clientCacheFactoryBean.setSubscriptionRedundancy(2);
		clientCacheFactoryBean.setThreadLocalConnections(false);
		clientCacheFactoryBean.addLocators(newConnectionEndpoint("localhost", 11235),
			newConnectionEndpoint("skullbox", 10334));

		assertThat(clientCacheFactoryBean.getFreeConnectionTimeout(), is(equalTo(5000)));
		assertThat(clientCacheFactoryBean.getIdleTimeout(), is(equalTo(300000L)));
		assertThat(clientCacheFactoryBean.getLoadConditioningInterval(), is(equalTo(120000)));
		assertThat(clientCacheFactoryBean.getLocators().size(), is(equalTo(2)));
		assertThat(clientCacheFactoryBean.getMaxConnections(), is(equalTo(99)));
		assertThat(clientCacheFactoryBean.getMinConnections(), is(equalTo(9)));
		assertThat(clientCacheFactoryBean.getMultiUserAuthentication(), is(equalTo(true)));
		assertThat(clientCacheFactoryBean.getPingInterval(), is(equalTo(15000L)));
		assertThat(clientCacheFactoryBean.getPool(), is(sameInstance(mockPool)));
		assertThat(clientCacheFactoryBean.getPoolName(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPrSingleHopEnabled(), is(equalTo(true)));
		assertThat(clientCacheFactoryBean.getReadTimeout(), is(equalTo(20000)));
		assertThat(clientCacheFactoryBean.getRetryAttempts(), is(equalTo(2)));
		assertThat(clientCacheFactoryBean.getServerGroup(), is(equalTo("TestGroup")));
		assertThat(clientCacheFactoryBean.getServers().isEmpty(), is(true));
		assertThat(clientCacheFactoryBean.getSocketBufferSize(), is(equalTo(16384)));
		assertThat(clientCacheFactoryBean.getSocketConnectTimeout(), is(equalTo(5000)));
		assertThat(clientCacheFactoryBean.getStatisticsInterval(), is(equalTo(1000)));
		assertThat(clientCacheFactoryBean.getSubscriptionAckInterval(), is(equalTo(100)));
		assertThat(clientCacheFactoryBean.getSubscriptionEnabled(), is(equalTo(true)));
		assertThat(clientCacheFactoryBean.getSubscriptionMessageTrackingTimeout(), is(equalTo(500)));
		assertThat(clientCacheFactoryBean.getSubscriptionRedundancy(), is(equalTo(2)));
		assertThat(clientCacheFactoryBean.getThreadLocalConnections(), is(equalTo(false)));

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class);

		assertThat(clientCacheFactoryBean.configurePool(mockClientCacheFactory),
			is(sameInstance(mockClientCacheFactory)));

		verifyZeroInteractions(mockPool);
		verify(mockClientCacheFactory, times(1)).setPoolFreeConnectionTimeout(eq(5000));
		verify(mockClientCacheFactory, times(1)).setPoolIdleTimeout(eq(300000L));
		verify(mockClientCacheFactory, times(1)).setPoolLoadConditioningInterval(eq(120000));
		verify(mockClientCacheFactory, times(1)).setPoolMaxConnections(eq(99));
		verify(mockClientCacheFactory, times(1)).setPoolMinConnections(eq(9));
		verify(mockClientCacheFactory, times(1)).setPoolMultiuserAuthentication(eq(true));
		verify(mockClientCacheFactory, times(1)).setPoolPingInterval(eq(15000L));
		verify(mockClientCacheFactory, times(1)).setPoolPRSingleHopEnabled(eq(true));
		verify(mockClientCacheFactory, times(1)).setPoolReadTimeout(eq(20000));
		verify(mockClientCacheFactory, times(1)).setPoolRetryAttempts(eq(2));
		verify(mockClientCacheFactory, times(1)).setPoolServerGroup(eq("TestGroup"));
		verify(mockClientCacheFactory, times(1)).setPoolSocketBufferSize(eq(16384));
		verify(mockClientCacheFactory, times(1)).setPoolSocketConnectTimeout(eq(5000));
		verify(mockClientCacheFactory, times(1)).setPoolStatisticInterval(eq(1000));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionAckInterval(eq(100));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionEnabled(eq(true));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionMessageTrackingTimeout(eq(500));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionRedundancy(eq(2));
		verify(mockClientCacheFactory, times(1)).setPoolThreadLocalConnections(eq(false));
		verify(mockClientCacheFactory, times(1)).addPoolLocator(eq("localhost"), eq(11235));
		verify(mockClientCacheFactory, times(1)).addPoolLocator(eq("skullbox"), eq(10334));
		verify(mockClientCacheFactory, never()).addPoolServer(anyString(), anyInt());
	}

	@Test
	public void initializePoolWithFactoryAndPoolButFactoryOverridesPool() {

		Pool mockPool = mock(Pool.class);

		when(mockPool.getFreeConnectionTimeout()).thenReturn(5000);
		when(mockPool.getIdleTimeout()).thenReturn(120000L);
		when(mockPool.getLoadConditioningInterval()).thenReturn(300000);
		when(mockPool.getLocators()).thenReturn(Collections.emptyList());
		when(mockPool.getMaxConnections()).thenReturn(200);
		when(mockPool.getMinConnections()).thenReturn(10);
		when(mockPool.getMultiuserAuthentication()).thenReturn(false);
		when(mockPool.getPingInterval()).thenReturn(15000L);
		when(mockPool.getPRSingleHopEnabled()).thenReturn(false);
		when(mockPool.getReadTimeout()).thenReturn(30000);
		when(mockPool.getRetryAttempts()).thenReturn(1);
		when(mockPool.getServerGroup()).thenReturn("TestServerGroup");
		when(mockPool.getServers()).thenReturn(Collections.singletonList(new InetSocketAddress("localhost", 12480)));
		when(mockPool.getSocketBufferSize()).thenReturn(8192);
		when(mockPool.getSocketConnectTimeout()).thenReturn(5000);
		when(mockPool.getStatisticInterval()).thenReturn(5000);
		when(mockPool.getSubscriptionAckInterval()).thenReturn(1000);
		when(mockPool.getSubscriptionEnabled()).thenReturn(false);
		when(mockPool.getSubscriptionMessageTrackingTimeout()).thenReturn(20000);
		when(mockPool.getSubscriptionRedundancy()).thenReturn(1);
		when(mockPool.getThreadLocalConnections()).thenReturn(true);

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setIdleTimeout(180000L);
		clientCacheFactoryBean.setMaxConnections(500);
		clientCacheFactoryBean.setMinConnections(50);
		clientCacheFactoryBean.setMultiUserAuthentication(true);
		clientCacheFactoryBean.setPool(mockPool);
		clientCacheFactoryBean.setPrSingleHopEnabled(true);
		clientCacheFactoryBean.setServerGroup("TestGroup");
		clientCacheFactoryBean.setSocketBufferSize(16384);
		clientCacheFactoryBean.setSocketConnectTimeout(10000);
		clientCacheFactoryBean.setStatisticsInterval(500);
		clientCacheFactoryBean.setSubscriptionAckInterval(100);
		clientCacheFactoryBean.setSubscriptionEnabled(true);
		clientCacheFactoryBean.setSubscriptionRedundancy(2);
		clientCacheFactoryBean.setThreadLocalConnections(false);
		clientCacheFactoryBean.addLocators(newConnectionEndpoint("localhost", 11235));

		assertThat(clientCacheFactoryBean.getFreeConnectionTimeout(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getIdleTimeout(), is(equalTo(180000L)));
		assertThat(clientCacheFactoryBean.getLoadConditioningInterval(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getLocators().size(), is(equalTo(1)));
		assertThat(clientCacheFactoryBean.getMaxConnections(), is(equalTo(500)));
		assertThat(clientCacheFactoryBean.getMinConnections(), is(equalTo(50)));
		assertThat(clientCacheFactoryBean.getMultiUserAuthentication(), is(true));
		assertThat(clientCacheFactoryBean.getPingInterval(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPool(), is(sameInstance(mockPool)));
		assertThat(clientCacheFactoryBean.getPoolName(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPrSingleHopEnabled(), is(true));
		assertThat(clientCacheFactoryBean.getReadTimeout(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getRetryAttempts(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getServerGroup(), is(equalTo("TestGroup")));
		assertThat(clientCacheFactoryBean.getServers().isEmpty(), is(true));
		assertThat(clientCacheFactoryBean.getSocketBufferSize(), is(equalTo(16384)));
		assertThat(clientCacheFactoryBean.getSocketConnectTimeout(), is(equalTo(10000)));
		assertThat(clientCacheFactoryBean.getStatisticsInterval(), is(equalTo(500)));
		assertThat(clientCacheFactoryBean.getSubscriptionAckInterval(), is(equalTo(100)));
		assertThat(clientCacheFactoryBean.getSubscriptionEnabled(), is(true));
		assertThat(clientCacheFactoryBean.getSubscriptionMessageTrackingTimeout(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getSubscriptionRedundancy(), is(equalTo(2)));
		assertThat(clientCacheFactoryBean.getThreadLocalConnections(), is(false));

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class);

		assertThat(clientCacheFactoryBean.configurePool(mockClientCacheFactory),
			is(sameInstance(mockClientCacheFactory)));

		verify(mockPool, times(1)).getFreeConnectionTimeout();
		verify(mockPool, never()).getIdleTimeout();
		verify(mockPool, times(1)).getLoadConditioningInterval();
		verify(mockPool, never()).getLocators();
		verify(mockPool, never()).getMaxConnections();
		verify(mockPool, never()).getMinConnections();
		verify(mockPool, never()).getMultiuserAuthentication();
		verify(mockPool, times(1)).getPingInterval();
		verify(mockPool, never()).getPRSingleHopEnabled();
		verify(mockPool, times(1)).getReadTimeout();
		verify(mockPool, times(1)).getRetryAttempts();
		verify(mockPool, never()).getServerGroup();
		verify(mockPool, never()).getServers();
		verify(mockPool, never()).getSocketBufferSize();
		verify(mockPool, never()).getSocketConnectTimeout();
		verify(mockPool, never()).getStatisticInterval();
		verify(mockPool, never()).getSubscriptionAckInterval();
		verify(mockPool, never()).getSubscriptionEnabled();
		verify(mockPool, times(1)).getSubscriptionMessageTrackingTimeout();
		verify(mockPool, never()).getSubscriptionRedundancy();
		verify(mockPool, never()).getThreadLocalConnections();
		verify(mockClientCacheFactory, times(1)).setPoolFreeConnectionTimeout(eq(5000));
		verify(mockClientCacheFactory, times(1)).setPoolIdleTimeout(eq(180000L));
		verify(mockClientCacheFactory, times(1)).setPoolLoadConditioningInterval(eq(300000));
		verify(mockClientCacheFactory, times(1)).setPoolMaxConnections(eq(500));
		verify(mockClientCacheFactory, times(1)).setPoolMinConnections(eq(50));
		verify(mockClientCacheFactory, times(1)).setPoolMultiuserAuthentication(eq(true));
		verify(mockClientCacheFactory, times(1)).setPoolPingInterval(eq(15000L));
		verify(mockClientCacheFactory, times(1)).setPoolPRSingleHopEnabled(eq(true));
		verify(mockClientCacheFactory, times(1)).setPoolReadTimeout(eq(30000));
		verify(mockClientCacheFactory, times(1)).setPoolRetryAttempts(eq(1));
		verify(mockClientCacheFactory, times(1)).setPoolServerGroup(eq("TestGroup"));
		verify(mockClientCacheFactory, times(1)).setPoolSocketBufferSize(eq(16384));
		verify(mockClientCacheFactory, times(1)).setPoolSocketConnectTimeout(eq(10000));
		verify(mockClientCacheFactory, times(1)).setPoolStatisticInterval(eq(500));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionAckInterval(eq(100));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionEnabled(eq(true));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionMessageTrackingTimeout(eq(20000));
		verify(mockClientCacheFactory, times(1)).setPoolSubscriptionRedundancy(eq(2));
		verify(mockClientCacheFactory, times(1)).addPoolLocator(eq("localhost"), eq(11235));
		verify(mockClientCacheFactory, never()).addPoolServer(anyString(), anyInt());
	}

	@Test
	public void initializePoolWithFactoryLocator() {

		Pool mockPool = mock(Pool.class);

		when(mockPool.getLocators()).thenReturn(Collections.singletonList(new InetSocketAddress("localhost", 21668)));
		when(mockPool.getServers()).thenReturn(Collections.singletonList(new InetSocketAddress("localhost", 41414)));

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setPool(mockPool);
		clientCacheFactoryBean.addLocators(newConnectionEndpoint("boombox", 11235));

		assertThat(clientCacheFactoryBean.getLocators().size(), is(equalTo(1)));
		assertThat(clientCacheFactoryBean.getPool(), is(sameInstance(mockPool)));
		assertThat(clientCacheFactoryBean.getServers().isEmpty(), is(true));

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class);

		assertThat(clientCacheFactoryBean.configurePool(mockClientCacheFactory),
			is(sameInstance(mockClientCacheFactory)));

		verify(mockPool, never()).getLocators();
		verify(mockPool, never()).getServers();
		verify(mockClientCacheFactory, times(1)).addPoolLocator(eq("boombox"), eq(11235));
		verify(mockClientCacheFactory, never()).addPoolServer(anyString(), anyInt());
	}

	@Test
	public void initializePoolWithFactoryServer() {

		Pool mockPool = mock(Pool.class);

		when(mockPool.getLocators()).thenReturn(Collections.singletonList(new InetSocketAddress("localhost", 21668)));
		when(mockPool.getServers()).thenReturn(Collections.singletonList(new InetSocketAddress("localhost", 41414)));

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setPool(mockPool);
		clientCacheFactoryBean.addServers(newConnectionEndpoint("skullbox", 12480));

		assertThat(clientCacheFactoryBean.getLocators().isEmpty(), is(true));
		assertThat(clientCacheFactoryBean.getPool(), is(sameInstance(mockPool)));
		assertThat(clientCacheFactoryBean.getServers().size(), is(equalTo(1)));

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class);

		assertThat(clientCacheFactoryBean.configurePool(mockClientCacheFactory),
			is(sameInstance(mockClientCacheFactory)));

		verify(mockPool, never()).getLocators();
		verify(mockPool, never()).getServers();
		verify(mockClientCacheFactory, never()).addPoolLocator(anyString(), anyInt());
		verify(mockClientCacheFactory, times(1)).addPoolServer(eq("skullbox"), eq(12480));
	}

	@Test
	public void initializePoolWithPoolLocator() {

		Pool mockPool = mock(Pool.class);

		when(mockPool.getLocators()).thenReturn(Collections.singletonList(new InetSocketAddress("skullbox", 21668)));
		when(mockPool.getServers()).thenReturn(Collections.emptyList());

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setPool(mockPool);

		assertThat(clientCacheFactoryBean.getLocators().isEmpty(), is(true));
		assertThat(clientCacheFactoryBean.getPool(), is(sameInstance(mockPool)));
		assertThat(clientCacheFactoryBean.getServers().isEmpty(), is(true));

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class);

		assertThat(clientCacheFactoryBean.configurePool(mockClientCacheFactory),
			is(sameInstance(mockClientCacheFactory)));

		verify(mockPool, times(1)).getLocators();
		verify(mockPool, times(1)).getServers();
		verify(mockClientCacheFactory, times(1)).addPoolLocator(eq("skullbox"), eq(21668));
		verify(mockClientCacheFactory, never()).addPoolServer(anyString(), anyInt());
	}

	@Test
	public void initializePoolWithPoolServer() {

		Pool mockPool = mock(Pool.class);

		when(mockPool.getLocators()).thenReturn(Collections.emptyList());
		when(mockPool.getServers()).thenReturn(Collections.singletonList(new InetSocketAddress("boombox", 41414)));

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setPool(mockPool);

		assertThat(clientCacheFactoryBean.getLocators().isEmpty(), is(true));
		assertThat(clientCacheFactoryBean.getPool(), is(sameInstance(mockPool)));
		assertThat(clientCacheFactoryBean.getServers().isEmpty(), is(true));

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class);

		assertThat(clientCacheFactoryBean.configurePool(mockClientCacheFactory),
			is(sameInstance(mockClientCacheFactory)));

		verify(mockPool, never()).getLocators();
		verify(mockPool, times(1)).getServers();
		verify(mockClientCacheFactory, never()).addPoolLocator(anyString(), anyInt());
		verify(mockClientCacheFactory, times(1)).addPoolServer(eq("boombox"), eq(41414));
	}

	@Test
	public void initializePoolWithDefaultServer() {

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override Pool resolvePool() {
				return null;
			}
		};

		assertThat(clientCacheFactoryBean.getLocators().isEmpty(), is(true));
		assertThat(clientCacheFactoryBean.getPool(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getServers().isEmpty(), is(true));

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class);

		assertThat(clientCacheFactoryBean.configurePool(mockClientCacheFactory),
			is(sameInstance(mockClientCacheFactory)));

		verify(mockClientCacheFactory, never()).addPoolLocator(anyString(), anyInt());
		verify(mockClientCacheFactory, times(1)).addPoolServer(eq("localhost"),
			eq(GemfireUtils.DEFAULT_CACHE_SERVER_PORT));
	}

	@Test
	public void createCache() {

		ClientCache mockClientCache = mock(ClientCache.class);

		ClientCacheFactory mockClientCacheFactory = mock(ClientCacheFactory.class);

		when(mockClientCacheFactory.create()).thenReturn(mockClientCache);

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		assertThat(clientCacheFactoryBean.createCache(mockClientCacheFactory), is(sameInstance(mockClientCache)));

		verify(mockClientCacheFactory, times(1)).create();
		verifyZeroInteractions(mockClientCache);
	}

	@Test
	public void resolvePoolReturnsConfiguredPool() {

		Pool mockPool = mock(Pool.class);

		ClientCacheFactoryBean clientCacheFactoryBean = spy(new ClientCacheFactoryBean());

		clientCacheFactoryBean.setPool(mockPool);

		assertThat(clientCacheFactoryBean.getPool(), is(sameInstance(mockPool)));
		assertThat(clientCacheFactoryBean.resolvePool(), is(equalTo(mockPool)));

		verify(clientCacheFactoryBean, never()).getPoolName();
		verifyZeroInteractions(mockPool);
	}

	@Test
	public void resolvesPoolReturnsNamedPool() {

		Pool mockPool = mock(Pool.class);

		ClientCacheFactoryBean clientCacheFactoryBean = spy(new ClientCacheFactoryBean());

		when(clientCacheFactoryBean.findPool(eq("TestPool"))).thenReturn(mockPool);

		clientCacheFactoryBean.setPoolName("TestPool");

		assertThat(clientCacheFactoryBean.getPool(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPoolName(), is(equalTo("TestPool")));
		assertThat(clientCacheFactoryBean.resolvePool(), is(equalTo(mockPool)));

		verify(clientCacheFactoryBean, times(1)).findPool(eq("TestPool"));
		verifyZeroInteractions(mockPool);
	}

	@Test
	public void resolvesPoolReturnsNamedPoolFromBeanFactory() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		Pool mockPool = mock(Pool.class);

		PoolFactoryBean mockPoolFactoryBean = mock(PoolFactoryBean.class);

		when(mockBeanFactory.containsBean(eq("TestPool"))).thenReturn(true);
		when(mockBeanFactory.getBean(eq("&TestPool"), eq(PoolFactoryBean.class))).thenReturn(mockPoolFactoryBean);
		when(mockPoolFactoryBean.getPool()).thenReturn(mockPool);

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setBeanFactory(mockBeanFactory);
		clientCacheFactoryBean.setPoolName("TestPool");

		assertThat(clientCacheFactoryBean.getBeanFactory(), is(equalTo(mockBeanFactory)));
		assertThat(clientCacheFactoryBean.getPool(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPoolName(), is(equalTo("TestPool")));
		assertThat(clientCacheFactoryBean.resolvePool(), is(equalTo(mockPool)));

		verify(mockBeanFactory, times(1)).containsBean(eq("TestPool"));
		verify(mockBeanFactory, times(1))
			.getBean(eq("&TestPool"), eq(PoolFactoryBean.class));
		verify(mockPoolFactoryBean, times(1)).getPool();
		verifyZeroInteractions(mockPool);
	}

	@Test
	public void resolvePoolWhenBeanFactoryHasNoPoolBeansReturnsNull() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		when(mockBeanFactory.containsBean(anyString())).thenReturn(false);

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setBeanFactory(mockBeanFactory);
		clientCacheFactoryBean.setPoolName("TestPool");

		assertThat(clientCacheFactoryBean.getBeanFactory(), is(equalTo(mockBeanFactory)));
		assertThat(clientCacheFactoryBean.getPool(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPoolName(), is(equalTo("TestPool")));
		assertThat(clientCacheFactoryBean.resolvePool(), is(nullValue(Pool.class)));

		verify(mockBeanFactory, times(1)).containsBean(eq("TestPool"));
		verify(mockBeanFactory, never()).getBean(anyString(), eq(PoolFactoryBean.class));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void onApplicationEventCallsClientCacheReadyForEvents() {

		ClientCache mockClientCache = mock(ClientCache.class);

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override protected <T extends GemFireCache> T fetchCache() {
				return (T) mockClientCache;
			}
		};

		clientCacheFactoryBean.setReadyForEvents(true);

		assertThat(clientCacheFactoryBean.isReadyForEvents(), is(true));

		clientCacheFactoryBean.onApplicationEvent(mock(ContextRefreshedEvent.class, "MockContextRefreshedEvent"));

		verify(mockClientCache, times(1)).readyForEvents();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void onApplicationEventDoesNotCallClientCacheReadyForEventsWhenClientCacheFactoryBeanReadyForEventsIsFalse() {

		ClientCache mockClientCache = mock(ClientCache.class);

		doThrow(new RuntimeException("test")).when(mockClientCache).readyForEvents();

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override protected <T extends GemFireCache> T fetchCache() {
				return (T) mockClientCache;
			}
		};

		clientCacheFactoryBean.setReadyForEvents(false);

		assertThat(clientCacheFactoryBean.isReadyForEvents(), is(false));

		clientCacheFactoryBean.onApplicationEvent(mock(ContextRefreshedEvent.class, "MockContextRefreshedEvent"));

		verify(mockClientCache, never()).readyForEvents();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void onApplicationEventHandlesIllegalStateException() {

		ClientCache mockClientCache = mock(ClientCache.class);

		doThrow(new IllegalStateException("test")).when(mockClientCache).readyForEvents();

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override protected <T extends GemFireCache> T fetchCache() {
				return (T) mockClientCache;
			}
		};

		clientCacheFactoryBean.setReadyForEvents(true);

		assertThat(clientCacheFactoryBean.isReadyForEvents(), is(true));

		clientCacheFactoryBean.onApplicationEvent(mock(ContextRefreshedEvent.class));

		verify(mockClientCache, times(1)).readyForEvents();
	}

	@Test
	public void onApplicationEventHandlesCacheClosedException() {

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override protected <T extends GemFireCache> T fetchCache() {
				throw new CacheClosedException("test");
			}
		};

		clientCacheFactoryBean.setReadyForEvents(true);

		assertThat(clientCacheFactoryBean.isReadyForEvents(), is(true));

		clientCacheFactoryBean.onApplicationEvent(mock(ContextRefreshedEvent.class));
	}

	@Test
	public void closeClientCacheWithKeepAlive() {

		ClientCache mockClientCache = mock(ClientCache.class);

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setKeepAlive(true);

		assertThat(clientCacheFactoryBean.isKeepAlive(), is(true));

		clientCacheFactoryBean.close(mockClientCache);

		verify(mockClientCache, times(1)).close(eq(true));
	}

	@Test
	public void closeClientCacheWithoutKeepAlive() {

		ClientCache mockClientCache = mock(ClientCache.class);

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setKeepAlive(false);

		assertThat(clientCacheFactoryBean.isKeepAlive(), is(false));

		clientCacheFactoryBean.close(mockClientCache);

		verify(mockClientCache, times(1)).close(eq(false));
	}

	@Test
	public void autoReconnectDisabled() {
		assertThat(new ClientCacheFactoryBean().getEnableAutoReconnect(), is(false));
	}

	@Test(expected = UnsupportedOperationException.class)
	public void autoReconnectEnabled() {
		new ClientCacheFactoryBean().setEnableAutoReconnect(true);
	}

	@Test
	public void clusterConfigurationNotUsed() {
		assertThat(new ClientCacheFactoryBean().getUseClusterConfiguration(), is(false));
	}

	@Test(expected = UnsupportedOperationException.class)
	public void usesClusterConfiguration() {
		new ClientCacheFactoryBean().setUseClusterConfiguration(true);
	}

	@Test
	public void setAndGetKeepAlive() {

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		assertThat(clientCacheFactoryBean.getKeepAlive(), is(false));
		assertThat(clientCacheFactoryBean.isKeepAlive(), is(false));

		clientCacheFactoryBean.setKeepAlive(true);

		assertThat(clientCacheFactoryBean.getKeepAlive(), is(true));
		assertThat(clientCacheFactoryBean.isKeepAlive(), is(true));

		clientCacheFactoryBean.setKeepAlive(null);

		assertThat(clientCacheFactoryBean.getKeepAlive(), is(nullValue()));
		assertThat(clientCacheFactoryBean.isKeepAlive(), is(false));
	}

	@Test
	public void setAndGetPool() {

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		Pool mockPool = mock(Pool.class);

		assertThat(clientCacheFactoryBean.getPool(), is(nullValue()));

		clientCacheFactoryBean.setPool(mockPool);

		assertThat(clientCacheFactoryBean.getPool(), is(sameInstance(mockPool)));

		clientCacheFactoryBean.setPool(null);

		assertThat(clientCacheFactoryBean.getPool(), is(nullValue()));
	}

	@Test
	public void setAndGetPoolName() {

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		assertThat(clientCacheFactoryBean.getPoolName(), is(nullValue()));

		clientCacheFactoryBean.setPoolName("TestPool");

		assertThat(clientCacheFactoryBean.getPoolName(), is(equalTo("TestPool")));

		clientCacheFactoryBean.setPoolName(null);

		assertThat(clientCacheFactoryBean.getPoolName(), is(nullValue()));
	}

	@Test
	public void setAndGetPoolSettings() {

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		assertThat(clientCacheFactoryBean.getFreeConnectionTimeout(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getIdleTimeout(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getLoadConditioningInterval(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getMaxConnections(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getMinConnections(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getMultiUserAuthentication(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPingInterval(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getPrSingleHopEnabled(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getReadTimeout(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getRetryAttempts(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getServerGroup(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getSocketBufferSize(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getSocketConnectTimeout(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getStatisticsInterval(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getSubscriptionAckInterval(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getSubscriptionEnabled(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getSubscriptionMessageTrackingTimeout(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getSubscriptionRedundancy(), is(nullValue()));
		assertThat(clientCacheFactoryBean.getThreadLocalConnections(), is(nullValue()));

		clientCacheFactoryBean.setFreeConnectionTimeout(5000);
		clientCacheFactoryBean.setIdleTimeout(120000L);
		clientCacheFactoryBean.setLoadConditioningInterval(300000);
		clientCacheFactoryBean.setMaxConnections(500);
		clientCacheFactoryBean.setMinConnections(50);
		clientCacheFactoryBean.setMultiUserAuthentication(true);
		clientCacheFactoryBean.setPingInterval(15000L);
		clientCacheFactoryBean.setPrSingleHopEnabled(true);
		clientCacheFactoryBean.setReadTimeout(30000);
		clientCacheFactoryBean.setRetryAttempts(1);
		clientCacheFactoryBean.setServerGroup("test");
		clientCacheFactoryBean.setSocketBufferSize(16384);
		clientCacheFactoryBean.setSocketConnectTimeout(5000);
		clientCacheFactoryBean.setStatisticsInterval(500);
		clientCacheFactoryBean.setSubscriptionAckInterval(200);
		clientCacheFactoryBean.setSubscriptionEnabled(true);
		clientCacheFactoryBean.setSubscriptionMessageTrackingTimeout(20000);
		clientCacheFactoryBean.setSubscriptionRedundancy(2);
		clientCacheFactoryBean.setThreadLocalConnections(false);

		assertThat(clientCacheFactoryBean.getFreeConnectionTimeout(), is(equalTo(5000)));
		assertThat(clientCacheFactoryBean.getIdleTimeout(), is(equalTo(120000L)));
		assertThat(clientCacheFactoryBean.getLoadConditioningInterval(), is(equalTo(300000)));
		assertThat(clientCacheFactoryBean.getMaxConnections(), is(equalTo(500)));
		assertThat(clientCacheFactoryBean.getMinConnections(), is(equalTo(50)));
		assertThat(clientCacheFactoryBean.getMultiUserAuthentication(), is(equalTo(true)));
		assertThat(clientCacheFactoryBean.getPingInterval(), is(equalTo(15000L)));
		assertThat(clientCacheFactoryBean.getPrSingleHopEnabled(), is(equalTo(true)));
		assertThat(clientCacheFactoryBean.getReadTimeout(), is(equalTo(30000)));
		assertThat(clientCacheFactoryBean.getRetryAttempts(), is(equalTo(1)));
		assertThat(clientCacheFactoryBean.getServerGroup(), is(equalTo("test")));
		assertThat(clientCacheFactoryBean.getSocketBufferSize(), is(16384));
		assertThat(clientCacheFactoryBean.getSocketConnectTimeout(), is(5000));
		assertThat(clientCacheFactoryBean.getStatisticsInterval(), is(equalTo(500)));
		assertThat(clientCacheFactoryBean.getSubscriptionAckInterval(), is(equalTo(200)));
		assertThat(clientCacheFactoryBean.getSubscriptionEnabled(), is(equalTo(true)));
		assertThat(clientCacheFactoryBean.getSubscriptionMessageTrackingTimeout(), is(equalTo(20000)));
		assertThat(clientCacheFactoryBean.getSubscriptionRedundancy(), is(equalTo(2)));
		assertThat(clientCacheFactoryBean.getThreadLocalConnections(), is(equalTo(false)));
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

	private ClientCache mockClientCache(String durableClientId) {

		ClientCache mockClientCache = mock(ClientCache.class);

		DistributedSystem mockDistributedSystem = mock(DistributedSystem.class);

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

		ClientCache mockClientCache = mockClientCache("");

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override protected <T extends GemFireCache> T fetchCache() {
				return (T) mockClientCache;
			}
		};

		clientCacheFactoryBean.setReadyForEvents(true);

		assertThat(clientCacheFactoryBean.getReadyForEvents(), is(true));
		assertThat(clientCacheFactoryBean.isReadyForEvents(), is(true));

		verifyZeroInteractions(mockClientCache);
	}

	@Test
	@SuppressWarnings("unchecked")
	public void isReadyForEventsIsFalseWhenClientCacheFactoryBeanReadyForEventsIsFalse() {

		ClientCache mockClientCache = mockClientCache("TestDurableClientId");

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override protected <T extends GemFireCache> T fetchCache() {
				return (T) mockClientCache;
			}
		};

		clientCacheFactoryBean.setReadyForEvents(false);

		assertThat(clientCacheFactoryBean.getReadyForEvents(), is(false));
		assertThat(clientCacheFactoryBean.isReadyForEvents(), is(false));

		verifyZeroInteractions(mockClientCache);
	}

	@Test
	public void isReadyForEventsIsFalseWhenClientCacheNotInitialized() {

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean() {
			@Override protected <T extends GemFireCache> T fetchCache() {
				throw new CacheClosedException("test");
			}
		};

		assertThat(clientCacheFactoryBean.getReadyForEvents(), is(nullValue()));
		assertThat(clientCacheFactoryBean.isReadyForEvents(), is(false));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void isReadyForEventsIsTrueWhenDurableClientIdIsSet() {

		ClientCache mockClientCache = mockClientCache("TestDurableClientId");

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

		ClientCache mockClientCache = mockClientCache("  ");

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
	public void addSetAndGetLocators() {

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		assertThat(clientCacheFactoryBean.getLocators(), is(notNullValue()));
		assertThat(clientCacheFactoryBean.getLocators().isEmpty(), is(true));

		ConnectionEndpoint localhost = newConnectionEndpoint("localhost", 21668);

		clientCacheFactoryBean.addLocators(localhost);

		assertThat(clientCacheFactoryBean.getLocators().size(), is(equalTo(1)));
		assertThat(clientCacheFactoryBean.getLocators().findOne("localhost"), is(equalTo(localhost)));

		ConnectionEndpoint skullbox = newConnectionEndpoint("skullbox", 10334);
		ConnectionEndpoint boombox = newConnectionEndpoint("boombox", 10334);

		clientCacheFactoryBean.addLocators(skullbox, boombox);

		assertThat(clientCacheFactoryBean.getLocators().size(), is(equalTo(3)));
		assertThat(clientCacheFactoryBean.getLocators().findOne("localhost"), is(equalTo(localhost)));
		assertThat(clientCacheFactoryBean.getLocators().findOne("skullbox"), is(equalTo(skullbox)));
		assertThat(clientCacheFactoryBean.getLocators().findOne("boombox"), is(equalTo(boombox)));

		clientCacheFactoryBean.setLocators(ArrayUtils.asArray(localhost));

		assertThat(clientCacheFactoryBean.getLocators().size(), is(equalTo(1)));
		assertThat(clientCacheFactoryBean.getLocators().findOne("localhost"), is(equalTo(localhost)));

		clientCacheFactoryBean.setLocators(Collections.emptyList());

		assertThat(clientCacheFactoryBean.getLocators(), is(notNullValue()));
		assertThat(clientCacheFactoryBean.getLocators().isEmpty(), is(true));
	}

	@Test
	public void addSetAndGetServers() {

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		assertThat(clientCacheFactoryBean.getServers(), is(notNullValue()));
		assertThat(clientCacheFactoryBean.getServers().isEmpty(), is(true));

		ConnectionEndpoint localhost = newConnectionEndpoint("localhost", 21668);

		clientCacheFactoryBean.addServers(localhost);

		assertThat(clientCacheFactoryBean.getServers().size(), is(equalTo(1)));
		assertThat(clientCacheFactoryBean.getServers().findOne("localhost"), is(equalTo(localhost)));

		ConnectionEndpoint skullbox = newConnectionEndpoint("skullbox", 10334);
		ConnectionEndpoint boombox = newConnectionEndpoint("boombox", 10334);

		clientCacheFactoryBean.addServers(skullbox, boombox);

		assertThat(clientCacheFactoryBean.getServers().size(), is(equalTo(3)));
		assertThat(clientCacheFactoryBean.getServers().findOne("localhost"), is(equalTo(localhost)));
		assertThat(clientCacheFactoryBean.getServers().findOne("skullbox"), is(equalTo(skullbox)));
		assertThat(clientCacheFactoryBean.getServers().findOne("boombox"), is(equalTo(boombox)));

		clientCacheFactoryBean.setServers(ArrayUtils.asArray(localhost));

		assertThat(clientCacheFactoryBean.getServers().size(), is(equalTo(1)));
		assertThat(clientCacheFactoryBean.getServers().findOne("localhost"), is(equalTo(localhost)));

		clientCacheFactoryBean.setServers(Collections.emptyList());

		assertThat(clientCacheFactoryBean.getServers(), is(notNullValue()));
		assertThat(clientCacheFactoryBean.getServers().isEmpty(), is(true));
	}
}
