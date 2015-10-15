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

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertEquals;
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

import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.util.Collections;
import java.util.Properties;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.util.ReflectionUtils;

import com.gemstone.gemfire.cache.client.Pool;
import com.gemstone.gemfire.cache.client.PoolFactory;

/**
 * The PoolFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the PoolFactoryBean class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.client.PoolFactoryBean
 * @see com.gemstone.gemfire.cache.client.Pool
 * @see com.gemstone.gemfire.cache.client.PoolFactory
 * @since 1.7.0
 */
public class PoolFactoryBeanTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	@Test
	@SuppressWarnings("deprecation")
	public void afterPropertiesSet() throws Exception {
		BeanFactory mockBeanFactory = mock(BeanFactory.class, "MockBeanFactory");

		final PoolFactory mockPoolFactory = mock(PoolFactory.class, "MockPoolFactory");

		Pool mockPool = mock(Pool.class, "MockPool");

		final Properties gemfireProperties = new Properties();

		gemfireProperties.setProperty("name", "testAfterPropertiesSet");

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		clientCacheFactoryBean.setProperties(gemfireProperties);

		when(mockBeanFactory.getBean(eq(ClientCacheFactoryBean.class))).thenReturn(clientCacheFactoryBean);
		when(mockPoolFactory.create(eq("GemFirePool"))).thenReturn(mockPool);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean() {
			@Override protected PoolFactory createPoolFactory() {
				return mockPoolFactory;
			}

			@Override void doDistributedSystemConnect(Properties properties) {
				assertSame(gemfireProperties, properties);
			}
		};

		poolFactoryBean.setBeanFactory(mockBeanFactory);
		poolFactoryBean.setBeanName("GemFirePool");
		poolFactoryBean.setName(null);
		poolFactoryBean.setFreeConnectionTimeout(60000);
		poolFactoryBean.setIdleTimeout(120000l);
		poolFactoryBean.setKeepAlive(false);
		poolFactoryBean.setLoadConditioningInterval(15000);
		poolFactoryBean.setLocators(Collections.singletonList(new InetSocketAddress(InetAddress.getLocalHost(), 54321)));
		poolFactoryBean.setMaxConnections(50);
		poolFactoryBean.setMinConnections(5);
		poolFactoryBean.setMultiUserAuthentication(false);
		poolFactoryBean.setPingInterval(5000l);
		poolFactoryBean.setPrSingleHopEnabled(true);
		poolFactoryBean.setRetryAttempts(10);
		poolFactoryBean.setServerGroup("TestServerGroup");
		poolFactoryBean.setServers(Collections.singletonList(new InetSocketAddress(InetAddress.getLocalHost(), 12345)));
		poolFactoryBean.setSocketBufferSize(32768);
		poolFactoryBean.setStatisticInterval(1000);
		poolFactoryBean.setSubscriptionAckInterval(500);
		poolFactoryBean.setSubscriptionMessageTrackingTimeout(20000);
		poolFactoryBean.setSubscriptionRedundancy(2);
		poolFactoryBean.setThreadLocalConnections(false);
		poolFactoryBean.afterPropertiesSet();

		assertSame(mockPool, poolFactoryBean.getObject());

		verify(mockPoolFactory, times(1)).setFreeConnectionTimeout(eq(60000));
		verify(mockPoolFactory, times(1)).setIdleTimeout(eq(120000l));
		verify(mockPoolFactory, times(1)).setLoadConditioningInterval(eq(15000));
		verify(mockPoolFactory, times(1)).setMaxConnections(eq(50));
		verify(mockPoolFactory, times(1)).setMinConnections(eq(5));
		verify(mockPoolFactory, times(1)).setMultiuserAuthentication(eq(false));
		verify(mockPoolFactory, times(1)).setPingInterval(eq(5000l));
		verify(mockPoolFactory, times(1)).setPRSingleHopEnabled(eq(true));
		verify(mockPoolFactory, times(1)).setRetryAttempts(eq(10));
		verify(mockPoolFactory, times(1)).setServerGroup(eq("TestServerGroup"));
		verify(mockPoolFactory, times(1)).setSocketBufferSize(eq(32768));
		verify(mockPoolFactory, times(1)).setStatisticInterval(eq(1000));
		verify(mockPoolFactory, times(1)).setSubscriptionAckInterval(eq(500));
		verify(mockPoolFactory, times(1)).setSubscriptionMessageTrackingTimeout(eq(20000));
		verify(mockPoolFactory, times(1)).setSubscriptionRedundancy(eq(2));
		verify(mockPoolFactory, times(1)).setThreadLocalConnections(eq(false));
		verify(mockPoolFactory, times(1)).addLocator(InetAddress.getLocalHost().getHostName(), 54321);
		verify(mockPoolFactory, times(1)).addServer(InetAddress.getLocalHost().getHostName(), 12345);
		verify(mockPoolFactory, times(1)).create(eq("GemFirePool"));
	}

	@Test
	public void afterPropertiesSetWithUnspecifiedName() throws Exception {
		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setBeanName(null);
		poolFactoryBean.setName(null);

		expectedException.expect(IllegalArgumentException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("Pool 'name' is required");

		poolFactoryBean.afterPropertiesSet();
	}

	@Test
	@SuppressWarnings("deprecation")
	public void afterPropertiesSetWithNoLocatorsOrServersSpecified() throws Exception {
		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setBeanName("GemFirePool");
		poolFactoryBean.setLocators(null);
		poolFactoryBean.setServers(Collections.<InetSocketAddress>emptyList());

		expectedException.expect(IllegalArgumentException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("at least one GemFire Locator or Server is required");

		poolFactoryBean.afterPropertiesSet();
	}

	@Test
	public void resolveGemfireProperties() {
		BeanFactory mockBeanFactory = mock(BeanFactory.class, "MockBeanFactory");

		ClientCacheFactoryBean clientCacheFactoryBean = new ClientCacheFactoryBean();

		when(mockBeanFactory.getBean(eq(ClientCacheFactoryBean.class))).thenReturn(clientCacheFactoryBean);

		Properties expectedGemfireProperties = new Properties();

		expectedGemfireProperties.setProperty("name", "testResolveGemfireProperties");
		clientCacheFactoryBean.setProperties(expectedGemfireProperties);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setBeanFactory(mockBeanFactory);

		Properties resolvedGemfireProperties = poolFactoryBean.resolveGemfireProperties();

		assertSame(expectedGemfireProperties, resolvedGemfireProperties);
	}

	@Test
	public void resolveUnresolvableGemfireProperties() {
		BeanFactory mockBeanFactory = mock(BeanFactory.class, "MockBeanFactory");

		when(mockBeanFactory.getBean(eq(ClientCacheFactoryBean.class))).thenThrow(
			new NoSuchBeanDefinitionException("TEST"));

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setBeanFactory(mockBeanFactory);

		assertNull(poolFactoryBean.resolveGemfireProperties());
	}

	@Test
	public void destroy() throws Exception {
		Pool mockPool = mock(Pool.class, "MockPool");

		when(mockPool.isDestroyed()).thenReturn(false);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setPool(mockPool);
		poolFactoryBean.destroy();

		assertNull(TestUtils.readField("pool", poolFactoryBean));

		verify(mockPool, times(1)).releaseThreadLocalConnection();
		verify(mockPool, times(1)).destroy(eq(false));
	}

	@Test
	public void destroyWithNonSpringBasedPool() throws Exception {
		Pool mockPool = mock(Pool.class, "MockGemFirePool");

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		ReflectionUtils.setField(PoolFactoryBean.class.getDeclaredField("springBasedPool"), poolFactoryBean, false);
		poolFactoryBean.setPool(mockPool);
		poolFactoryBean.destroy();

		verify(mockPool, never()).isDestroyed();
		verify(mockPool, never()).releaseThreadLocalConnection();
		verify(mockPool, never()).destroy(any(Boolean.class));
	}

	@Test
	public void destroyWithUninitializedPool() throws Exception {
		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setPool(null);
		poolFactoryBean.destroy();
	}

	@Test
	public void getObjectType() {
		assertEquals(Pool.class, new PoolFactoryBean().getObjectType());
	}

	@Test
	public void isSingleton() {
		assertTrue(new PoolFactoryBean().isSingleton());
	}

}
