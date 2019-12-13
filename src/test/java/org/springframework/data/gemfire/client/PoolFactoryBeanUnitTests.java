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
package org.springframework.data.gemfire.client;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.net.InetSocketAddress;
import java.util.Collections;

import org.junit.Test;

import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.client.PoolFactory;
import org.apache.geode.cache.query.QueryService;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.data.util.ReflectionUtils;

/**
 * Unit Tests for {@link PoolFactoryBean}.
 *
 * @author John Blum
 * @see java.net.InetSocketAddress
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.Pool
 * @see org.apache.geode.cache.client.PoolFactory
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.data.gemfire.client.PoolFactoryBean
 * @see org.springframework.data.gemfire.client.PoolResolver
 * @see org.springframework.data.gemfire.support.ConnectionEndpoint
 * @since 1.7.0
 */
public class PoolFactoryBeanUnitTests {

	private ConnectionEndpoint newConnectionEndpoint(String host, int port) {
		return new ConnectionEndpoint(host, port);
	}

	private InetSocketAddress newSocketAddress(String host, int port) {
		return new InetSocketAddress(host, port);
	}

	@Test
	@SuppressWarnings("deprecation")
	public void afterPropertiesSetCreatesPool() throws Exception {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		Pool mockPool = mock(Pool.class);

		PoolFactory mockPoolFactory = mock(PoolFactory.class);

		PoolResolver mockPoolResolver = mock(PoolResolver.class);

		when(mockPoolFactory.create(eq("GemFirePool"))).thenReturn(mockPool);
		when(mockPoolResolver.resolve(anyString())).thenReturn(null);

		PoolFactoryBean poolFactoryBean = spy(new PoolFactoryBean());

		doReturn(mockPoolFactory).when(poolFactoryBean).createPoolFactory();
		doReturn(false).when(poolFactoryBean).isClientCachePresent();

		poolFactoryBean.setBeanFactory(mockBeanFactory);
		poolFactoryBean.setBeanName("GemFirePool");
		poolFactoryBean.setName(null);
		poolFactoryBean.setFreeConnectionTimeout(60000);
		poolFactoryBean.setIdleTimeout(120000L);
		poolFactoryBean.setKeepAlive(false);
		poolFactoryBean.setLoadConditioningInterval(15000);
		poolFactoryBean.setLocators(Collections.singletonList(newConnectionEndpoint("localhost", 54321)));
		poolFactoryBean.setMaxConnections(50);
		poolFactoryBean.setMinConnections(5);
		poolFactoryBean.setMultiUserAuthentication(false);
		poolFactoryBean.setPingInterval(5000L);
		poolFactoryBean.setPoolResolver(mockPoolResolver);
		poolFactoryBean.setPrSingleHopEnabled(true);
		poolFactoryBean.setReadTimeout(30000);
		poolFactoryBean.setRetryAttempts(10);
		poolFactoryBean.setServerGroup("TestServerGroup");
		poolFactoryBean.setServers(Collections.singletonList(newConnectionEndpoint("localhost", 12345)));
		poolFactoryBean.setSocketBufferSize(32768);
		poolFactoryBean.setSocketConnectTimeout(5000);
		poolFactoryBean.setStatisticInterval(1000);
		poolFactoryBean.setSubscriptionAckInterval(500);
		poolFactoryBean.setSubscriptionEnabled(true);
		poolFactoryBean.setSubscriptionMessageTrackingTimeout(20000);
		poolFactoryBean.setSubscriptionRedundancy(2);
		poolFactoryBean.setThreadLocalConnections(false);
		poolFactoryBean.afterPropertiesSet();

		assertThat(poolFactoryBean.getBeanFactory()).isEqualTo(mockBeanFactory);
		assertThat(poolFactoryBean.getObject()).isSameAs(mockPool);
		assertThat(poolFactoryBean.getPoolResolver()).isSameAs(mockPoolResolver);

		verify(mockBeanFactory, times(1)).getBean(eq(ClientCache.class));
		verify(mockPoolFactory, times(1)).setFreeConnectionTimeout(eq(60000));
		verify(mockPoolFactory, times(1)).setIdleTimeout(eq(120000L));
		verify(mockPoolFactory, times(1)).setLoadConditioningInterval(eq(15000));
		verify(mockPoolFactory, times(1)).setMaxConnections(eq(50));
		verify(mockPoolFactory, times(1)).setMinConnections(eq(5));
		verify(mockPoolFactory, times(1)).setMultiuserAuthentication(eq(false));
		verify(mockPoolFactory, times(1)).setPingInterval(eq(5000L));
		verify(mockPoolFactory, times(1)).setPRSingleHopEnabled(eq(true));
		verify(mockPoolFactory, times(1)).setReadTimeout(eq(30000));
		verify(mockPoolFactory, times(1)).setRetryAttempts(eq(10));
		verify(mockPoolFactory, times(1)).setServerGroup(eq("TestServerGroup"));
		verify(mockPoolFactory, times(1)).setSocketBufferSize(eq(32768));
		verify(mockPoolFactory, times(1)).setSocketConnectTimeout(eq(5000));
		verify(mockPoolFactory, times(1)).setStatisticInterval(eq(1000));
		verify(mockPoolFactory, times(1)).setSubscriptionAckInterval(eq(500));
		verify(mockPoolFactory, times(1)).setSubscriptionEnabled(eq(true));
		verify(mockPoolFactory, times(1)).setSubscriptionMessageTrackingTimeout(eq(20000));
		verify(mockPoolFactory, times(1)).setSubscriptionRedundancy(eq(2));
		verify(mockPoolFactory, times(1)).setThreadLocalConnections(eq(false));
		verify(mockPoolFactory, times(1)).addLocator(eq("localhost"), eq(54321));
		verify(mockPoolFactory, times(1)).addServer(eq("localhost"), eq(12345));
		verify(mockPoolFactory, times(1)).create(eq("GemFirePool"));
		verify(mockPoolResolver, times(2)).resolve(eq("GemFirePool"));
	}

	@SuppressWarnings("all")
	@Test(expected = IllegalArgumentException.class)
	public void afterPropertiesSetWithUnspecifiedName() throws Exception {

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setBeanName(null);
		poolFactoryBean.setName(null);

		assertThat(poolFactoryBean.getBeanName()).isNull();
		assertThat(poolFactoryBean.getName()).isNull();

		try {
			poolFactoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("Pool name is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void afterPropertiesSetUsesName() throws Exception {

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setBeanName("gemfirePool");
		poolFactoryBean.setName("TestPool");

		assertThat(poolFactoryBean.getBeanName()).isEqualTo("gemfirePool");
		assertThat(poolFactoryBean.getName()).isEqualTo("TestPool");
		assertThat(poolFactoryBean.getLocators()).isEmpty();
		assertThat(poolFactoryBean.getServers()).isEmpty();

		poolFactoryBean.afterPropertiesSet();

		assertThat(poolFactoryBean.getName()).isEqualTo("TestPool");
	}

	@Test
	public void afterPropertiesSetDefaultsToBeanName() throws Exception {

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setBeanName("swimPool");

		assertThat(poolFactoryBean.getBeanName()).isEqualTo("swimPool");
		assertThat(poolFactoryBean.getName()).isNull();
		assertThat(poolFactoryBean.getLocators()).isEmpty();
		assertThat(poolFactoryBean.getServers()).isEmpty();

		poolFactoryBean.afterPropertiesSet();

		assertThat(poolFactoryBean.getName()).isEqualTo("swimPool");
	}

	@Test
	public void destroyDestroysPool() throws Exception {

		Pool mockPool = mock(Pool.class);

		when(mockPool.isDestroyed()).thenReturn(false);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setPool(mockPool);
		poolFactoryBean.destroy();

		assertThat(TestUtils.<Pool>readField("pool", poolFactoryBean)).isNull();

		verify(mockPool, times(1)).isDestroyed();
		verify(mockPool, times(1)).releaseThreadLocalConnection();
		verify(mockPool, times(1)).destroy(eq(false));
	}

	@Test
	public void destroyNonSpringManagedPool() throws Exception {

		Pool mockPool = mock(Pool.class);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		ReflectionUtils.setField(PoolFactoryBean.class.getDeclaredField("springManagedPool"), poolFactoryBean, false);
		poolFactoryBean.setPool(mockPool);
		poolFactoryBean.destroy();

		verify(mockPool, never()).isDestroyed();
		verify(mockPool, never()).releaseThreadLocalConnection();
		verify(mockPool, never()).destroy(anyBoolean());
	}

	@Test
	public void destroyUninitializedPool() throws Exception {

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setPool(null);
		poolFactoryBean.destroy();
	}

	@Test
	public void getObjectType() {
		assertThat(new PoolFactoryBean().getObjectType()).isEqualTo(Pool.class);
	}

	@Test
	public void isSingleton() {
		assertThat(new PoolFactoryBean().isSingleton()).isTrue();
	}

	@Test
	public void addGetAndSetLocators() {

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		assertThat(poolFactoryBean.getLocators()).isNotNull();
		assertThat(poolFactoryBean.getLocators().isEmpty()).isTrue();

		ConnectionEndpoint localhost = newConnectionEndpoint("localhost", 21668);

		poolFactoryBean.addLocators(localhost);

		assertThat(poolFactoryBean.getLocators().size()).isEqualTo(1);
		assertThat(poolFactoryBean.getLocators().findOne("localhost")).isEqualTo(localhost);

		ConnectionEndpoint skullbox = newConnectionEndpoint("skullbox", 10334);
		ConnectionEndpoint boombox = newConnectionEndpoint("boombox", 10334);

		poolFactoryBean.addLocators(skullbox, boombox);

		assertThat(poolFactoryBean.getLocators().size()).isEqualTo(3);
		assertThat(poolFactoryBean.getLocators().findOne("localhost")).isEqualTo(localhost);
		assertThat(poolFactoryBean.getLocators().findOne("skullbox")).isEqualTo(skullbox);
		assertThat(poolFactoryBean.getLocators().findOne("boombox")).isEqualTo(boombox);

		poolFactoryBean.setLocators(ArrayUtils.asArray(localhost));

		assertThat(poolFactoryBean.getLocators().size()).isEqualTo(1);
		assertThat(poolFactoryBean.getLocators().findOne("localhost")).isEqualTo(localhost);

		poolFactoryBean.setLocators(Collections.emptyList());

		assertThat(poolFactoryBean.getLocators()).isNotNull();
		assertThat(poolFactoryBean.getLocators().isEmpty()).isTrue();
	}

	@Test
	public void addGetAndSetServers() {

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		assertThat(poolFactoryBean.getServers()).isNotNull();
		assertThat(poolFactoryBean.getServers().isEmpty()).isTrue();

		ConnectionEndpoint localhost = newConnectionEndpoint("localhost", 21668);

		poolFactoryBean.addServers(localhost);

		assertThat(poolFactoryBean.getServers().size()).isEqualTo(1);
		assertThat(poolFactoryBean.getServers().findOne("localhost")).isEqualTo(localhost);

		ConnectionEndpoint skullbox = newConnectionEndpoint("skullbox", 10334);
		ConnectionEndpoint boombox = newConnectionEndpoint("boombox", 10334);

		poolFactoryBean.addServers(skullbox, boombox);

		assertThat(poolFactoryBean.getServers().size()).isEqualTo(3);
		assertThat(poolFactoryBean.getServers().findOne("localhost")).isEqualTo(localhost);
		assertThat(poolFactoryBean.getServers().findOne("skullbox")).isEqualTo(skullbox);
		assertThat(poolFactoryBean.getServers().findOne("boombox")).isEqualTo(boombox);

		poolFactoryBean.setServers(ArrayUtils.asArray(localhost));

		assertThat(poolFactoryBean.getServers().size()).isEqualTo(1);
		assertThat(poolFactoryBean.getServers().findOne("localhost")).isEqualTo(localhost);

		poolFactoryBean.setServers(Collections.emptyList());

		assertThat(poolFactoryBean.getServers()).isNotNull();
		assertThat(poolFactoryBean.getServers().isEmpty()).isTrue();
	}

	@Test
	public void getPoolWhenPoolIsSet() {

		Pool mockPool = mock(Pool.class);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setPool(mockPool);

		assertThat(poolFactoryBean.getPool()).isSameAs(mockPool);
	}

	@Test
	public void getPoolWhenPoolIsUnset() {

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setFreeConnectionTimeout(5000);
		poolFactoryBean.setIdleTimeout(120000L);
		poolFactoryBean.setLoadConditioningInterval(300000);
		poolFactoryBean.setLocators(ArrayUtils.asArray(newConnectionEndpoint("skullbox", 11235)));
		poolFactoryBean.setMaxConnections(500);
		poolFactoryBean.setMinConnections(50);
		poolFactoryBean.setMultiUserAuthentication(true);
		poolFactoryBean.setPingInterval(15000L);
		poolFactoryBean.setPrSingleHopEnabled(true);
		poolFactoryBean.setReadTimeout(30000);
		poolFactoryBean.setRetryAttempts(1);
		poolFactoryBean.setServerGroup("TestGroup");
		poolFactoryBean.setServers(ArrayUtils.asArray(newConnectionEndpoint("boombox", 12480)));
		poolFactoryBean.setSocketBufferSize(16384);
		poolFactoryBean.setSocketConnectTimeout(5000);
		poolFactoryBean.setStatisticInterval(500);
		poolFactoryBean.setSubscriptionAckInterval(200);
		poolFactoryBean.setSubscriptionEnabled(true);
		poolFactoryBean.setSubscriptionMessageTrackingTimeout(20000);
		poolFactoryBean.setSubscriptionRedundancy(2);
		poolFactoryBean.setSubscriptionTimeoutMultiplier(4);
		poolFactoryBean.setThreadLocalConnections(false);

		Pool pool = poolFactoryBean.getPool();

		assertThat(pool).isInstanceOf(PoolAdapter.class);
		assertThat(pool.isDestroyed()).isFalse();
		assertThat(pool.getFreeConnectionTimeout()).isEqualTo(5000);
		assertThat(pool.getIdleTimeout()).isEqualTo(120000L);
		assertThat(pool.getLoadConditioningInterval()).isEqualTo(300000);
		assertThat(pool.getLocators()).isEqualTo(Collections.singletonList(newSocketAddress("skullbox", 11235)));
		assertThat(pool.getMaxConnections()).isEqualTo(500);
		assertThat(pool.getMinConnections()).isEqualTo(50);
		assertThat(pool.getMultiuserAuthentication()).isTrue();
		assertThat(pool.getName()).isNull();
		assertThat(pool.getPingInterval()).isEqualTo(15000L);
		assertThat(pool.getPRSingleHopEnabled()).isTrue();
		assertThat(pool.getReadTimeout()).isEqualTo(30000);
		assertThat(pool.getRetryAttempts()).isEqualTo(1);
		assertThat(pool.getServerGroup()).isEqualTo("TestGroup");
		assertThat(pool.getServers()).isEqualTo(Collections.singletonList(newSocketAddress("boombox", 12480)));
		assertThat(pool.getSocketBufferSize()).isEqualTo(16384);
		assertThat(pool.getSocketConnectTimeout()).isEqualTo(5000);
		assertThat(pool.getStatisticInterval()).isEqualTo(500);
		assertThat(pool.getSubscriptionAckInterval()).isEqualTo(200);
		assertThat(pool.getSubscriptionEnabled()).isTrue();
		assertThat(pool.getSubscriptionMessageTrackingTimeout()).isEqualTo(20000);
		assertThat(pool.getSubscriptionRedundancy()).isEqualTo(2);
		assertThat(pool.getSubscriptionTimeoutMultiplier()).isEqualTo(4);
		assertThat(pool.getThreadLocalConnections()).isFalse();
	}

	@Test
	public void getPoolNameWhenBeanNameSet() {

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setBeanName("PoolBean");
		poolFactoryBean.setName(null);

		assertThat(poolFactoryBean.getPool().getName()).isEqualTo("PoolBean");
	}

	@Test
	public void getPoolNameWhenBeanNameAndNameSet() {

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setBeanName("PoolBean");
		poolFactoryBean.setName("TestPool");

		assertThat(poolFactoryBean.getPool().getName()).isEqualTo("TestPool");
	}

	@Test
	public void getPoolPendingEventCountWithPool() {

		Pool mockPool = mock(Pool.class);

		when(mockPool.getPendingEventCount()).thenReturn(2);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		Pool pool = poolFactoryBean.getPool();

		assertThat(pool).isNotSameAs(mockPool);
		assertThat(pool).isInstanceOf(PoolAdapter.class);

		poolFactoryBean.setPool(mockPool);

		assertThat(pool.getPendingEventCount()).isEqualTo(2);

		verify(mockPool, times(1)).getPendingEventCount();
	}

	@Test(expected = IllegalStateException.class)
	public void getPoolPendingEventCountWithoutPoolThrowsIllegalStateException() {

		try {
			new PoolFactoryBean().getPool().getPendingEventCount();
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessage("Pool [null] has not been initialized");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void getPoolQueryServiceWithPool() {

		Pool mockPool = mock(Pool.class);

		QueryService mockQueryService = mock(QueryService.class);

		when(mockPool.getQueryService()).thenReturn(mockQueryService);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();
		Pool pool = poolFactoryBean.getPool();

		assertThat(pool).isNotSameAs(mockPool);
		assertThat(pool).isInstanceOf(PoolAdapter.class);

		poolFactoryBean.setPool(mockPool);

		assertThat(pool.getQueryService()).isEqualTo(mockQueryService);

		verify(mockPool, times(1)).getQueryService();
	}

	@Test(expected = IllegalStateException.class)
	public void getPoolQueryServiceWithoutPoolThrowsIllegalStateException() {

		try {
			new PoolFactoryBean().getPool().getQueryService();
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessage("Pool [null] has not been initialized");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void getPoolAndDestroyWithPool() throws Exception {

		Pool mockPool = mock(Pool.class);

		PoolFactoryBean poolFactoryBean = spy(new PoolFactoryBean());

		doThrow(newIllegalStateException("test")).when(poolFactoryBean).destroy();

		Pool pool = poolFactoryBean.getPool();

		assertThat(pool).isNotSameAs(mockPool);
		assertThat(pool).isInstanceOf(PoolAdapter.class);

		poolFactoryBean.setPool(mockPool);
		pool.destroy();
		pool.destroy(true);

		verify(mockPool, times(1)).destroy(eq(false));
		verify(mockPool, times(1)).destroy(eq(true));
	}

	@Test
	public void getPoolAndDestroyWithoutPool() throws Exception {

		PoolFactoryBean poolFactoryBean = spy(new PoolFactoryBean());;

		doThrow(newIllegalStateException("test")).when(poolFactoryBean).destroy();

		Pool pool = poolFactoryBean.getPool();

		assertThat(pool).isInstanceOf(PoolAdapter.class);

		pool.destroy();
		pool.destroy(true);

		verify(poolFactoryBean, times(2)).destroy();
	}

	@Test
	public void getPoolAndReleaseThreadLocalConnectionWithPool() {

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		Pool pool = poolFactoryBean.getPool();
		Pool mockPool = mock(Pool.class);

		assertThat(pool).isNotSameAs(mockPool);
		assertThat(pool).isInstanceOf(PoolAdapter.class);

		poolFactoryBean.setPool(mockPool);
		pool.releaseThreadLocalConnection();

		verify(mockPool, times(1)).releaseThreadLocalConnection();
	}

	@Test(expected = IllegalStateException.class)
	public void getPoolAndReleaseThreadLocalConnectionWithoutPool() {

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		Pool pool = poolFactoryBean.getPool();

		assertThat(pool).isInstanceOf(PoolAdapter.class);

		try {
			pool.releaseThreadLocalConnection();
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessage("Pool [null] has not been initialized");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void setAndGetPoolResolver() {

		PoolResolver mockPoolResolver = mock(PoolResolver.class);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		assertThat(poolFactoryBean.getPoolResolver()).isEqualTo(PoolFactoryBean.DEFAULT_POOL_RESOLVER);

		poolFactoryBean.setPoolResolver(mockPoolResolver);

		assertThat(poolFactoryBean.getPoolResolver()).isSameAs(mockPoolResolver);

		poolFactoryBean.setPoolResolver(null);

		assertThat(poolFactoryBean.getPoolResolver()).isEqualTo(PoolFactoryBean.DEFAULT_POOL_RESOLVER);
	}
}
