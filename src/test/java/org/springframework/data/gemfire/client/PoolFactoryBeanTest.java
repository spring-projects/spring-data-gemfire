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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.net.InetSocketAddress;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.client.PoolFactory;
import org.apache.geode.cache.query.QueryService;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.data.util.ReflectionUtils;

/**
 * Unit tests for {@link PoolFactoryBean}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.rules.ExpectedException
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.client.Pool
 * @see org.springframework.data.gemfire.client.PoolFactoryBean
 * @since 1.7.0
 */
public class PoolFactoryBeanTest {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	protected ConnectionEndpoint newConnectionEndpoint(String host, int port) {
		return new ConnectionEndpoint(host, port);
	}

	protected InetSocketAddress newSocketAddress(String host, int port) {
		return new InetSocketAddress(host, port);
	}

	@Test
	@SuppressWarnings("deprecation")
	public void afterPropertiesSet() throws Exception {
		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		Pool mockPool = mock(Pool.class);

		final PoolFactory mockPoolFactory = mock(PoolFactory.class);

		when(mockPoolFactory.create(eq("GemFirePool"))).thenReturn(mockPool);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean() {
			@Override protected PoolFactory createPoolFactory() {
				return mockPoolFactory;
			}
			@Override boolean isDistributedSystemPresent() {
				return false;
			}
		};

		poolFactoryBean.setBeanFactory(mockBeanFactory);
		poolFactoryBean.setBeanName("GemFirePool");
		poolFactoryBean.setName(null);
		poolFactoryBean.setFreeConnectionTimeout(60000);
		poolFactoryBean.setIdleTimeout(120000l);
		poolFactoryBean.setKeepAlive(false);
		poolFactoryBean.setLoadConditioningInterval(15000);
		poolFactoryBean.setLocators(Collections.singletonList(newConnectionEndpoint("localhost", 54321)));
		poolFactoryBean.setMaxConnections(50);
		poolFactoryBean.setMinConnections(5);
		poolFactoryBean.setMultiUserAuthentication(false);
		poolFactoryBean.setPingInterval(5000l);
		poolFactoryBean.setPrSingleHopEnabled(true);
		poolFactoryBean.setReadTimeout(30000);
		poolFactoryBean.setRetryAttempts(10);
		poolFactoryBean.setServerGroup("TestServerGroup");
		poolFactoryBean.setServers(Collections.singletonList(newConnectionEndpoint("localhost", 12345)));
		poolFactoryBean.setSocketBufferSize(32768);
		poolFactoryBean.setStatisticInterval(1000);
		poolFactoryBean.setSubscriptionAckInterval(500);
		poolFactoryBean.setSubscriptionEnabled(true);
		poolFactoryBean.setSubscriptionMessageTrackingTimeout(20000);
		poolFactoryBean.setSubscriptionRedundancy(2);
		poolFactoryBean.setThreadLocalConnections(false);
		poolFactoryBean.afterPropertiesSet();

		assertThat(poolFactoryBean.getBeanFactory(), is(equalTo(mockBeanFactory)));
		assertThat(poolFactoryBean.getObject(), is(sameInstance(mockPool)));

		verify(mockBeanFactory, times(1)).getBean(eq(ClientCache.class));
		verify(mockPoolFactory, times(1)).setFreeConnectionTimeout(eq(60000));
		verify(mockPoolFactory, times(1)).setIdleTimeout(eq(120000l));
		verify(mockPoolFactory, times(1)).setLoadConditioningInterval(eq(15000));
		verify(mockPoolFactory, times(1)).setMaxConnections(eq(50));
		verify(mockPoolFactory, times(1)).setMinConnections(eq(5));
		verify(mockPoolFactory, times(1)).setMultiuserAuthentication(eq(false));
		verify(mockPoolFactory, times(1)).setPingInterval(eq(5000l));
		verify(mockPoolFactory, times(1)).setPRSingleHopEnabled(eq(true));
		verify(mockPoolFactory, times(1)).setReadTimeout(eq(30000));
		verify(mockPoolFactory, times(1)).setRetryAttempts(eq(10));
		verify(mockPoolFactory, times(1)).setServerGroup(eq("TestServerGroup"));
		verify(mockPoolFactory, times(1)).setSocketBufferSize(eq(32768));
		verify(mockPoolFactory, times(1)).setStatisticInterval(eq(1000));
		verify(mockPoolFactory, times(1)).setSubscriptionAckInterval(eq(500));
		verify(mockPoolFactory, times(1)).setSubscriptionEnabled(eq(true));
		verify(mockPoolFactory, times(1)).setSubscriptionMessageTrackingTimeout(eq(20000));
		verify(mockPoolFactory, times(1)).setSubscriptionRedundancy(eq(2));
		verify(mockPoolFactory, times(1)).setThreadLocalConnections(eq(false));
		verify(mockPoolFactory, times(1)).addLocator(eq("localhost"), eq(54321));
		verify(mockPoolFactory, times(1)).addServer(eq("localhost"), eq(12345));
		verify(mockPoolFactory, times(1)).create(eq("GemFirePool"));
	}

	@Test
	public void afterPropertiesSetWithUnspecifiedName() throws Exception {
		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setBeanName(null);
		poolFactoryBean.setName(null);

		assertThat(poolFactoryBean.getName(), is(nullValue()));

		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("Pool name is required");

		poolFactoryBean.afterPropertiesSet();
	}

	@Test
	@SuppressWarnings("deprecation")
	public void afterPropertiesSetUsesName() throws Exception {
		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setBeanName("gemfirePool");
		poolFactoryBean.setName("TestPool");

		assertThat(poolFactoryBean.getLocators().isEmpty(), is(true));
		assertThat(poolFactoryBean.getServers().isEmpty(), is(true));

		poolFactoryBean.afterPropertiesSet();

		assertThat(poolFactoryBean.getName(), is(equalTo("TestPool")));
	}

	@Test
	@SuppressWarnings("deprecation")
	public void afterPropertiesSetDefaultsToBeanName() throws Exception {
		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setBeanName("swimPool");

		assertThat(poolFactoryBean.getName(), is(nullValue()));
		assertThat(poolFactoryBean.getLocators().isEmpty(), is(true));
		assertThat(poolFactoryBean.getServers().isEmpty(), is(true));

		poolFactoryBean.afterPropertiesSet();

		assertThat(poolFactoryBean.getName(), is(equalTo("swimPool")));
	}

	@Test
	public void destroy() throws Exception {
		Pool mockPool = mock(Pool.class);

		when(mockPool.isDestroyed()).thenReturn(false);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setPool(mockPool);
		poolFactoryBean.destroy();

		assertThat(TestUtils.readField("pool", poolFactoryBean), is(nullValue()));

		verify(mockPool, times(1)).releaseThreadLocalConnection();
		verify(mockPool, times(1)).destroy(eq(false));
	}

	@Test
	public void destroyWithNonSpringBasedPool() throws Exception {
		Pool mockPool = mock(Pool.class);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		ReflectionUtils.setField(PoolFactoryBean.class.getDeclaredField("springBasedPool"), poolFactoryBean, false);
		poolFactoryBean.setPool(mockPool);
		poolFactoryBean.destroy();

		verify(mockPool, never()).isDestroyed();
		verify(mockPool, never()).releaseThreadLocalConnection();
		verify(mockPool, never()).destroy(anyBoolean());
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

	@Test
	public void addGetAndSetLocators() {
		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		assertThat(poolFactoryBean.getLocators(), is(notNullValue()));
		assertThat(poolFactoryBean.getLocators().isEmpty(), is(true));

		ConnectionEndpoint localhost = newConnectionEndpoint("localhost", 21668);

		poolFactoryBean.addLocators(localhost);

		assertThat(poolFactoryBean.getLocators().size(), is(equalTo(1)));
		assertThat(poolFactoryBean.getLocators().findOne("localhost"), is(equalTo(localhost)));

		ConnectionEndpoint skullbox = newConnectionEndpoint("skullbox", 10334);
		ConnectionEndpoint boombox = newConnectionEndpoint("boombox", 10334);

		poolFactoryBean.addLocators(skullbox, boombox);

		assertThat(poolFactoryBean.getLocators().size(), is(equalTo(3)));
		assertThat(poolFactoryBean.getLocators().findOne("localhost"), is(equalTo(localhost)));
		assertThat(poolFactoryBean.getLocators().findOne("skullbox"), is(equalTo(skullbox)));
		assertThat(poolFactoryBean.getLocators().findOne("boombox"), is(equalTo(boombox)));

		poolFactoryBean.setLocators(ArrayUtils.asArray(localhost));

		assertThat(poolFactoryBean.getLocators().size(), is(equalTo(1)));
		assertThat(poolFactoryBean.getLocators().findOne("localhost"), is(equalTo(localhost)));

		poolFactoryBean.setLocators(Collections.<ConnectionEndpoint>emptyList());

		assertThat(poolFactoryBean.getLocators(), is(notNullValue()));
		assertThat(poolFactoryBean.getLocators().isEmpty(), is(true));
	}

	@Test
	public void addGetAndSetServers() {
		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		assertThat(poolFactoryBean.getServers(), is(notNullValue()));
		assertThat(poolFactoryBean.getServers().isEmpty(), is(true));

		ConnectionEndpoint localhost = newConnectionEndpoint("localhost", 21668);

		poolFactoryBean.addServers(localhost);

		assertThat(poolFactoryBean.getServers().size(), is(equalTo(1)));
		assertThat(poolFactoryBean.getServers().findOne("localhost"), is(equalTo(localhost)));

		ConnectionEndpoint skullbox = newConnectionEndpoint("skullbox", 10334);
		ConnectionEndpoint boombox = newConnectionEndpoint("boombox", 10334);

		poolFactoryBean.addServers(skullbox, boombox);

		assertThat(poolFactoryBean.getServers().size(), is(equalTo(3)));
		assertThat(poolFactoryBean.getServers().findOne("localhost"), is(equalTo(localhost)));
		assertThat(poolFactoryBean.getServers().findOne("skullbox"), is(equalTo(skullbox)));
		assertThat(poolFactoryBean.getServers().findOne("boombox"), is(equalTo(boombox)));

		poolFactoryBean.setServers(ArrayUtils.asArray(localhost));

		assertThat(poolFactoryBean.getServers().size(), is(equalTo(1)));
		assertThat(poolFactoryBean.getServers().findOne("localhost"), is(equalTo(localhost)));

		poolFactoryBean.setServers(Collections.<ConnectionEndpoint>emptyList());

		assertThat(poolFactoryBean.getServers(), is(notNullValue()));
		assertThat(poolFactoryBean.getServers().isEmpty(), is(true));
	}

	@Test
	public void getPoolWhenPoolIsSetIsThePool() {
		Pool mockPool = mock(Pool.class);
		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setPool(mockPool);

		assertThat(poolFactoryBean.getPool(), is(sameInstance(mockPool)));
	}

	@Test
	public void getPoolWhenPoolIsUnsetIsThePoolFactoryBean() {
		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setFreeConnectionTimeout(5000);
		poolFactoryBean.setIdleTimeout(120000l);
		poolFactoryBean.setLoadConditioningInterval(300000);
		poolFactoryBean.setLocators(ArrayUtils.asArray(newConnectionEndpoint("skullbox", 11235)));
		poolFactoryBean.setMaxConnections(500);
		poolFactoryBean.setMinConnections(50);
		poolFactoryBean.setMultiUserAuthentication(true);
		poolFactoryBean.setPingInterval(15000l);
		poolFactoryBean.setPrSingleHopEnabled(true);
		poolFactoryBean.setReadTimeout(30000);
		poolFactoryBean.setRetryAttempts(1);
		poolFactoryBean.setServerGroup("TestGroup");
		poolFactoryBean.setServers(ArrayUtils.asArray(newConnectionEndpoint("boombox", 12480)));
		poolFactoryBean.setSocketBufferSize(16384);
		poolFactoryBean.setStatisticInterval(500);
		poolFactoryBean.setSubscriptionAckInterval(200);
		poolFactoryBean.setSubscriptionEnabled(true);
		poolFactoryBean.setSubscriptionMessageTrackingTimeout(20000);
		poolFactoryBean.setSubscriptionRedundancy(2);
		poolFactoryBean.setThreadLocalConnections(false);

		Pool pool = poolFactoryBean.getPool();

		assertThat(pool, is(instanceOf(PoolAdapter.class)));
		assertThat(pool.isDestroyed(), is(false));
		assertThat(pool.getFreeConnectionTimeout(), is(equalTo(5000)));
		assertThat(pool.getIdleTimeout(), is(equalTo(120000l)));
		assertThat(pool.getLoadConditioningInterval(), is(equalTo(300000)));
		assertThat(pool.getLocators(), is(equalTo(Collections.singletonList(newSocketAddress("skullbox", 11235)))));
		assertThat(pool.getMaxConnections(), is(equalTo(500)));
		assertThat(pool.getMinConnections(), is(equalTo(50)));
		assertThat(pool.getMultiuserAuthentication(), is(equalTo(true)));
		assertThat(pool.getName(), is(nullValue()));
		assertThat(pool.getPingInterval(), is(equalTo(15000l)));
		assertThat(pool.getPRSingleHopEnabled(), is(equalTo(true)));
		assertThat(pool.getReadTimeout(), is(equalTo(30000)));
		assertThat(pool.getRetryAttempts(), is(equalTo(1)));
		assertThat(pool.getServerGroup(), is(equalTo("TestGroup")));
		assertThat(pool.getServers(), is(equalTo(Collections.singletonList(newSocketAddress("boombox", 12480)))));
		assertThat(pool.getSocketBufferSize(), is(equalTo(16384)));
		assertThat(pool.getStatisticInterval(), is(equalTo(500)));
		assertThat(pool.getSubscriptionAckInterval(), is(equalTo(200)));
		assertThat(pool.getSubscriptionEnabled(), is(equalTo(true)));
		assertThat(pool.getSubscriptionMessageTrackingTimeout(), is(equalTo(20000)));
		assertThat(pool.getSubscriptionRedundancy(), is(equalTo(2)));
		assertThat(pool.getThreadLocalConnections(), is(equalTo(false)));
	}

	@Test
	public void getPoolNameWhenBeanNameSet() {
		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setBeanName("PoolBean");
		poolFactoryBean.setName(null);

		assertThat(poolFactoryBean.getPool().getName(), is(equalTo("PoolBean")));
	}

	@Test
	public void getPoolNameWhenBeanNameAndNameSet() {
		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		poolFactoryBean.setBeanName("PoolBean");
		poolFactoryBean.setName("TestPool");

		assertThat(poolFactoryBean.getPool().getName(), is(equalTo("TestPool")));
	}

	@Test
	public void getPoolPendingEventCountWithPool() {
		Pool mockPool = mock(Pool.class);

		when(mockPool.getPendingEventCount()).thenReturn(2);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();
		Pool pool = poolFactoryBean.getPool();

		assertThat(pool, is(not(sameInstance(mockPool))));
		assertThat(pool, is(instanceOf(PoolAdapter.class)));

		poolFactoryBean.setPool(mockPool);

		assertThat(pool.getPendingEventCount(), is(equalTo(2)));

		verify(mockPool, times(1)).getPendingEventCount();
	}

	@Test
	public void getPoolPendingEventCountWithoutPoolThrowsIllegalStateException() {
		exception.expect(IllegalStateException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("The Pool has not been initialized");

		new PoolFactoryBean().getPool().getPendingEventCount();
	}

	@Test
	public void getPoolQueryServiceWithPool() {
		Pool mockPool = mock(Pool.class);
		QueryService mockQueryService = mock(QueryService.class);

		when(mockPool.getQueryService()).thenReturn(mockQueryService);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();
		Pool pool = poolFactoryBean.getPool();

		assertThat(pool, is(not(sameInstance(mockPool))));
		assertThat(pool, is(instanceOf(PoolAdapter.class)));

		poolFactoryBean.setPool(mockPool);

		assertThat(pool.getQueryService(), is(equalTo(mockQueryService)));

		verify(mockPool, times(1)).getQueryService();
	}

	@Test
	public void getPoolQueryServiceWithoutPoolThrowsIllegalStateException() {
		exception.expect(IllegalStateException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("The Pool has not been initialized");

		new PoolFactoryBean().getPool().getQueryService();
	}

	@Test
	public void getPoolAndDestroyWithPool() {
		Pool mockPool = mock(Pool.class);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean() {
			@Override public void destroy() throws Exception {
				throw new IllegalStateException("test");
			}
		};

		Pool pool = poolFactoryBean.getPool();

		assertThat(pool, is(not(sameInstance(mockPool))));
		assertThat(pool, is(instanceOf(PoolAdapter.class)));

		poolFactoryBean.setPool(mockPool);
		pool.destroy();
		pool.destroy(true);

		verify(mockPool, times(1)).destroy(eq(false));
		verify(mockPool, times(1)).destroy(eq(true));
	}

	@Test
	public void getPoolAndDestroyWithoutPool() {
		final AtomicBoolean destroyCalled = new AtomicBoolean(false);

		PoolFactoryBean poolFactoryBean = new PoolFactoryBean() {
			@Override public void destroy() throws Exception {
				destroyCalled.set(true);
				throw new IllegalStateException("test");
			}
		};

		Pool pool = poolFactoryBean.getPool();

		assertThat(pool, is(instanceOf(PoolAdapter.class)));

		pool.destroy();

		assertThat(destroyCalled.get(), is(true));

		destroyCalled.set(false);
		pool.destroy(true);

		assertThat(destroyCalled.get(), is(true));
	}

	@Test
	public void getPoolAndReleaseThreadLocalConnectionWithPool() {
		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		Pool pool = poolFactoryBean.getPool();
		Pool mockPool = mock(Pool.class);

		assertThat(pool, is(not(sameInstance(mockPool))));
		assertThat(pool, is(instanceOf(PoolAdapter.class)));

		poolFactoryBean.setPool(mockPool);
		pool.releaseThreadLocalConnection();

		verify(mockPool, times(1)).releaseThreadLocalConnection();
	}

	@Test
	public void getPoolAndReleaseThreadLocalConnectionWithoutPool() {
		PoolFactoryBean poolFactoryBean = new PoolFactoryBean();

		Pool pool = poolFactoryBean.getPool();

		assertThat(pool, is(instanceOf(PoolAdapter.class)));

		exception.expect(IllegalStateException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("The Pool has not been initialized");

		pool.releaseThreadLocalConnection();
	}
}
