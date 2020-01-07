/*
 * Copyright 2017-2020 the original author or authors.
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
 */

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;

import java.net.InetSocketAddress;
import java.util.Optional;

import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.client.PoolFactory;

import org.junit.After;
import org.junit.Test;

import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertySource;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.mock.env.MockPropertySource;

/**
 * Integration tests for {@link EnablePool} and {@link EnablePools}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.config.annotation.EnablePool
 * @see org.springframework.data.gemfire.config.annotation.EnablePools
 * @since 2.0.0
 */
public class PoolPropertiesIntegrationTests {

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
	}

	private ConfigurableApplicationContext newApplicationContext(PropertySource<?> testPropertySource,
			Class<?>... annotatedClasses) {

		AnnotationConfigApplicationContext applicationContext = new AnnotationConfigApplicationContext();

		MutablePropertySources propertySources = applicationContext.getEnvironment().getPropertySources();

		propertySources.addFirst(testPropertySource);

		applicationContext.registerShutdownHook();
		applicationContext.register(annotatedClasses);
		applicationContext.refresh();

		return applicationContext;
	}

	private void assertPool(Pool pool, int freeConnectionTimeout, long idleTimeout, int loadConditioningInterval,
			int maxConnections, int minConnections, boolean multiUserAuthentication, String name, long pingInterval,
			boolean prSinglehopEnabled, int readTimeout, int retryAttempts, String serverGroup, int socketBufferSize,
			int statisticInterval, int subscriptionAckInterval, boolean subscriptionEnabled,
			int subscriptionMessageTrackingTimeout, int subscriptionRedundancy, boolean threadLocalConnections) {

		assertThat(pool).isNotNull();
		assertThat(pool.getFreeConnectionTimeout()).isEqualTo(freeConnectionTimeout);
		assertThat(pool.getIdleTimeout()).isEqualTo(idleTimeout);
		assertThat(pool.getLoadConditioningInterval()).isEqualTo(loadConditioningInterval);
		assertThat(pool.getMaxConnections()).isEqualTo(maxConnections);
		assertThat(pool.getMinConnections()).isEqualTo(minConnections);
		assertThat(pool.getMultiuserAuthentication()).isEqualTo(multiUserAuthentication);
		assertThat(pool.getName()).isEqualTo(name);
		assertThat(pool.getPingInterval()).isEqualTo(pingInterval);
		assertThat(pool.getPRSingleHopEnabled()).isEqualTo(prSinglehopEnabled);
		assertThat(pool.getReadTimeout()).isEqualTo(readTimeout);
		assertThat(pool.getRetryAttempts()).isEqualTo(retryAttempts);
		assertThat(pool.getServerGroup()).isEqualTo(serverGroup);
		assertThat(pool.getSocketBufferSize()).isEqualTo(socketBufferSize);
		assertThat(pool.getStatisticInterval()).isEqualTo(statisticInterval);
		assertThat(pool.getSubscriptionAckInterval()).isEqualTo(subscriptionAckInterval);
		assertThat(pool.getSubscriptionEnabled()).isEqualTo(subscriptionEnabled);
		assertThat(pool.getSubscriptionMessageTrackingTimeout()).isEqualTo(subscriptionMessageTrackingTimeout);
		assertThat(pool.getSubscriptionRedundancy()).isEqualTo(subscriptionRedundancy);
		assertThat(pool.getThreadLocalConnections()).isEqualTo(threadLocalConnections);
	}

	@Test
	public void poolConfiguration() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.pool.free-connection-timeout", 5000)
			.withProperty("spring.data.gemfire.pool.locators", "skullbox[11235]")
			.withProperty("spring.data.gemfire.pool.max-connections", 500)
			.withProperty("spring.data.gemfire.pool.min-connections", 25)
			.withProperty("spring.data.gemfire.pool.ping-interval", 5000L)
			.withProperty("spring.data.gemfire.pool.pr-single-hop-enabled", false)
			.withProperty("spring.data.gemfire.pool.default.read-timeout", 5000L)
			.withProperty("spring.data.gemfire.pool.read-timeout", 15000)
			.withProperty("spring.data.gemfire.pool.retry-attempts", 2)
			.withProperty("spring.data.gemfire.pool.server-group", "testGroup")
			.withProperty("spring.data.gemfire.pool.subscription-enabled", true)
			.withProperty("spring.data.gemfire.pool.TestPool.subscription-redundancy", 2);

		this.applicationContext = newApplicationContext(testPropertySource, TestPoolConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();
		assertThat(this.applicationContext.containsBean("TestPool")).isTrue();

		Pool testPool = this.applicationContext.getBean("TestPool", Pool.class);

		assertThat(testPool).isNotNull();
		assertThat(testPool.getFreeConnectionTimeout()).isEqualTo(5000);
		assertThat(testPool.getIdleTimeout()).isEqualTo(10000L);
		assertThat(testPool.getLoadConditioningInterval()).isEqualTo(100000);
		assertThat(testPool.getLocators()).contains(new InetSocketAddress("skullbox", 11235));
		assertThat(testPool.getMaxConnections()).isEqualTo(500);
		assertThat(testPool.getMinConnections()).isEqualTo(25);
		assertThat(testPool.getMultiuserAuthentication()).isFalse();
		assertThat(testPool.getName()).isEqualTo("TestPool");
		assertThat(testPool.getPingInterval()).isEqualTo(5000L);
		assertThat(testPool.getPRSingleHopEnabled()).isFalse();
		assertThat(testPool.getReadTimeout()).isEqualTo(15000);
		assertThat(testPool.getRetryAttempts()).isEqualTo(1);
		assertThat(testPool.getServerGroup()).isEqualTo("testGroup");
		assertThat(testPool.getSubscriptionAckInterval()).isEqualTo(PoolFactory.DEFAULT_SUBSCRIPTION_ACK_INTERVAL);
		assertThat(testPool.getSubscriptionEnabled()).isEqualTo(true);
		assertThat(testPool.getSubscriptionMessageTrackingTimeout()).isEqualTo(PoolFactory.DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT);
		assertThat(testPool.getSubscriptionRedundancy()).isEqualTo(2);
		assertThat(testPool.getThreadLocalConnections()).isFalse();
	}

	@Test
	public void multiPoolConfiguration() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.pool.free-connection-timeout", 5000)
			.withProperty("spring.data.gemfire.pool.idle-timeout", 10000L)
			.withProperty("spring.data.gemfire.pool.load-conditioning-interval", 120000)
			.withProperty("spring.data.gemfire.pool.locators", "localhost[10334]")
			.withProperty("spring.data.gemfire.pool.max-connections", 500)
			.withProperty("spring.data.gemfire.pool.min-connections", 50)
			.withProperty("spring.data.gemfire.pool.multi-user-authentication", true)
			.withProperty("spring.data.gemfire.pool.ping-interval", 5000L)
			.withProperty("spring.data.gemfire.pool.pr-single-hop-enabled", false)
			.withProperty("spring.data.gemfire.pool.read-timeout", 15000L)
			.withProperty("spring.data.gemfire.pool.retry-attempts", 2)
			.withProperty("spring.data.gemfire.pool.server-group", "testGroup")
			.withProperty("spring.data.gemfire.pool.socket-buffer-size", 8192)
			.withProperty("spring.data.gemfire.pool.statistic-interval", 1000)
			.withProperty("spring.data.gemfire.pool.subscription-ack-interval", 5000)
			.withProperty("spring.data.gemfire.pool.subscription-enabled", true)
			.withProperty("spring.data.gemfire.pool.subscription-message-tracking-timeout", 180000)
			.withProperty("spring.data.gemfire.pool.subscription-redundancy", 2)
			.withProperty("spring.data.gemfire.pool.thread-local-connections", true)
			.withProperty("spring.data.gemfire.pool.default.free-connection-timeout", 15000)
			.withProperty("spring.data.gemfire.pool.default.idle-timeout", 20000L)
			.withProperty("spring.data.gemfire.pool.default.load-conditioning-interval", 180000)
			.withProperty("spring.data.gemfire.pool.default.locators", "skullbox[11235]")
			.withProperty("spring.data.gemfire.pool.default.max-connections", 275)
			.withProperty("spring.data.gemfire.pool.default.min-connections", 27)
			.withProperty("spring.data.gemfire.pool.default.multi-user-authentication", true)
			.withProperty("spring.data.gemfire.pool.default.ping-interval", 15000L)
			.withProperty("spring.data.gemfire.pool.default.pr-single-hop-enabled", false)
			.withProperty("spring.data.gemfire.pool.default.read-timeout", 2000L)
			.withProperty("spring.data.gemfire.pool.default.retry-attempts", 1)
			.withProperty("spring.data.gemfire.pool.default.server-group", "testDefaultGroup")
			.withProperty("spring.data.gemfire.pool.default.socket-buffer-size", 16384)
			.withProperty("spring.data.gemfire.pool.default.statistic-interval", 500)
			.withProperty("spring.data.gemfire.pool.default.subscription-ack-interval", 250)
			.withProperty("spring.data.gemfire.pool.default.subscription-enabled", true)
			.withProperty("spring.data.gemfire.pool.default.subscription-message-tracking-timeout", 300000)
			.withProperty("spring.data.gemfire.pool.default.subscription-redundancy", 3)
			.withProperty("spring.data.gemfire.pool.default.thread-local-connections", true)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.free-connection-timeout", 20000)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.idle-timeout", 15000L)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.load-conditioning-interval", 60000)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.servers", "jambox[12480]")
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.max-connections", 1000)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.min-connections", 100)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.multi-user-authentication", true)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.ping-interval", 20000L)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.pr-single-hop-enabled", false)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.read-timeout", 5000L)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.retry-attempts", 4)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.server-group", "testTwoGroup")
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.socket-buffer-size", 65536)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.statistic-interval", 2000)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.subscription-ack-interval", 500)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.subscription-enabled", true)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.subscription-message-tracking-timeout", 300000)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.subscription-redundancy", 4)
			.withProperty("spring.data.gemfire.pool.TestPoolTwo.thread-local-connections", true);

		this.applicationContext = newApplicationContext(testPropertySource, TestPoolsConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();
		assertThat(this.applicationContext.containsBean("TestPoolOne")).isTrue();
		assertThat(this.applicationContext.containsBean("TestPoolTwo")).isTrue();

		ClientCache gemfireCache = this.applicationContext.getBean(ClientCache.class);

		assertThat(gemfireCache).isNotNull();

		Pool defaultPool = gemfireCache.getDefaultPool();

		assertPool(defaultPool, 15000, 20000L, 180000,
			275, 27, true, "DEFAULT", 15000L,
			false, 2000, 1, "testDefaultGroup",
			16384, 500, 250, true,
			300000, 3, true);

		Pool testPoolOne = this.applicationContext.getBean("TestPoolOne", Pool.class);

		assertPool(testPoolOne, 5000, 10000L, 120000,
			500, 50, true, "TestPoolOne", 5000L,
			false, 15000, 2, "testGroup",
			8192, 1000, 5000, true,
			180000, 2, true);

		Pool testPoolTwo = this.applicationContext.getBean("TestPoolTwo", Pool.class);

		assertPool(testPoolTwo, 20000, 15000L, 60000,
			1000, 100, true, "TestPoolTwo", 20000L,
			false, 5000, 4, "testTwoGroup",
			65536, 2000, 500, true,
			300000, 4, true);
	}

	@EnableGemFireMockObjects
	@ClientCacheApplication
	@EnablePool(name = "TestPool", idleTimeout = 10000L, maxConnections = 200, minConnections = 20)
	static class TestPoolConfiguration {

		@Bean
		PoolConfigurer testPoolConfigurer() {
			return (beanName, beanFactory) -> {
				beanFactory.setLoadConditioningInterval(100000);
				beanFactory.setRetryAttempts(1);
			};
		}
	}

	@EnableGemFireMockObjects
	@ClientCacheApplication
	@EnablePools(pools = { @EnablePool(name = "TestPoolOne"), @EnablePool(name = "TestPoolTwo") })
	static class TestPoolsConfiguration {
	}
}
