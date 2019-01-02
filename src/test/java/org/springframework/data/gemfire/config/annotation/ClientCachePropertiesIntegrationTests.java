/*
 * Copyright 2017-2019 the original author or authors.
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
 */

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;

import java.util.Optional;
import java.util.Properties;

import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.client.PoolFactory;
import org.apache.geode.cache.control.ResourceManager;
import org.apache.geode.pdx.PdxSerializer;
import org.junit.After;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertySource;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.mock.env.MockPropertySource;

/**
 * Integration tests for {@link ClientCacheApplication}.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.springframework.core.env.PropertySource
 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.ClientCacheApplication
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects
 * @since 2.0.0
 */
public class ClientCachePropertiesIntegrationTests {

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

		applicationContext.register(annotatedClasses);
		applicationContext.registerShutdownHook();
		applicationContext.refresh();

		return applicationContext;
	}

	@Test
	public void clientCacheConfiguration() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.cache.critical-heap-percentage", 90.0f)
			.withProperty("spring.data.gemfire.cache.critical-off-heap-percentage", 95.0f)
			.withProperty("spring.data.gemfire.cache.eviction-heap-percentage", 85.0f)
			.withProperty("spring.data.gemfire.cache.eviction-off-heap-percentage", 80.0f)
			.withProperty("spring.data.gemfire.pdx.ignore-unread-fields", false)
			.withProperty("spring.data.gemfire.pdx.persistent", true)
			.withProperty("spring.data.gemfire.pool.free-connection-timeout", 20000L)
			.withProperty("spring.data.gemfire.pool.max-connections", 250)
			.withProperty("spring.data.gemfire.pool.ping-interval", 5000L)
			.withProperty("spring.data.gemfire.pool.pr-single-hop-enabled", false)
			.withProperty("spring.data.gemfire.pool.default.read-timeout", 5000L)
			.withProperty("spring.data.gemfire.pool.read-timeout", 20000L)
			.withProperty("spring.data.gemfire.pool.retry-attempts", 2)
			.withProperty("spring.data.gemfire.pool.server-group", "testGroup")
			.withProperty("spring.data.gemfire.pool.default.subscription-redundancy", 2);

		this.applicationContext = newApplicationContext(testPropertySource, TestClientCacheConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();
		assertThat(this.applicationContext.containsBean("mockPdxSerializer")).isTrue();

		ClientCache testClientCache = this.applicationContext.getBean("gemfireCache", ClientCache.class);

		assertThat(testClientCache).isNotNull();

		PdxSerializer mockPdxSerializer = this.applicationContext.getBean("mockPdxSerializer", PdxSerializer.class);

		assertThat(mockPdxSerializer).isNotNull();
		assertThat(testClientCache).isNotNull();
		assertThat(testClientCache.getCopyOnRead()).isTrue();
		assertThat(testClientCache.getPdxDiskStore()).isNull();
		assertThat(testClientCache.getPdxIgnoreUnreadFields()).isFalse();
		assertThat(testClientCache.getPdxPersistent()).isTrue();
		assertThat(testClientCache.getPdxReadSerialized()).isFalse();
		assertThat(testClientCache.getPdxSerializer()).isSameAs(mockPdxSerializer);

		Pool defaultPool = testClientCache.getDefaultPool();

		assertThat(defaultPool).isNotNull();
		assertThat(defaultPool.getFreeConnectionTimeout()).isEqualTo(20000);
		assertThat(defaultPool.getIdleTimeout()).isEqualTo(15000L);
		assertThat(defaultPool.getLoadConditioningInterval()).isEqualTo(180000);
		assertThat(defaultPool.getMaxConnections()).isEqualTo(250);
		assertThat(defaultPool.getMinConnections()).isEqualTo(50);
		assertThat(defaultPool.getMultiuserAuthentication()).isFalse();
		assertThat(defaultPool.getName()).isEqualTo("DEFAULT");
		assertThat(defaultPool.getPingInterval()).isEqualTo(5000L);
		assertThat(defaultPool.getPRSingleHopEnabled()).isFalse();
		assertThat(defaultPool.getReadTimeout()).isEqualTo(5000);
		assertThat(defaultPool.getRetryAttempts()).isEqualTo(2);
		assertThat(defaultPool.getServerGroup()).isEqualTo("testGroup");
		assertThat(defaultPool.getSocketBufferSize()).isEqualTo(PoolFactory.DEFAULT_SOCKET_BUFFER_SIZE);
		assertThat(defaultPool.getStatisticInterval()).isEqualTo(500);
		assertThat(defaultPool.getSubscriptionAckInterval()).isEqualTo(PoolFactory.DEFAULT_SUBSCRIPTION_ACK_INTERVAL);
		assertThat(defaultPool.getSubscriptionEnabled()).isTrue();
		assertThat(defaultPool.getSubscriptionMessageTrackingTimeout()).isEqualTo(PoolFactory.DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT);
		assertThat(defaultPool.getSubscriptionRedundancy()).isEqualTo(2);

		ResourceManager resourceManager = testClientCache.getResourceManager();

		assertThat(resourceManager).isNotNull();
		assertThat(resourceManager.getCriticalHeapPercentage()).isEqualTo(90.0f);
		assertThat(resourceManager.getCriticalOffHeapPercentage()).isEqualTo(95.0f);
		assertThat(resourceManager.getEvictionHeapPercentage()).isEqualTo(90.0f);
		assertThat(resourceManager.getEvictionOffHeapPercentage()).isEqualTo(80.0f);
	}

	@Test
	public void dynamicClientCacheConfiguration() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.cache.copy-on-read", true)
			.withProperty("spring.data.gemfire.cache.critical-heap-percentage", 90.0f)
			.withProperty("spring.data.gemfire.cache.eviction-heap-percentage", 75.0f)
			.withProperty("spring.data.gemfire.cache.log-level", "info")
			.withProperty("spring.data.gemfire.cache.name", "ABC123")
			.withProperty("spring.data.gemfire.cache.client.durable-client-id", "123")
			.withProperty("spring.data.gemfire.cache.client.durable-client-timeout", 600)
			.withProperty("spring.data.gemfire.cache.client.keep-alive", true)
			.withProperty("spring.data.gemfire.pool.default.free-connection-timeout", 5000)
			.withProperty("spring.data.gemfire.pool.default.idle-timeout", 15000)
			.withProperty("spring.data.gemfire.pool.default.load-conditioning-interval", 120000)
			.withProperty("spring.data.gemfire.pool.default.max-connections", 100)
			.withProperty("spring.data.gemfire.pool.default.min-connections", 10)
			.withProperty("spring.data.gemfire.pool.default.multi-user-authentication", true)
			.withProperty("spring.data.gemfire.pool.default.ping-interval", 15000L)
			.withProperty("spring.data.gemfire.pool.default.pr-single-hop-enabled", false)
			.withProperty("spring.data.gemfire.pool.default.read-timeout", 5000)
			.withProperty("spring.data.gemfire.pool.default.ready-for-events", true)
			.withProperty("spring.data.gemfire.pool.default.retry-attempts", 2)
			.withProperty("spring.data.gemfire.pool.default.server-group", "testGroup")
			.withProperty("spring.data.gemfire.pool.default.socket-buffer-size", 65535)
			.withProperty("spring.data.gemfire.pool.default.statistic-interval", 100)
			.withProperty("spring.data.gemfire.pool.default.subscription-ack-interval", 250)
			.withProperty("spring.data.gemfire.pool.default.subscription-enabled", true)
			.withProperty("spring.data.gemfire.pool.default.subscription-message-tracking-timeout", 300000)
			.withProperty("spring.data.gemfire.pool.default.subscription-redundancy", 2)
			.withProperty("spring.data.gemfire.pool.default.thread-local-connections", true)
			.withProperty("spring.data.gemfire.pdx.disk-store-name", "TestPdxDiskStore")
			.withProperty("spring.data.gemfire.pdx.ignore-unread-fields", false)
			.withProperty("spring.data.gemfire.pdx.persistent", true)
			.withProperty("spring.data.gemfire.pdx.read-serialized", true);

		this.applicationContext = newApplicationContext(testPropertySource, TestDynamicClientCacheConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();
		assertThat(this.applicationContext.containsBean("mockPdxSerializer")).isTrue();

		PdxSerializer mockPdxSerializer = this.applicationContext.getBean("mockPdxSerializer", PdxSerializer.class);

		ClientCacheFactoryBean clientCacheFactoryBean =
			this.applicationContext.getBean("&gemfireCache", ClientCacheFactoryBean.class);

		ClientCache clientCache = this.applicationContext.getBean("gemfireCache", ClientCache.class);

		assertThat(mockPdxSerializer).isNotNull();
		assertThat(clientCacheFactoryBean).isNotNull();
		assertThat(clientCacheFactoryBean.getDurableClientId()).isEqualTo("123");
		assertThat(clientCacheFactoryBean.getDurableClientTimeout()).isEqualTo(600);
		assertThat(clientCacheFactoryBean.isKeepAlive()).isTrue();
		assertThat(clientCacheFactoryBean.isReadyForEvents()).isTrue();
		assertThat(clientCache).isNotNull();
		assertThat(clientCache.getCopyOnRead()).isTrue();
		assertThat(clientCache.getDistributedSystem()).isNotNull();
		assertThat(clientCache.getPdxDiskStore()).isEqualTo("TestPdxDiskStore");
		assertThat(clientCache.getPdxIgnoreUnreadFields()).isFalse();
		assertThat(clientCache.getPdxPersistent()).isTrue();
		assertThat(clientCache.getPdxReadSerialized()).isTrue();
		assertThat(clientCache.getPdxSerializer()).isSameAs(mockPdxSerializer);

		Properties gemfireProperties = clientCache.getDistributedSystem().getProperties();

		assertThat(gemfireProperties).isNotNull();
		assertThat(gemfireProperties.getProperty("log-level")).isEqualTo("info");
		assertThat(gemfireProperties.getProperty("name")).isEqualTo("ABC123");

		Pool defaultPool = clientCache.getDefaultPool();

		assertThat(defaultPool).isNotNull();
		assertThat(defaultPool.getFreeConnectionTimeout()).isEqualTo(5000);
		assertThat(defaultPool.getIdleTimeout()).isEqualTo(15000L);
		assertThat(defaultPool.getLoadConditioningInterval()).isEqualTo(120000);
		assertThat(defaultPool.getMaxConnections()).isEqualTo(100);
		assertThat(defaultPool.getMinConnections()).isEqualTo(10);
		assertThat(defaultPool.getMultiuserAuthentication()).isTrue();
		assertThat(defaultPool.getName()).isEqualTo("DEFAULT");
		assertThat(defaultPool.getPingInterval()).isEqualTo(15000L);
		assertThat(defaultPool.getPRSingleHopEnabled()).isFalse();
		assertThat(defaultPool.getReadTimeout()).isEqualTo(5000);
		assertThat(defaultPool.getRetryAttempts()).isEqualTo(2);
		assertThat(defaultPool.getServerGroup()).isEqualTo("testGroup");
		assertThat(defaultPool.getSocketBufferSize()).isEqualTo(65535);
		assertThat(defaultPool.getStatisticInterval()).isEqualTo(100);
		assertThat(defaultPool.getSubscriptionAckInterval()).isEqualTo(250);
		assertThat(defaultPool.getSubscriptionEnabled()).isTrue();
		assertThat(defaultPool.getSubscriptionMessageTrackingTimeout()).isEqualTo(300000);
		assertThat(defaultPool.getSubscriptionRedundancy()).isEqualTo(2);
		assertThat(defaultPool.getThreadLocalConnections()).isTrue();

		ResourceManager resourceManager = clientCache.getResourceManager();

		assertThat(resourceManager).isNotNull();
		assertThat(resourceManager.getCriticalHeapPercentage()).isEqualTo(90.0f);
		assertThat(resourceManager.getEvictionHeapPercentage()).isEqualTo(75.0f);
	}

	// TODO add more tests!

	@EnableGemFireMockObjects
	@ClientCacheApplication(name = "TestClientCache", copyOnRead = true,
		criticalHeapPercentage = 95.0f, evictionHeapPercentage = 80.0f, idleTimeout = 15000L,
		maxConnections = 100, minConnections = 10, pingInterval = 15000L, readTimeout = 15000, retryAttempts = 1,
		subscriptionEnabled = true, subscriptionRedundancy = 1)
	@EnablePdx(ignoreUnreadFields = true, readSerialized = true, serializerBeanName = "mockPdxSerializer")
	@SuppressWarnings("unused")
	static class TestClientCacheConfiguration {

		@Bean
		ClientCacheConfigurer testClientCacheConfigurer() {

			return (beanName, factoryBean) -> {
				factoryBean.setEvictionHeapPercentage(90.0f);
				factoryBean.setPdxReadSerialized(false);
				factoryBean.setLoadConditioningInterval(180000);
				factoryBean.setMinConnections(50);
				factoryBean.setStatisticsInterval(500);
			};
		}

		@Bean
		PdxSerializer mockPdxSerializer() {
			return mock(PdxSerializer.class);
		}
	}

	@EnableGemFireMockObjects
	@ClientCacheApplication(name = "TestClientCache")
	@EnablePdx(serializerBeanName = "mockPdxSerializer")
	@SuppressWarnings("unused")
	static class TestDynamicClientCacheConfiguration {

		@Bean
		PdxSerializer mockPdxSerializer() {
			return mock(PdxSerializer.class);
		}
	}
}
