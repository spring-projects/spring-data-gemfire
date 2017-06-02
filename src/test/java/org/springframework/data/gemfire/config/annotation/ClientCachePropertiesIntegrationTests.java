/*
 * Copyright 2017 the original author or authors.
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
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMocking;
import org.springframework.mock.env.MockPropertySource;

/**
 * The ClientCachePropertiesIntegrationTests class...
 *
 * @author John Blum
 * @since 1.0.0
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

		applicationContext.registerShutdownHook();
		applicationContext.register(annotatedClasses);
		applicationContext.refresh();

		return applicationContext;
	}

	@Test
	public void clientCacheConfiguration() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.cache.critical-heap-percentage", 90.0f)
			.withProperty("spring.data.gemfire.cache.eviction-heap-percentage", 85.0f)
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
		assertThat(resourceManager.getEvictionHeapPercentage()).isEqualTo(90.0f);
	}

	// TODO add more tests!

	@EnableGemFireMocking
	@EnablePdx(ignoreUnreadFields = true, readSerialized = true, serializerBeanName = "mockPdxSerializer")
	@ClientCacheApplication(name = "TestClientCache", copyOnRead = true,
		criticalHeapPercentage = 95.0f, evictionHeapPercentage = 80.0f, idleTimeout = 15000L,
		maxConnections = 100, minConnections = 10, pingInterval = 15000L, readTimeout = 15000, retryAttempts = 1,
		subscriptionEnabled = true, subscriptionRedundancy = 1)
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
}
