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

import java.util.Optional;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.server.CacheServer;
import org.apache.geode.cache.server.ClientSubscriptionConfig;
import org.junit.After;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertySource;
import org.springframework.data.gemfire.server.SubscriptionEvictionPolicy;
import org.springframework.data.gemfire.test.mock.MockGemFireObjectsSupport;
import org.springframework.mock.env.MockPropertySource;

/**
 * The CacheServerPropertiesIntegrationTests class...
 *
 * @author John Blum
 * @since 1.0.0
 */
public class CacheServerPropertiesIntegrationTests {

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
	public void cacheServerConfigurationIsCorrect() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("gemfire.cache.server.port", "12480")
			.withProperty("spring.data.gemfire.cache.server.bind-address", "10.120.12.1")
			.withProperty("spring.data.gemfire.cache.server.max-connections", 500)
			.withProperty("spring.data.gemfire.cache.server.max-time-between-pings", 30000)
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.max-time-between-pings", 15000)
			.withProperty("spring.data.gemfire.cache.server.NonExistingCacheServer.max-time-between-pings", 5000)
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.subscription-capacity", 200)
			.withProperty("spring.data.gemfire.cache.server.port", "${gemfire.cache.server.port:12345}")
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.subscription-eviction-policy", "MEM");

		this.applicationContext = newApplicationContext(testPropertySource, TestCacheServerConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("TestCacheServer")).isTrue();

		CacheServer testCacheServer = this.applicationContext.getBean("TestCacheServer", CacheServer.class);

		assertThat(testCacheServer).isNotNull();
		assertThat(testCacheServer.getBindAddress()).isEqualTo("10.120.12.1");
		assertThat(testCacheServer.getHostnameForClients()).isEqualTo("skullbox");
		assertThat(testCacheServer.getLoadPollInterval()).isEqualTo(CacheServer.DEFAULT_LOAD_POLL_INTERVAL);
		assertThat(testCacheServer.getMaxConnections()).isEqualTo(1200);
		assertThat(testCacheServer.getMaximumMessageCount()).isEqualTo(400000);
		assertThat(testCacheServer.getMaxThreads()).isEqualTo(8);
		assertThat(testCacheServer.getMessageTimeToLive()).isEqualTo(600);
		assertThat(testCacheServer.getPort()).isEqualTo(12480);
		assertThat(testCacheServer.getSocketBufferSize()).isEqualTo(CacheServer.DEFAULT_SOCKET_BUFFER_SIZE);
		assertThat(testCacheServer.getTcpNoDelay()).isEqualTo(CacheServer.DEFAULT_TCP_NO_DELAY);

		ClientSubscriptionConfig testClientSubscriptionConfig = testCacheServer.getClientSubscriptionConfig();

		assertThat(testClientSubscriptionConfig).isNotNull();
		assertThat(testClientSubscriptionConfig.getCapacity()).isEqualTo(200);
		assertThat(testClientSubscriptionConfig.getDiskStoreName()).isEmpty();
		assertThat(testClientSubscriptionConfig.getEvictionPolicy()).isEqualTo("mem");
	}

	// TODO add more tests!

	@Configuration
	@EnableCacheServer(name = "TestCacheServer", bindAddress = "192.16.0.22", hostnameForClients = "skullbox",
		maxConnections = 100, maxThreads = 8, messageTimeToLive = 300, port = 11235, subscriptionCapacity = 100,
		subscriptionEvictionPolicy = SubscriptionEvictionPolicy.ENTRY)
	static class TestCacheServerConfiguration {

		@Bean("gemfireCache")
		GemFireCache mockGemFireCache() {
			return MockGemFireObjectsSupport.mockPeerCache();
		}

		@Bean
		CacheServerConfigurer testCacheServerConfigurer() {
			return (beanName, beanFactory) -> {
				beanFactory.setMaxConnections(1200);
				beanFactory.setMaxMessageCount(400000);
				beanFactory.setMessageTimeToLive(600);
			};
		}
	}
}
