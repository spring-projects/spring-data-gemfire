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

import org.apache.geode.cache.server.CacheServer;
import org.apache.geode.cache.server.ClientSubscriptionConfig;
import org.junit.After;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertySource;
import org.springframework.data.gemfire.server.SubscriptionEvictionPolicy;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMocking;
import org.springframework.mock.env.MockPropertySource;

/**
 * Integration tests for {@link CacheServerApplication} and {@link EnableCacheServer}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.apache.geode.cache.server.ClientSubscriptionConfig
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.core.env.PropertySource
 * @see org.springframework.data.gemfire.config.annotation.CacheServerApplication
 * @see org.springframework.data.gemfire.server.SubscriptionEvictionPolicy
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMocking
 * @since 2.0.0
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

	private void assertCacheServer(CacheServer cacheServer, String bindAddress, String hostnameForClients,
			long loadPollInterval, int maxConnections, int maxMessageCount, int maxThreads, int maxTimeBetweenPings,
			int messageTimeToLive, int port, int socketBufferSize, int subscriptionCapacity,
			String subscriptionDiskStoreName, SubscriptionEvictionPolicy subscriptionEvictionPolicy, boolean tcpNoDelay) {

		assertThat(cacheServer).isNotNull();
		assertThat(cacheServer.getBindAddress()).isEqualTo(bindAddress);
		assertThat(cacheServer.getHostnameForClients()).isEqualTo(hostnameForClients);
		assertThat(cacheServer.getLoadPollInterval()).isEqualTo(loadPollInterval);
		assertThat(cacheServer.getMaxConnections()).isEqualTo(maxConnections);
		assertThat(cacheServer.getMaximumMessageCount()).isEqualTo(maxMessageCount);
		assertThat(cacheServer.getMaxThreads()).isEqualTo(maxThreads);
		assertThat(cacheServer.getMaximumTimeBetweenPings()).isEqualTo(maxTimeBetweenPings);
		assertThat(cacheServer.getMessageTimeToLive()).isEqualTo(messageTimeToLive);
		assertThat(cacheServer.getPort()).isEqualTo(port);
		assertThat(cacheServer.getSocketBufferSize()).isEqualTo(socketBufferSize);
		assertThat(cacheServer.getTcpNoDelay()).isEqualTo(tcpNoDelay);

		ClientSubscriptionConfig clientSubscriptionConfig = cacheServer.getClientSubscriptionConfig();

		assertThat(clientSubscriptionConfig).isNotNull();
		assertThat(clientSubscriptionConfig.getCapacity()).isEqualTo(subscriptionCapacity);
		assertThat(clientSubscriptionConfig.getDiskStoreName()).isEqualTo(subscriptionDiskStoreName);
		assertThat(clientSubscriptionConfig.getEvictionPolicy()).isEqualTo(subscriptionEvictionPolicy.toString().toLowerCase());
	}
	@Test
	public void cacheServerConfiguration() {

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

	@Test
	public void cacheServersConfiguration() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.cache.server.bind-address", "192.168.0.2")
			.withProperty("spring.data.gemfire.cache.server.hostname-for-clients", "skullbox")
			.withProperty("spring.data.gemfire.cache.server.load-poll-interval", 10000L)
			.withProperty("spring.data.gemfire.cache.server.max-connections", 500)
			.withProperty("spring.data.gemfire.cache.server.max-message-count", 451000)
			.withProperty("spring.data.gemfire.cache.server.max-threads", 8)
			.withProperty("spring.data.gemfire.cache.server.max-time-between-pings", 30000)
			.withProperty("spring.data.gemfire.cache.server.message-time-to-live", 60)
			.withProperty("spring.data.gemfire.cache.server.port", 41414)
			.withProperty("spring.data.gemfire.cache.server.socket-buffer-size", 16384)
			.withProperty("spring.data.gemfire.cache.server.subscription-capacity", 21)
			.withProperty("spring.data.gemfire.cache.server.subscription-disk-store-name", "TestDiskStore")
			.withProperty("spring.data.gemfire.cache.server.subscription-eviction-policy", "MEM")
			.withProperty("spring.data.gemfire.cache.server.tcp-no-delay", false)
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.bind-address", "10.121.12.1")
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.hostname-for-clients", "jambox")
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.load-poll-interval", "15000")
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.max-connections", 200)
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.max-message-count", 651000)
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.max-threads", 16)
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.max-time-between-pings", 15000)
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.message-time-to-live", 120)
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.port", 42424)
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.socket-buffer-size", 65536)
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.subscription-capacity", 42)
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.subscription-disk-store-name", "JunkDiskStore")
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.subscription-eviction-policy", "ENTRY")
			.withProperty("spring.data.gemfire.cache.server.TestCacheServer.tcp-no-delay", true);

		this.applicationContext = newApplicationContext(testPropertySource, TestCacheServersConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("TestCacheServer")).isTrue();

		CacheServer gemfirCacheServer = this.applicationContext.getBean("gemfireCacheServer", CacheServer.class);

		assertCacheServer(gemfirCacheServer, "192.168.0.2", "skullbox", 10000L,
			500, 451000, 8, 30000,
			60, 41414, 16384, 21,
			"TestDiskStore", SubscriptionEvictionPolicy.MEM, false);

		CacheServer testCacheServer = this.applicationContext.getBean("TestCacheServer", CacheServer.class);

		assertCacheServer(testCacheServer, "10.121.12.1", "jambox", 15000L,
			200, 651000, 16, 15000,
			120, 42424, 65536, 42,
			"JunkDiskStore", SubscriptionEvictionPolicy.ENTRY, true);
	}

	@PeerCacheApplication
	@EnableGemFireMocking
	@EnableCacheServer(name = "TestCacheServer", bindAddress = "192.16.0.22", hostnameForClients = "skullbox",
		maxConnections = 100, maxThreads = 8, messageTimeToLive = 300, port = 11235, subscriptionCapacity = 100,
		subscriptionEvictionPolicy = SubscriptionEvictionPolicy.ENTRY)
	@SuppressWarnings("unused")
	static class TestCacheServerConfiguration {

		@Bean
		CacheServerConfigurer testCacheServerConfigurer() {
			return (beanName, beanFactory) -> {
				beanFactory.setMaxConnections(1200);
				beanFactory.setMaxMessageCount(400000);
				beanFactory.setMessageTimeToLive(600);
			};
		}
	}

	@CacheServerApplication
	@EnableGemFireMocking
	@EnableCacheServer(name = "TestCacheServer")
	static class TestCacheServersConfiguration {
	}
}
