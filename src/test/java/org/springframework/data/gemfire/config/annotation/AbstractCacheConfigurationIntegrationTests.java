/*
 * Copyright 2017-2019 the original author or authors.
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

import java.util.Optional;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.client.ClientCache;
import org.junit.After;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertySource;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.mock.env.MockPropertySource;

/**
 * Integration tests for {@link AbstractCacheConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.config.annotation.AbstractCacheConfiguration
 * @since 2.0.2
 */
public class AbstractCacheConfigurationIntegrationTests {

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
	}

	private void assertName(GemFireCache gemfireCache, String name) {

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();
		assertThat(gemfireCache.getDistributedSystem().getProperties()).isNotNull();
		assertThat(gemfireCache.getDistributedSystem().getProperties().getProperty("name")).isEqualTo(name);
	}

	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		return newApplicationContext(null, annotatedClasses);
	}

	private ConfigurableApplicationContext newApplicationContext(PropertySource<?> testPropertySource,
			Class<?>... annotatedClasses) {

		AnnotationConfigApplicationContext applicationContext = new AnnotationConfigApplicationContext();

		Optional.ofNullable(testPropertySource).ifPresent(it -> {

			MutablePropertySources propertySources = applicationContext.getEnvironment().getPropertySources();

			propertySources.addFirst(testPropertySource);
		});

		applicationContext.registerShutdownHook();
		applicationContext.register(annotatedClasses);
		applicationContext.refresh();

		return applicationContext;
	}

	@Test
	public void clientCacheNameUsesAnnotationNameAttributeDefaultValue() {

		this.applicationContext = newApplicationContext(TestClientCacheConfiguration.class);

		GemFireCache peerCache = this.applicationContext.getBean("gemfireCache", ClientCache.class);

		assertName(peerCache, ClientCacheConfiguration.DEFAULT_NAME);
	}

	@Test
	public void clientCacheNameUsesSpringDataGemFireNameProperty() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.name", "TestClient");

		this.applicationContext = newApplicationContext(testPropertySource, TestClientCacheConfiguration.class);

		GemFireCache peerCache = this.applicationContext.getBean("gemfireCache", ClientCache.class);

		assertName(peerCache, "TestClient");
	}

	@Test
	public void peerCacheNameUsesAnnotationNameAttributeConfiguredValue() {

		this.applicationContext = newApplicationContext(TestPeerCacheConfiguration.class);

		GemFireCache peerCache = this.applicationContext.getBean("gemfireCache", Cache.class);

		assertName(peerCache, "TestPeerCacheApp");
	}

	@Test
	public void peerCacheNameUsesSpringDataGemFireCacheNameProperty() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.cache.name", "TestPeer");

		this.applicationContext = newApplicationContext(testPropertySource, TestPeerCacheConfiguration.class);

		GemFireCache peerCache = this.applicationContext.getBean("gemfireCache", Cache.class);

		assertName(peerCache, "TestPeer");
	}

	@ClientCacheApplication
	@EnableGemFireMockObjects
	static class TestClientCacheConfiguration {
	}

	@PeerCacheApplication(name = "TestPeerCacheApp")
	@EnableGemFireMockObjects
	static class TestPeerCacheConfiguration {
	}
}
