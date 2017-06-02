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

import org.apache.geode.cache.Cache;
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
 * The PeerCachePropertiesIntegrationTests class...
 *
 * @author John Blum
 * @since 1.0.0
 */
public class PeerCachePropertiesIntegrationTests {

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
	public void peerCacheConfiguration() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.cache.copy-on-read", true)
			.withProperty("spring.data.gemfire.cache.critical-heap-percentage", 95.0f)
			.withProperty("spring.data.gemfire.cache.eviction-heap-percentage", 85.0f)
			.withProperty("spring.data.gemfire.cache.peer.lock-lease", 180)
			.withProperty("spring.data.gemfire.cache.peer.lock-timeout", 30)
			.withProperty("spring.data.gemfire.cache.peer.search-timeout", 120)
			.withProperty("spring.data.gemfire.pdx.ignore-unread-fields", false)
			.withProperty("spring.data.gemfire.pdx.persistent", true);

		this.applicationContext = newApplicationContext(testPropertySource, TestPeerCacheConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();
		assertThat(this.applicationContext.containsBean("mockPdxSerializer")).isTrue();

		Cache testPeerCache = this.applicationContext.getBean("gemfireCache", Cache.class);

		PdxSerializer mockPdxSerializer = this.applicationContext.getBean("mockPdxSerializer", PdxSerializer.class);

		assertThat(testPeerCache).isNotNull();
		assertThat(mockPdxSerializer).isNotNull();
		assertThat(testPeerCache.getLockLease()).isEqualTo(180);
		assertThat(testPeerCache.getLockTimeout()).isEqualTo(30);
		assertThat(testPeerCache.getPdxDiskStore()).isNull();
		assertThat(testPeerCache.getPdxIgnoreUnreadFields()).isFalse();
		assertThat(testPeerCache.getPdxPersistent()).isTrue();
		assertThat(testPeerCache.getPdxReadSerialized()).isTrue();
		assertThat(testPeerCache.getPdxSerializer()).isSameAs(mockPdxSerializer);
		assertThat(testPeerCache.getSearchTimeout()).isEqualTo(60);

		ResourceManager resourceManager = testPeerCache.getResourceManager();

		assertThat(resourceManager).isNotNull();
		assertThat(resourceManager.getCriticalHeapPercentage()).isEqualTo(95.0f);
		assertThat(resourceManager.getEvictionHeapPercentage()).isEqualTo(85.0f);
	}

	//TODO add more tests

	@EnableGemFireMocking
	@EnablePdx(ignoreUnreadFields = true, readSerialized = true, serializerBeanName = "mockPdxSerializer")
	@PeerCacheApplication(name = "TestPeerCache", criticalHeapPercentage = 90.0f, evictionHeapPercentage = 75.0f,
		lockLease = 300, lockTimeout = 120)
	static class TestPeerCacheConfiguration {

		@Bean
		PeerCacheConfigurer testPeerCacheConfigurer() {
			return (beanName, beanFactory) -> {
				beanFactory.setSearchTimeout(60);
			};
		}

		@Bean
		PdxSerializer mockPdxSerializer() {
			return mock(PdxSerializer.class);
		}
	}
}
