/*
 * Copyright 2019 the original author or authors.
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

import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;

import org.junit.After;
import org.junit.Test;

import org.springframework.cache.annotation.CacheConfig;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.stereotype.Service;

/**
 * Integration Tests for {@link EnableCachingDefinedRegions} and {@link CachingDefinedRegionsConfiguration} asserting
 * that the annotation config supports the Spring Cache Abstractions {@link CacheConfig} annotation.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.springframework.cache.annotation.CacheConfig
 * @see org.springframework.cache.annotation.Cacheable
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.annotation.AnnotationConfigApplicationContext
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.config.annotation.EnableCachingDefinedRegions
 * @see org.springframework.data.gemfire.config.annotation.CachingDefinedRegionsConfiguration
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects
 * @see org.springframework.stereotype.Service
 * @see <a href="https://jira.spring.io/browse/DATAGEODE-232">Add support for @CacheConfig in @EnableCachingDefinedRegions</a>
 * @since 2.2.0
 */
@SuppressWarnings("unused")
public class CachingDefinedRegionsConsidersCacheConfigCacheNamesIntegrationTests {

	private ConfigurableApplicationContext applicationContext;

	@After
	public void closeApplicationContext() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
	}

	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {

		AnnotationConfigApplicationContext applicationContext =
			new AnnotationConfigApplicationContext();

		applicationContext.register(annotatedClasses);
		applicationContext.registerShutdownHook();
		applicationContext.refresh();

		this.applicationContext = applicationContext;

		return applicationContext;
	}

	private Set<String> resolveCacheRegionNames(Class<?>... annotatedClasses) {

		newApplicationContext(annotatedClasses);

		GemFireCache cache = this.applicationContext.getBean(GemFireCache.class);

		assertThat(cache).isNotNull();

		return CollectionUtils.nullSafeSet(cache.rootRegions()).stream()
			.filter(Objects::nonNull)
			.map(Region::getName)
			.collect(Collectors.toSet());
	}

	@Test
	public void testServiceOneCacheRegionsAreCorrect() {
		assertThat(resolveCacheRegionNames(TestConfigurationOne.class)).containsExactlyInAnyOrder("A", "B");
	}

	@Test
	public void testServiceTwoCacheRegionsAreCorrect() {
		assertThat(resolveCacheRegionNames(TestConfigurationTwo.class)).containsExactlyInAnyOrder("A", "B");
	}

	@Test
	public void testServiceThreeCacheRegionsAreCorrect() {
		assertThat(resolveCacheRegionNames(TestConfigurationThree.class))
			.containsExactlyInAnyOrder("A", "B", "C");
	}

	@Test
	public void testServiceFourCacheRegionsAreCorrect() {
		assertThat(resolveCacheRegionNames(TestConfigurationFour.class))
			.containsExactlyInAnyOrder("A", "B", "C", "D", "F");
	}

	@ClientCacheApplication
	@EnableGemFireMockObjects
	@EnableCachingDefinedRegions
	static class TestConfigurationOne {

		@Bean
		TestServiceOne testServiceOne() {
			return new TestServiceOne();
		}
	}

	@ClientCacheApplication
	@EnableGemFireMockObjects
	@EnableCachingDefinedRegions
	static class TestConfigurationTwo {

		@Bean
		TestServiceTwo testServiceTwo() {
			return new TestServiceTwo();
		}
	}

	@ClientCacheApplication
	@EnableGemFireMockObjects
	@EnableCachingDefinedRegions
	static class TestConfigurationThree {

		@Bean
		TestServiceThree testServiceThree() {
			return new TestServiceThree();
		}
	}

	@ClientCacheApplication
	@EnableGemFireMockObjects
	@EnableCachingDefinedRegions
	static class TestConfigurationFour {

		@Bean
		TestServiceFour testServiceFour() {
			return new TestServiceFour();
		}
	}

	@Service
	@CacheConfig(cacheNames = { "A", "B" })
	static class TestServiceOne {

		public Object cacheableMethodOne() {
			return "ONE";
		}

		public Object cacheableMethodTwo() {
			return "TWO";
		}
	}

	@Service
	@CacheConfig(cacheNames = { "A", "B" })
	static class TestServiceTwo {

		@Cacheable
		public Object cacheableMethodOne() {
			return "ONE";
		}

		@Cacheable
		public Object cacheableMethodTwo() {
			return "TWO";
		}
	}

	@Service
	@CacheConfig(cacheNames = { "A", "B" })
	static class TestServiceThree {

		@Cacheable
		public Object cacheableMethodOne() {
			return "ONE";
		}

		@Cacheable("C")
		public Object cacheableMethodTwo() {
			return "TWO";
		}
	}

	@Service
	@CacheConfig(cacheNames = { "A", "B" })
	static class TestServiceFour {

		@Cacheable("C")
		public Object cacheableMethodOne() {
			return "ONE";
		}

		@Cacheable(cacheNames = { "A", "C", "D", "F" })
		public Object cacheableMethodTwo() {
			return "TWO";
		}
	}

}
