/*
 * Copyright 2018-2020 the original author or authors.
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.stereotype.Service;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration Tests for {@link EnableCachingDefinedRegions} and {@link CachingDefinedRegionsConfiguration} to assert
 * that {@link RegionConfigurer RegionConfigurers} beans are applied to the caching-defined {@link Region Regions}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.springframework.cache.annotation.Cacheable
 * @see org.springframework.data.gemfire.config.annotation.CachingDefinedRegionsConfiguration
 * @see org.springframework.data.gemfire.config.annotation.EnableCachingDefinedRegions
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.2.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class CachingDefinedRegionsAppliesRegionConfigurersIntegrationTests {

	private static final List<String> configuredRegionNames = Collections.synchronizedList(new ArrayList<>());

	@Autowired
	private GemFireCache clientCache;

	@Test
	public void clientCacheContainsCachingDefinedRegions() {

		assertThat(this.clientCache).isNotNull();

		assertThat(this.clientCache.rootRegions().stream()
			.filter(Objects::nonNull)
			.map(region -> region.getName())
			.collect(Collectors.toList())).containsExactlyInAnyOrder("TestCacheOne", "TestCacheTwo");
	}

	@Test
	public void regionConfigurerWasAppliedToCachingDefinedRegions() {
		assertThat(configuredRegionNames).containsExactlyInAnyOrder("TestCacheOne", "TestCacheTwo");
	}

	@ClientCacheApplication
	@EnableGemFireMockObjects
	@EnableCachingDefinedRegions(clientRegionShortcut = ClientRegionShortcut.LOCAL)
	static class TestConfiguration {

		@Bean
		TestCacheableService testCacheableService() {
			return new TestCacheableService();
		}

		@Bean
		RegionConfigurer testRegionConfigurer() {

			return new RegionConfigurer() {

				@Override
				public void configure(String beanName, ClientRegionFactoryBean<?, ?> bean) {
					configuredRegionNames.add(beanName);
				}
			};
		}
	}

	@Service
	static class TestCacheableService {

		@Cacheable("TestCacheOne")
		public Object testCacheableOperationOne(String key) {
			return "TEST";
		}

		@Cacheable("TestCacheTwo")
		public Object testCacheableOperationTwo(String key) {
			return "MOCK";
		}
	}
}
