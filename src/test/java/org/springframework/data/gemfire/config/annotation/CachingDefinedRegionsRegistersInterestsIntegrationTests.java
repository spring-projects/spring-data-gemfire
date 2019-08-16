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
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Arrays;

import javax.annotation.Resource;

import org.apache.geode.cache.InterestResultPolicy;
import org.apache.geode.cache.Region;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.Lifecycle;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.client.Interest;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.stereotype.Service;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration Tests asserting that caching-defined {@link Region Regions} receive a {@link Lifecycle#start()} callback
 * to register interests.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.Region
 * @see org.springframework.cache.annotation.Cacheable
 * @see org.springframework.context.Lifecycle
 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
 * @see org.springframework.data.gemfire.client.Interest
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @see <a href="https://jira.spring.io/browse/DATAGEODE-219">DATAGEODE-219</a>
 * @since 2.2.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings({ "unchecked", "unused" })
public class CachingDefinedRegionsRegistersInterestsIntegrationTests {

	private static final Interest testInterest =
		new Interest<>("TestKey", InterestResultPolicy.KEYS_VALUES, false, true);

	@Resource(name = "CacheOne")
	private Region cacheOne;

	@Resource(name = "CacheTwo")
	private Region cacheTwo;

	@Test
	public void cachesRegisterInterestInTestKey() {

		assertThat(this.cacheOne).isNotNull();
		assertThat(this.cacheOne.getName()).isEqualTo("CacheOne");
		assertThat(this.cacheTwo).isNotNull();
		assertThat(this.cacheTwo.getName()).isEqualTo("CacheTwo");

		Arrays.asList(this.cacheOne, this.cacheTwo).forEach(cache -> {
			verify(cache, times(1))
				.registerInterest(eq(testInterest.getKey()), eq(testInterest.getPolicy()), eq(testInterest.isDurable()),
					eq(testInterest.isReceiveValues()));
		});
	}

	@ClientCacheApplication
	@EnableCachingDefinedRegions
	@EnableGemFireMockObjects
	static class TestConfiguration {

		@Bean
		RegionConfigurer interestsRegisteringRegionConfigurer() {

			return new RegionConfigurer() {

				@Override
				public void configure(String beanName, ClientRegionFactoryBean<?, ?> bean) {
					bean.setInterests(ArrayUtils.asArray(testInterest));
				}
			};
		}

		@Bean
		CacheableServiceObject cacheableServiceObject() {
			return new CacheableServiceObject();
		}
	}

	@Service
	static class CacheableServiceObject {

		@Cacheable("CacheOne")
		public Object cacheableOperationOne(String input) {
			return "FROM-CACHE-ONE";
		}

		@Cacheable("CacheTwo")
		public Object cacheableOperationTwo(String input) {
			return "FROM-CACHE-TWO";
		}
	}
}
