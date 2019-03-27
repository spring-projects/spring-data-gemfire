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

package org.springframework.data.gemfire.cache.config;

import static org.assertj.core.api.Java6Assertions.assertThat;

import javax.annotation.Resource;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.shiro.util.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.cache.GemfireCacheManager;
import org.springframework.data.gemfire.config.annotation.PeerCacheApplication;
import org.springframework.stereotype.Service;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests for {@link EnableGemfireCaching} and {@link GemfireCachingConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.apache.geode.cache.GemFireCache
 * @see org.springframework.cache.annotation.Cacheable
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.data.gemfire.cache.config.EnableGemfireCaching
 * @see org.springframework.data.gemfire.cache.config.GemfireCachingConfiguration
 * @see org.springframework.data.gemfire.config.annotation.PeerCacheApplication
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.0.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
public class EnableGemfireCachingIntegrationTests {

	@Autowired
	private CalculatorService calculator;

	@Autowired
	@SuppressWarnings("unused")
	private GemfireCacheManager gemfireCacheManager;

	@Test
	public void gemfireCacheManagerIsConfigured() {
		assertThat(this.gemfireCacheManager).isNotNull();
	}

	@Test
	public void enableGemfireCachingIsSuccessful() {

		assertThat(calculator.isCacheMiss()).isFalse();
		assertThat(calculator.factorial(0L)).isEqualTo(1L);
		assertThat(calculator.isCacheMiss()).isTrue();
		assertThat(calculator.factorial(1L)).isEqualTo(1L);
		assertThat(calculator.isCacheMiss()).isTrue();
		assertThat(calculator.factorial(2L)).isEqualTo(2L);
		assertThat(calculator.isCacheMiss()).isTrue();
		assertThat(calculator.factorial(0L)).isEqualTo(1L);
		assertThat(calculator.isCacheMiss()).isFalse();
		assertThat(calculator.factorial(1L)).isEqualTo(1L);
		assertThat(calculator.isCacheMiss()).isFalse();
		assertThat(calculator.factorial(2L)).isEqualTo(2L);
		assertThat(calculator.isCacheMiss()).isFalse();
		assertThat(calculator.factorial(3L)).isEqualTo(6L);
		assertThat(calculator.isCacheMiss()).isTrue();
		assertThat(calculator.factorial(4L)).isEqualTo(24L);
		assertThat(calculator.isCacheMiss()).isTrue();
		assertThat(calculator.factorial(5L)).isEqualTo(120L);
		assertThat(calculator.isCacheMiss()).isTrue();
	}

	@Service
	static class CalculatorService {

		private volatile boolean cacheMiss;

		@Resource(name = "Factorials")
		private Region<Long, Long> factorials;

		public boolean isCacheMiss() {
			boolean cacheMiss = this.cacheMiss;
			this.cacheMiss = false;
			return cacheMiss;
		}

		@Cacheable("Factorials")
		public Long factorial(Long number) {

			Assert.notNull(number, "Number to compute the factorial of must not be null");

			Assert.isTrue(number > -1,
				String.format("Number [%d] must be greater than equal to 0", number));

			cacheMiss = true;

			if (number < 3) {
				return (number > 1L ? 2L : 1L);
			}

			long result = number;

			while (number-- > 1) {
				result *= number;
			}

			return result;
		}
	}

	@EnableGemfireCaching
	@PeerCacheApplication(name = "EnableGemfireCachingIntegrationTests", logLevel = "warning")
	@SuppressWarnings("unused")
	static class TestConfiguration {

		@Bean
		CalculatorService calculatorService() {
			return new CalculatorService();
		}

		@Bean("Factorials")
		LocalRegionFactoryBean<Long, Long> factorialsRegion(GemFireCache gemfireCache) {

			LocalRegionFactoryBean<Long, Long> factorials = new LocalRegionFactoryBean<>();

			factorials.setCache(gemfireCache);
			factorials.setClose(false);
			factorials.setPersistent(false);

			return factorials;
		}
	}
}
