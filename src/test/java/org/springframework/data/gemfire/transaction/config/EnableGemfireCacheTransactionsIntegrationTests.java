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

package org.springframework.data.gemfire.transaction.config;

import static org.assertj.core.api.Java6Assertions.assertThat;

import javax.annotation.Resource;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.PeerCacheApplication;
import org.springframework.data.gemfire.transaction.GemfireTransactionManager;
import org.springframework.stereotype.Service;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.transaction.annotation.Transactional;

/**
 * Integration tests for {@link EnableGemfireCacheTransactions} and {@link GemfireCacheTransactionsConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.data.gemfire.config.annotation.PeerCacheApplication
 * @see org.springframework.data.gemfire.transaction.GemfireTransactionManager
 * @see org.springframework.data.gemfire.transaction.config.EnableGemfireCacheTransactions
 * @see org.springframework.data.gemfire.transaction.config.GemfireCacheTransactionsConfiguration
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @see org.springframework.transaction.annotation.Transactional
 * @since 2.0.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class EnableGemfireCacheTransactionsIntegrationTests {

	@Resource(name = "Example")
	private Region<Object, Object> example;

	@Autowired
	private GemFireCache gemfireCache;

	@Autowired
	private GemfireTransactionManager transactionManager;

	@Autowired
	private TestTransactionalService transactionalService;

	@Test
	public void transactionManagerIsConfigured() {
		assertThat(this.gemfireCache).isNotNull();
		assertThat(this.transactionManager).isNotNull();
		assertThat(this.transactionManager.getCache()).isSameAs(this.gemfireCache);
	}

	@Test
	public void doInTransactionCommits() {

		assertThat(example).isNotNull();
		assertThat(example).isEmpty();
		assertThat(transactionalService.doInTransactionCommits(1, "pass")).isTrue();
		assertThat(example).hasSize(1);
		assertThat(example).containsKey(1);
		assertThat(example.get(1)).isEqualTo("pass");
	}

	@Test(expected = RuntimeException.class)
	public void doInTransactionRollsback() {
		try {
			assertThat(example).doesNotContainKey(2);
			transactionalService.doInTransactionRollsBack(2, "fail");
		}
		catch (RuntimeException expected) {
			assertThat(expected).hasMessage("test");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			assertThat(example).doesNotContainKey(2);
		}
	}

	@SuppressWarnings("unused")
	@EnableGemfireCacheTransactions
	@PeerCacheApplication(name = "EnableGemfireCacheTransactionsIntegrationTests", logLevel = "warning")
	static class TestConfiguration {

		@Bean("Example")
		public LocalRegionFactoryBean<Object, Object> exampleRegion(GemFireCache gemFireCache) {

			LocalRegionFactoryBean<Object, Object> example = new LocalRegionFactoryBean<>();

			example.setCache(gemFireCache);
			example.setClose(false);
			example.setPersistent(false);

			return example;
		}

		@Bean
		TestTransactionalService transactionalService() {
			return new TestTransactionalService();
		}
	}

	@Service
	@SuppressWarnings("all")
	static class TestTransactionalService {

		@Resource(name = "Example")
		private Region<Object, Object> example;

		@Transactional
		public boolean doInTransactionCommits(Object key, Object value) {
			example.put(key, value);
			return true;
		}

		@Transactional
		public boolean doInTransactionRollsBack(Object key, Object value) {
			example.put(2, "fail");
			throw new RuntimeException("test");
		}
	}
}
