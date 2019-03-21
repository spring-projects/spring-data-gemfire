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

import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.gemfire.server.CacheServerFactoryBean;
import org.springframework.data.gemfire.test.GemfireTestBeanPostProcessor;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests for {@link CacheServerConfigurer}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.config.annotation.CacheServerConfigurer
 * @see org.springframework.data.gemfire.config.annotation.EnableCacheServer
 * @see org.springframework.data.gemfire.config.annotation.EnableCacheServers
 * @see org.springframework.data.gemfire.server.CacheServerFactoryBean
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 1.1.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class CacheServerConfigurerIntegrationTests {

	@Autowired
	@Qualifier("configurerOne")
	private TestCacheServerConfigurer configurerOne;

	@Autowired
	@Qualifier("configurerTwo")
	private TestCacheServerConfigurer configurerTwo;

	private void assertCacheServerConfigurerCalled(TestCacheServerConfigurer configurer,
			String... cacheServerBeanNames) {

		assertThat(configurer).isNotNull();
		assertThat(configurer).hasSize(cacheServerBeanNames.length);
		assertThat(configurer).contains(cacheServerBeanNames);
	}

	@Test
	public void cacheServerConfigurerOneCalledSuccessfully() {
		assertCacheServerConfigurerCalled(this.configurerOne,
			"gemfireCacheServer", "marsServer", "saturnServer", "venusServer");
	}

	@Test
	public void cacheServerConfigurerTwoCalledSuccessfully() {
		assertCacheServerConfigurerCalled(this.configurerTwo,
			"gemfireCacheServer", "marsServer", "saturnServer", "venusServer");
	}

	@Configuration
	@CacheServerApplication
	@EnableCacheServers(servers = {
		@EnableCacheServer(name = "marsServer"),
		@EnableCacheServer(name = "saturnServer"),
		@EnableCacheServer(name = "venusServer"),
	})
	static class TestConfiguration {

		@Bean
		GemfireTestBeanPostProcessor testBeanPostProcessor() {
			return new GemfireTestBeanPostProcessor();
		}

		@Bean
		TestCacheServerConfigurer configurerOne() {
			return new TestCacheServerConfigurer();
		}

		@Bean
		TestCacheServerConfigurer configurerTwo() {
			return new TestCacheServerConfigurer();
		}

		@Bean
		Object nonRelevantBean() {
			return "test";
		}
	}

	static class TestCacheServerConfigurer implements CacheServerConfigurer, Iterable<String> {

		private final Set<String> beanNames = new HashSet<>();

		@Override
		public void configure(String beanName, CacheServerFactoryBean bean) {
			this.beanNames.add(beanName);
		}

		@Override
		public Iterator<String> iterator() {
			return Collections.unmodifiableSet(this.beanNames).iterator();
		}
	}
}
