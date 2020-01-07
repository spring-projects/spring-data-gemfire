/*
 * Copyright 2017-2020 the original author or authors.
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
import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.apache.geode.cache.client.ClientCache;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.client.ClientCacheFactoryBean;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests for {@link ClientCacheConfigurer}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.ClientCacheApplication
 * @see org.springframework.data.gemfire.config.annotation.ClientCacheConfigurer
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.1.0
 */
@RunWith(SpringRunner.class)
@SuppressWarnings("unused")
public class ClientCacheConfigurerIntegrationTests {

	private static final AtomicBoolean testClientCacheConfigurerThreeCalled = new AtomicBoolean(false);

	private static final String GEMFIRE_LOG_LEVEL = "error";

	@Autowired
	private ClientCache clientCache;

	@Autowired
	@Qualifier("testClientCacheConfigurerOne")
	private TestClientCacheConfigurer configurerOne;

	@Autowired
	@Qualifier("testClientCacheConfigurerTwo")
	private TestClientCacheConfigurer configurerTwo;

	@Before
	public void setup() {
		assertThat(this.clientCache).isNotNull();
	}

	private void assertClientCacheConfigurerInvokedSuccessfully(TestClientCacheConfigurer clientCacheConfigurer,
			String... beanNames) {

		assertThat(clientCacheConfigurer).isNotNull();
		assertThat(clientCacheConfigurer).hasSize(beanNames.length);
		assertThat(clientCacheConfigurer).contains(beanNames);
	}

	@Test
	public void clientCacheConfigurerOneCalledSuccessfully() {
		assertClientCacheConfigurerInvokedSuccessfully(this.configurerOne, "gemfireCache");
	}

	@Test
	public void clientCacheConfigurerTwoCalledSuccessfully() {
		assertClientCacheConfigurerInvokedSuccessfully(this.configurerTwo, "gemfireCache");
	}

	@Test
	public void clientCacheConfigurerThreeCalledSuccessfully() {
		assertThat(testClientCacheConfigurerThreeCalled.get()).isTrue();
	}

	@EnableGemFireMockObjects
	@ClientCacheApplication(logLevel = GEMFIRE_LOG_LEVEL)
	static class TestConfiguration {

		@Bean
		TestClientCacheConfigurer testClientCacheConfigurerOne() {
			return new TestClientCacheConfigurer();
		}

		@Bean
		TestClientCacheConfigurer testClientCacheConfigurerTwo() {
			return new TestClientCacheConfigurer();
		}

		@Bean
		ClientCacheConfigurer testClientCacheConfigurerThree() {
			return (beanName, bean) -> testClientCacheConfigurerThreeCalled.set(true);
		}

		@Bean
		String nonRelevantBean() {
			return "test";
		}
	}

	static final class TestClientCacheConfigurer implements ClientCacheConfigurer, Iterable<String> {

		private Set<String> beanNames = new HashSet<>();

		@Override
		public void configure(String beanName, ClientCacheFactoryBean bean) {
			this.beanNames.add(beanName);
		}

		@Override
		public Iterator<String> iterator() {
			return Collections.unmodifiableSet(this.beanNames).iterator();
		}
	}
}
