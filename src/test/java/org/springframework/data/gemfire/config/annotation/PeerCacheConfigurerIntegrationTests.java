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

import org.apache.geode.cache.Cache;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.test.GemfireTestBeanPostProcessor;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests for {@link PeerCacheConfigurer}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.apache.geode.cache.Cache
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.PeerCacheApplication
 * @see org.springframework.data.gemfire.config.annotation.PeerCacheConfigurer
 * @see org.springframework.data.gemfire.test.GemfireTestBeanPostProcessor
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 1.0.0
 */
@RunWith(SpringRunner.class)
@SuppressWarnings("unused")
public class PeerCacheConfigurerIntegrationTests {

	@Autowired
	private Cache peerCache;

	@Autowired
	@Qualifier("testPeerCacheConfigurerOne")
	private TestPeerCacheConfigurer configurerOne;

	@Autowired
	@Qualifier("testPeerCacheConfigurerTwo")
	private TestPeerCacheConfigurer configurerTwo;

	@Before
	public void setup() {
		assertThat(this.peerCache).isNotNull();
	}

	private void assertTestPeerCacheConfigurerCalledSuccessully(TestPeerCacheConfigurer peerCacheConfigurer,
			String... beanNames) {

		assertThat(peerCacheConfigurer).isNotNull();
		assertThat(peerCacheConfigurer).hasSize(beanNames.length);
		assertThat(peerCacheConfigurer).contains(beanNames);
	}

	@Test
	public void peerCacheConfigurerOneCalledSuccessfully() {
		assertTestPeerCacheConfigurerCalledSuccessully(this.configurerOne, "gemfireCache");
	}

	@Test
	public void peerCacheConfigurerTwoCalledSuccessfully() {
		assertTestPeerCacheConfigurerCalledSuccessully(this.configurerTwo, "gemfireCache");
	}

	@PeerCacheApplication
	static class TestConfiguration {

		@Bean
		GemfireTestBeanPostProcessor testBeanPostProcessor() {
			return new GemfireTestBeanPostProcessor();
		}

		@Bean
		TestPeerCacheConfigurer testPeerCacheConfigurerOne() {
			return new TestPeerCacheConfigurer();
		}

		@Bean
		TestPeerCacheConfigurer testPeerCacheConfigurerTwo() {
			return new TestPeerCacheConfigurer();
		}

		@Bean
		String nonRelevantBean() {
			return "test";
		}
	}

	static class TestPeerCacheConfigurer implements Iterable<String>, PeerCacheConfigurer {

		private Set<String> beanNames = new HashSet<>();

		@Override
		public void configure(String beanName, CacheFactoryBean bean) {
			this.beanNames.add(beanName);
		}

		@Override
		public Iterator<String> iterator() {
			return Collections.unmodifiableSet(this.beanNames).iterator();
		}
	}
}
