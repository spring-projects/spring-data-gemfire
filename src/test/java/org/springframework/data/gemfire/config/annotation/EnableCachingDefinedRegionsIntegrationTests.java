/*
 * Copyright 2017-2018 the original author or authors.
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

import java.util.concurrent.atomic.AtomicBoolean;

import javax.cache.annotation.CacheDefaults;
import javax.cache.annotation.CacheRemoveAll;
import javax.cache.annotation.CacheResult;

import org.apache.geode.cache.GemFireCache;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.cache.annotation.CachePut;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.stereotype.Service;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * The EnableCachingDefinedRegionsIntegrationTests class...
 *
 * @author John Blum
 * @since 1.0.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class EnableCachingDefinedRegionsIntegrationTests {

	@Autowired
	private ApplicationContext applicationContext;

	@Autowired
	private JCacheEchoService jcacheEchoService;

	@Autowired
	private SpringCacheableEchoService springEchoService;

	@Autowired
	private GemFireCache gemfireCache;

	@Before
	public void setup() {
		assertThat(this.gemfireCache).isNotNull();
	}

	@Test
	public void cacheRegionsExists() {

		assertThat(gemfireCache.getRegion("/Echo")).isNotNull();
		assertThat(gemfireCache.getRegion("/JCacheOne")).isNotNull();
		assertThat(gemfireCache.getRegion("/JCacheTwo")).isNotNull();
		assertThat(gemfireCache.getRegion("/SpringOne")).isNotNull();
	}

	@Test
	public void echoServiceCachingWithJCacheIsSuccessful() {

		assertThat(jcacheEchoService.isCacheMiss()).isFalse();
		assertThat(jcacheEchoService.echo("four")).isEqualTo("four");
		assertThat(jcacheEchoService.isCacheMiss()).isTrue();
		assertThat(jcacheEchoService.echo("five")).isEqualTo("five");
		assertThat(jcacheEchoService.isCacheMiss()).isTrue();
		assertThat(jcacheEchoService.echo("four")).isEqualTo("four");
		assertThat(jcacheEchoService.isCacheMiss()).isFalse();
		assertThat(jcacheEchoService.echo("six")).isEqualTo("six");
		assertThat(jcacheEchoService.isCacheMiss()).isTrue();
		assertThat(jcacheEchoService.echo("five")).isEqualTo("five");
		assertThat(jcacheEchoService.isCacheMiss()).isFalse();
	}

	@Test
	public void echoServiceCachingWithSpringIsSuccessful() {

		assertThat(springEchoService.isCacheMiss()).isFalse();
		assertThat(springEchoService.echo("one")).isEqualTo("one");
		assertThat(springEchoService.isCacheMiss()).isTrue();
		assertThat(springEchoService.echo("two")).isEqualTo("two");
		assertThat(springEchoService.isCacheMiss()).isTrue();
		assertThat(springEchoService.echo("one")).isEqualTo("one");
		assertThat(springEchoService.isCacheMiss()).isFalse();
		assertThat(springEchoService.echo("three")).isEqualTo("three");
		assertThat(springEchoService.isCacheMiss()).isTrue();
		assertThat(springEchoService.echo("two")).isEqualTo("two");
		assertThat(springEchoService.isCacheMiss()).isFalse();
	}

	@PeerCacheApplication(name = "EnableCachingDefinedRegionsIntegrationTests", logLevel = "warning")
	@EnableCachingDefinedRegions
	static class TestConfiguration {

		@Bean
		JCacheEchoService jcacheEchoService() {
			return new JCacheEchoService();
		}

		@Bean
		@Qualifier("Spring")
		SpringCacheableEchoService springEchoService() {
			return new SpringCacheableEchoService();
		}

		@Bean
		TestService testServiceOne() {
			return new SpringCachingTestService();
		}

		@Bean
		TestService testServiceTwo() {
			return new Jsr107CachingTestService();
		}
	}

	static abstract class AbstractCacheableService {

		private final AtomicBoolean cacheMiss = new AtomicBoolean(false);

		public boolean isCacheMiss() {
			return this.cacheMiss.compareAndSet(true, false);
		}

		public void setCacheMiss() {
			this.cacheMiss.set(true);
		}
	}

	@Service
	static class JCacheEchoService extends AbstractCacheableService {

		@CacheResult(cacheName = "Echo")
		public Object echo(String key) {
			setCacheMiss();
			return key;
		}
	}

	@Service
	static class SpringCacheableEchoService extends AbstractCacheableService {

		@Cacheable("Echo")
		public Object echo(String key) {
			setCacheMiss();
			return key;
		}
	}

	interface TestService {
		Object testMethod(String key);
	}

	@CacheDefaults(cacheName = "JCacheOne")
	@CacheRemoveAll(cacheName = "SpringOne")
	static class Jsr107CachingTestService implements TestService {

		@CacheResult(cacheName = "JCacheTwo")
		public Object testMethod(String key) {
			return null;
		}
	}

	static class SpringCachingTestService implements TestService {

		@CachePut("SpringOne")
		public Object testMethod(String key) {
			return "test";
		}
	}
}
