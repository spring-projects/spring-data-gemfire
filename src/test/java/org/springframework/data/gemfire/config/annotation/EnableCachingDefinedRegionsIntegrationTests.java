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

import java.util.concurrent.atomic.AtomicBoolean;

import org.apache.geode.cache.GemFireCache;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
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
	private CacheableEchoService echoService;

	@Autowired
	private GemFireCache gemfireCache;

	@Before
	public void setup() {

		//System.err.printf("@Cacheable Beans [%s]%n",
		//	Arrays.toString(applicationContext.getBeanNamesForAnnotation(Cacheable.class)));

		assertThat(this.gemfireCache).isNotNull();

		//System.err.printf("Cache Regions [%s]%n", this.gemfireCache.rootRegions().stream()
		//	.map(Region::getFullPath).collect(Collectors.toSet()));
	}

	@Test
	public void cacheRegionsExists() {

		assertThat(gemfireCache.getRegion("/Example")).isNotNull();
		assertThat(gemfireCache.getRegion("/Echo")).isNotNull();
	}

	@Test
	public void echoServiceOperationsAreSuccessful() {

		assertThat(echoService.isCacheMiss()).isFalse();
		assertThat(echoService.echo("one")).isEqualTo("one");
		assertThat(echoService.isCacheMiss()).isTrue();
		assertThat(echoService.echo("two")).isEqualTo("two");
		assertThat(echoService.isCacheMiss()).isTrue();
		assertThat(echoService.echo("one")).isEqualTo("one");
		assertThat(echoService.isCacheMiss()).isFalse();
		assertThat(echoService.echo("three")).isEqualTo("three");
		assertThat(echoService.isCacheMiss()).isTrue();
		assertThat(echoService.echo("two")).isEqualTo("two");
		assertThat(echoService.isCacheMiss()).isFalse();
	}

	@PeerCacheApplication(name = "EnableCachingDefinedRegionsIntegrationTests", logLevel = "warning")
	@EnableCachingDefinedRegions
	static class TestConfiguration {

		@Bean
		CacheableEchoService echoService() {
			return new CacheableEchoService();
		}

		@Bean
		TestService testService() {
			return new CachingTestService();
		}
	}

	@Service
	static class CacheableEchoService {

		private final AtomicBoolean cacheMiss = new AtomicBoolean(false);

		public boolean isCacheMiss() {
			return this.cacheMiss.compareAndSet(true, false);
		}

		@Cacheable("Echo")
		public Object echo(String key) {
			this.cacheMiss.set(true);
			return key;
		}
	}

	interface TestService {
		Object testMethod(String key);
	}

	static class CachingTestService implements TestService {

		@CachePut("Example")
		public Object testMethod(String key) {
			return "test";
		}
	}
}
