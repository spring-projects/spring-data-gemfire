/*
 * Copyright 2010-2019 the original author or authors.
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
package org.springframework.data.gemfire.client;

import static org.assertj.core.api.Assertions.assertThat;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.data.gemfire.test.support.IOUtils;

/**
 * Integration tests for {@link org.apache.geode.cache.client.ClientCache}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.client.ClientCacheFactoryBean
 * @see EnableGemFireMockObjects
 */
public class ClientCacheIntegrationTests {

	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		return new AnnotationConfigApplicationContext(annotatedClasses);
	}

	private boolean testClientCacheClose(Class<?> clientCacheConfiguration) {

		ConfigurableApplicationContext applicationContext = null;

		try {
			applicationContext = newApplicationContext(clientCacheConfiguration);

			ClientCache clientCache = applicationContext.getBean(ClientCache.class);

			assertThat(clientCache.isClosed()).isFalse();

			applicationContext.close();

			return clientCache.isClosed();
		}
		finally {
			IOUtils.close(applicationContext);
		}
	}

	@Test
	public void clientCacheIsClosed() {
		assertThat(testClientCacheClose(ClosingClientCacheConfiguration.class)).isTrue();
	}

	@Test
	public void clientCacheIsNotClosed() {
		assertThat(testClientCacheClose(CloseSuppressingClientCacheConfiguration.class)).isFalse();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void multipleClientCachesAreTheSame() {

		ConfigurableApplicationContext applicationContextOne = newApplicationContext(MultiClientCacheConfiguration.class);
		ConfigurableApplicationContext applicationContextTwo = newApplicationContext(MultiClientCacheConfiguration.class);

		ClientCache clientCacheOne = applicationContextOne.getBean(ClientCache.class);
		ClientCache clientCacheTwo = applicationContextTwo.getBean(ClientCache.class);

		assertThat(clientCacheOne).isNotNull();
		assertThat(clientCacheTwo).isSameAs(clientCacheOne);

		Region regionOne = applicationContextOne.getBean(Region.class);
		Region regionTwo = applicationContextTwo.getBean(Region.class);

		assertThat(regionOne).isNotNull();
		assertThat(regionTwo).isSameAs(regionTwo);
		assertThat(clientCacheOne.isClosed()).isFalse();
		assertThat(regionOne.isDestroyed()).isFalse();

		applicationContextOne.close();

		assertThat(clientCacheOne.isClosed()).describedAs("ClientCache was closed").isFalse();
		assertThat(regionOne.isDestroyed()).describedAs("Region was destroyed").isFalse();
	}

	@Configuration
	@EnableGemFireMockObjects
	static class ClosingClientCacheConfiguration {

		@Bean
		ClientCacheFactoryBean gemfireCache() {

			ClientCacheFactoryBean clientCache = new ClientCacheFactoryBean();

			clientCache.setClose(true);

			return clientCache;
		}
	}

	@Configuration
	@EnableGemFireMockObjects
	static class CloseSuppressingClientCacheConfiguration {

		@Bean
		ClientCacheFactoryBean gemfireCache() {

			ClientCacheFactoryBean clientCache = new ClientCacheFactoryBean();

			clientCache.setClose(false);

			return clientCache;
		}
	}

	@Configuration
	@EnableGemFireMockObjects(useSingletonCache = true)
	static class MultiClientCacheConfiguration {

		@Bean
		ClientCacheFactoryBean gemfireCache() {

			ClientCacheFactoryBean clientCache = new ClientCacheFactoryBean();

			clientCache.setClose(false);

			return clientCache;
		}

		@Bean("Example")
		ClientRegionFactoryBean<Object, Object> exampleRegion(GemFireCache gemfireCache) {

			ClientRegionFactoryBean<Object, Object> exampleRegion = new ClientRegionFactoryBean<>();

			exampleRegion.setCache(gemfireCache);
			exampleRegion.setClose(false);
			exampleRegion.setDestroy(false);
			exampleRegion.setLookupEnabled(true);
			exampleRegion.setShortcut(ClientRegionShortcut.LOCAL);

			return exampleRegion;
		}
	}
}
