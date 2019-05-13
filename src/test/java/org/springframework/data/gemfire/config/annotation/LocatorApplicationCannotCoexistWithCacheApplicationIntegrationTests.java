/*
 * Copyright 2018 the original author or authors.
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
import static org.assertj.core.api.Fail.fail;
import static org.mockito.Mockito.mock;

import java.util.Optional;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.distributed.Locator;
import org.junit.After;
import org.junit.Test;
import org.springframework.beans.factory.BeanDefinitionStoreException;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;

/**
 * Integration Tests for {@link LocatorApplication} and {@link LocatorApplicationConfiguration} asserting that
 * {@link GemFireCache} and {@link Locator} instances are mutually exclusive.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.distributed.Locator
 * @see org.springframework.data.gemfire.config.annotation.LocatorApplication
 * @see org.springframework.data.gemfire.config.annotation.LocatorApplicationConfiguration
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.annotation.AnnotationConfigApplicationContext
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects
 * @since 2.2.0
 */
@SuppressWarnings("unused")
public class LocatorApplicationCannotCoexistWithCacheApplicationIntegrationTests {

	private static final String GEMFIRE_LOG_LEVEL = "error";

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {

		Optional.ofNullable(this.applicationContext)
			.ifPresent(ConfigurableApplicationContext::close);
	}

	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {

		AnnotationConfigApplicationContext applicationContext = new AnnotationConfigApplicationContext();

		applicationContext.register(annotatedClasses);
		applicationContext.registerShutdownHook();
		applicationContext.refresh();

		return applicationContext;
	}

	private void testCacheAndLocatorApplication(Class<?> testConfiguration) {

		try {

			this.applicationContext = newApplicationContext(testConfiguration);
			this.applicationContext.getBean(GemFireCache.class);
			this.applicationContext.getBean(Locator.class);

			fail("Caches and Locators cannot coexist!");
		}
		catch (BeanDefinitionStoreException expected) {

			assertThat(expected)
				.hasMessage(LocatorApplicationConfiguration.EXCLUSIVE_LOCATOR_APPLICATION_ERROR_MESSAGE);

			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = BeanDefinitionStoreException.class)
	public void clientCacheAndLocatorApplicationThrowsException() {
		testCacheAndLocatorApplication(ClientCacheAndLocatorTestConfiguration.class);
	}

	@Test(expected = BeanDefinitionStoreException.class)
	public void locatorAndPeerCacheApplicationThrowsException() {
		testCacheAndLocatorApplication(LocatorAndPeerCacheTestConfiguration.class);
	}

	@EnableGemFireMockObjects
	@LocatorApplication(logLevel = GEMFIRE_LOG_LEVEL, port = 0)
	static class ClientCacheAndLocatorTestConfiguration {

		@Bean
		ClientCache mockClientCache() {
			return mock(ClientCache.class);
		}
	}

	@EnableGemFireMockObjects
	@LocatorApplication(logLevel = GEMFIRE_LOG_LEVEL, port = 0)
	@PeerCacheApplication(logLevel = GEMFIRE_LOG_LEVEL)
	static class LocatorAndPeerCacheTestConfiguration { }

}
