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

import java.util.Optional;

import org.junit.After;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;

/**
 * Integration test for {@link EnableBeanFactoryLocator} and {@link BeanFactoryLocatorConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.BeanFactoryLocatorConfiguration
 * @see org.springframework.data.gemfire.config.annotation.EnableBeanFactoryLocator
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects
 * @since 2.0.0
 */
public class EnableBeanFactoryLocatorConfigurationIntegrationTests {

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
	}

	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		ConfigurableApplicationContext applicationContext = new AnnotationConfigApplicationContext(annotatedClasses);
		applicationContext.registerShutdownHook();
		return applicationContext;
	}

	@Test
	public void gemfireBeanFactoryLocatorIsDisabled() {

		this.applicationContext = newApplicationContext(TestBeanFactoryLocatorDisabledConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();

		CacheFactoryBean gemfireCache = this.applicationContext.getBean("&gemfireCache", CacheFactoryBean.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.isUseBeanFactoryLocator()).isFalse();
	}

	@Test
	public void gemfireBeanFactoryLocatorIsEnabled() {

		this.applicationContext = newApplicationContext(TestBeanFactoryLocatorEnabledConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();

		CacheFactoryBean gemfireCache = this.applicationContext.getBean("&gemfireCache", CacheFactoryBean.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.isUseBeanFactoryLocator()).isTrue();
	}

	@EnableGemFireMockObjects
	@PeerCacheApplication
	static class TestBeanFactoryLocatorDisabledConfiguration {
	}

	@EnableGemFireMockObjects
	@EnableBeanFactoryLocator
	@PeerCacheApplication
	static class TestBeanFactoryLocatorEnabledConfiguration {
	}
}
