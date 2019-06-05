/*
 * Copyright 2018 the original author or authors.
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
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.spy;

import java.util.Optional;
import java.util.Properties;

import org.apache.geode.distributed.Locator;
import org.junit.After;
import org.junit.Test;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertySource;
import org.springframework.data.gemfire.LocatorFactoryBean;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.lang.Nullable;
import org.springframework.mock.env.MockPropertySource;

/**
 * Integration Tests for {@link LocatorApplication} and {@link LocatorApplicationConfiguration} asserting that
 * the {@link Locator} is configured with {@link Properties}.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.apache.geode.distributed.Locator
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.annotation.AnnotationConfigApplicationContext
 * @see org.springframework.core.env.PropertySource
 * @see org.springframework.data.gemfire.LocatorFactoryBean
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects
 * @see org.springframework.mock.env.MockPropertySource
 * @since 2.2.0
 */
@SuppressWarnings("unused")
public class LocatorApplicationPropertiesIntegrationTests {

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {

		Optional.ofNullable(this.applicationContext)
			.ifPresent(ConfigurableApplicationContext::close);
	}

	private ConfigurableApplicationContext newApplicationContext(PropertySource<?> testPropertySource,
			Class<?>... annotatedClasses) {

		AnnotationConfigApplicationContext applicationContext = new AnnotationConfigApplicationContext();

		MutablePropertySources propertySources = applicationContext.getEnvironment().getPropertySources();

		propertySources.addFirst(testPropertySource);

		applicationContext.register(annotatedClasses);
		applicationContext.registerShutdownHook();
		applicationContext.refresh();

		return applicationContext;
	}

	@Test
	public void locatorIsConfiguredWithProperties() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.locator.bind-address", "10.120.240.32")
			.withProperty("spring.data.gemfire.locator.hostname-for-clients", "skullbox")
			.withProperty("spring.data.gemfire.locator.log-level", "error")
			.withProperty("spring.data.gemfire.locator.name", "MockLocator")
			.withProperty("spring.data.gemfire.locator.port", 54321);

		this.applicationContext = newApplicationContext(testPropertySource, TestConfiguration.class);

		LocatorFactoryBean locatorFactoryBean = this.applicationContext.getBean(LocatorFactoryBean.class);

		assertThat(locatorFactoryBean).isNotNull();
		assertThat(locatorFactoryBean.getBindAddress().orElse(null)).isEqualTo("10.120.240.32");
		assertThat(locatorFactoryBean.getHostnameForClients().orElse(null)).isEqualTo("skullbox");
		assertThat(locatorFactoryBean.getLogLevel()).isEqualTo("error");
		assertThat(locatorFactoryBean.getName().orElse(null)).isEqualTo("MockLocator");
		assertThat(locatorFactoryBean.getPort()).isEqualTo(54321);
	}

	@EnableGemFireMockObjects
	@LocatorApplication(
		bindAddress = "10.105.210.16",
		hostnameForClients = "mailbox",
		logLevel = "info",
		name = "TestLocator",
		port = 12345
	)
	static class TestConfiguration {

		@Bean
		BeanPostProcessor locatorFactoryBeanPostProcessor() {

			return new BeanPostProcessor() {

				@Nullable @Override
				public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {

					if (bean instanceof LocatorFactoryBean) {

						LocatorFactoryBean locatorFactoryBean = spy((LocatorFactoryBean) bean);

						doNothing().when(locatorFactoryBean).init();

						bean = locatorFactoryBean;

					}

					return bean;
				}
			};
		}
	}
}
