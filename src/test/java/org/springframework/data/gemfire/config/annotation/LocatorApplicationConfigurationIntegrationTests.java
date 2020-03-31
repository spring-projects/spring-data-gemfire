/*
 * Copyright 2020 the original author or authors.
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

import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.LocatorFactoryBean;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.lang.Nullable;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration Tests for {@link LocatorApplication} and {@link LocatorApplicationConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.beans.factory.config.BeanPostProcessor
 * @see org.springframework.data.gemfire.LocatorFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.LocatorApplication
 * @see org.springframework.data.gemfire.config.annotation.LocatorApplicationConfiguration
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.3.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class LocatorApplicationConfigurationIntegrationTests {

	@Autowired
	private LocatorFactoryBean locatorFactoryBean;

	@Test
	public void locatorFactoryBeanWasConfiguredFromAnnotationAttributes() {

		assertThat(this.locatorFactoryBean).isNotNull();
		assertThat(this.locatorFactoryBean.getBindAddress().orElse(null)).isEqualTo("10.101.202.8");
		assertThat(this.locatorFactoryBean.getHostnameForClients().orElse(null)).isEqualTo("cardboardBox");
		assertThat(this.locatorFactoryBean.getLocators().orElse(null)).isEqualTo("host1[1234],host2[6789]");
		assertThat(this.locatorFactoryBean.getLogLevel()).isEqualTo("WARN");
		assertThat(this.locatorFactoryBean.getName().orElse(null)).isEqualTo("MockLocator");
		assertThat(this.locatorFactoryBean.getPort()).isEqualTo(9876);
	}

	@EnableGemFireMockObjects
	@LocatorApplication(
		bindAddress = "10.101.202.8",
		hostnameForClients = "cardboardBox",
		locators = "host1[1234],host2[6789]",
		logLevel = "WARN",
		name = "MockLocator",
		port = 9876
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
