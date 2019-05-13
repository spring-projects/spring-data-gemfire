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
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Arrays;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.LocatorFactoryBean;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.lang.Nullable;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration Tests for {@link LocatorApplication} and {@link LocatorConfigurer}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.annotation.LocatorApplication
 * @see org.springframework.data.gemfire.config.annotation.LocatorApplicationConfiguration
 * @see org.springframework.data.gemfire.config.annotation.LocatorConfigurer
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.2.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class LocatorApplicationConfigurerIntegrationTests {

	private static final String GEMFIRE_LOG_LEVEL = "error";

	@Autowired
	private LocatorFactoryBean locatorFactoryBean;

	@Autowired
	@Qualifier("locatorConfigurerOne")
	private LocatorConfigurer locatorConfigurerOne;

	@Autowired
	@Qualifier("locatorConfigurerTwo")
	private LocatorConfigurer locatorConfigurerTwo;

	@Test
	public void locatorConfigurersInvoked() {

		assertThat(this.locatorFactoryBean).isNotNull();
		assertThat(this.locatorConfigurerOne).isNotNull();
		assertThat(this.locatorConfigurerTwo).isNotNull();

		Arrays.asList(this.locatorConfigurerOne, this.locatorConfigurerTwo).forEach(locatorConfigurer ->
			verify(locatorConfigurer, times(1))
				.configure(eq("locatorApplication"), eq(this.locatorFactoryBean)));
	}

	@EnableGemFireMockObjects
	@LocatorApplication(logLevel = GEMFIRE_LOG_LEVEL, port = 0)
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

		@Bean
		LocatorConfigurer locatorConfigurerOne() {
			return mock(LocatorConfigurer.class);
		}

		@Bean
		LocatorConfigurer locatorConfigurerTwo() {
			return mock(LocatorConfigurer.class);
		}
	}
}
