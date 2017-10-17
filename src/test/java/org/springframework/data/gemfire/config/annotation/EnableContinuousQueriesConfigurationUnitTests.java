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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.query.CqAttributes;
import org.apache.geode.cache.query.CqEvent;
import org.apache.geode.cache.query.CqQuery;
import org.apache.geode.cache.query.QueryService;
import org.junit.Test;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer;
import org.springframework.data.gemfire.listener.annotation.ContinuousQuery;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.stereotype.Component;

/**
 * Unit tests for {@link EnableContinuousQueries}, {@link ContinuousQueryConfiguration}, {@link ContinuousQuery}
 * and {@link ContinuousQueryListenerContainer}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.config.annotation.ContinuousQueryConfiguration
 * @see org.springframework.data.gemfire.config.annotation.EnableContinuousQueries
 * @see org.springframework.data.gemfire.listener.annotation.ContinuousQuery
 * @since 2.0.1
 */
public class EnableContinuousQueriesConfigurationUnitTests {

	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		return new AnnotationConfigApplicationContext(annotatedClasses);
	}

	@Test
	public void registersAndExecutesContinuousQueries() throws Exception {

		ConfigurableApplicationContext applicationContext =
			newApplicationContext(TestContinuousQueryRegistrationAndExecutionConfiguration.class);

		assertThat(applicationContext).isNotNull();
		assertThat(applicationContext.containsBean("DEFAULT")).isTrue();

		GemFireCache gemfireCache = applicationContext.getBean(ClientCache.class);

		assertThat(gemfireCache).isNotNull();

		QueryService mockQueryService = gemfireCache.getQueryService();

		assertThat(mockQueryService).isNotNull();
		assertThat(mockQueryService.getCqs()).hasSize(1);

		CqQuery mockCqQuery = mockQueryService.getCqs()[0];

		assertThat(mockCqQuery).isNotNull();
		assertThat(mockCqQuery.getName()).isEqualTo("TestQuery");
		assertThat(mockCqQuery.getQueryString()).isEqualTo("SELECT * FROM /Example");
		assertThat(mockCqQuery.isRunning()).isTrue();

		verify(mockQueryService, times(1)).newCq(eq("TestQuery"),
			eq("SELECT * FROM /Example"), any(CqAttributes.class), eq(false));

		verify(mockCqQuery, times(1)).execute();
	}

	@ClientCacheApplication
	@EnableContinuousQueries
	@EnableGemFireMockObjects
	@SuppressWarnings("unused")
	static class TestContinuousQueryRegistrationAndExecutionConfiguration {

		@Bean
		BeanFactoryPostProcessor dependsOnBeanFactoryPostProcessor() {

			return configurableListableBeanFactory -> {
				configurableListableBeanFactory.getBeanDefinition("continuousQueryListenerContainer")
					.setDependsOn("testComponent");
			};
		}

		@Bean("DEFAULT")
		Pool mockPool() {
			return mock(Pool.class);
		}

		@Bean
		@Order(Ordered.HIGHEST_PRECEDENCE)
		TestContinuousQueryComponent testComponent() {
			return new TestContinuousQueryComponent();
		}
	}

	@Component
	@SuppressWarnings("unused")
	static class TestContinuousQueryComponent {

		@ContinuousQuery(name = "TestQuery", query = "SELECT * FROM /Example")
		public void handle(CqEvent event) {
		}
	}
}
