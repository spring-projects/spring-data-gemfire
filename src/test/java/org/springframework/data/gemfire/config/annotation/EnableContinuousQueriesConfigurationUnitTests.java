/*
 * Copyright 2017-2019 the original author or authors.
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
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.ArrayUtils.asArray;

import java.lang.reflect.Proxy;
import java.util.concurrent.Executor;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.query.CqAttributes;
import org.apache.geode.cache.query.CqEvent;
import org.apache.geode.cache.query.CqQuery;
import org.apache.geode.cache.query.QueryService;
import org.junit.Test;
import org.springframework.aop.framework.ProxyFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.data.annotation.Id;
import org.springframework.data.gemfire.RegionAttributesFactoryBean;
import org.springframework.data.gemfire.listener.ContinuousQueryListenerContainer;
import org.springframework.data.gemfire.listener.annotation.ContinuousQuery;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.repository.config.EnableGemfireRepositories;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean;
import org.springframework.data.gemfire.test.mock.GemFireMockObjectsSupport;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.data.gemfire.test.model.Person;
import org.springframework.data.gemfire.test.repo.PersonRepository;
import org.springframework.data.gemfire.test.support.IOUtils;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Component;
import org.springframework.util.ErrorHandler;

import lombok.Data;

/**
 * Unit tests for {@link EnableContinuousQueries}, {@link ContinuousQueryConfiguration}, {@link ContinuousQuery}
 * and {@link ContinuousQueryListenerContainer}.
 *
 * @author John Blum
 * @see java.lang.reflect.Proxy
 * @see org.junit.Test
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.query.CqQuery
 * @see org.apache.geode.cache.query.QueryService
 * @see org.springframework.aop.framework.ProxyFactory
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
	public void continuousQueryListenerContainerConfigurationIsCorrect() {

		ConfigurableApplicationContext applicationContext =
			newApplicationContext(TestContinuousQueryListenerContainerConfiguration.class);

		try {

			ErrorHandler mockErrorHandler = applicationContext.getBean("mockErrorHandler", ErrorHandler.class);

			Pool mockPool = applicationContext.getBean("mockPool", Pool.class);

			QueryService mockQueryService = applicationContext.getBean("mockQueryService", QueryService.class);

			Executor mockTaskExecutor = applicationContext.getBean("mockTaskExecutor", Executor.class);

			assertThat(applicationContext.containsBean("continuousQueryListenerContainer")).isTrue();

			ContinuousQueryListenerContainer container =
				applicationContext.getBean("continuousQueryListenerContainer",
					ContinuousQueryListenerContainer.class);

			assertThat(container).isNotNull();
			assertThat(container.getErrorHandler().orElse(null)).isEqualTo(mockErrorHandler);
			assertThat(container.getPhase()).isEqualTo(1);
			assertThat(container.getPoolName()).isEqualTo(mockPool.getName());
			assertThat(container.getQueryService()).isEqualTo(mockQueryService);
			assertThat(container.getTaskExecutor()).isEqualTo(mockTaskExecutor);
		}
		finally {
			IOUtils.close(applicationContext);
		}
	}

	private void testRegisterAndExecuteContinuousQuery(Class<?>... annotatedClasses) throws Exception {

		ConfigurableApplicationContext applicationContext = newApplicationContext(annotatedClasses);

		try {
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
		finally {
			IOUtils.close(applicationContext);
		}
	}

	@Test
	public void registersAndExecutesContinuousQueries() throws Exception {
		testRegisterAndExecuteContinuousQuery(TestContinuousQueryRegistrationAndExecutionConfiguration.class);
	}

	@Test
	public void registersAndExecutesContinuousQueriesOnProxiedBean() throws Exception {
		testRegisterAndExecuteContinuousQuery(TestContinuousQueryRegistrationAndExecutionOnProxiedBeanConfiguration.class);
	}

	@ClientCacheApplication
	@EnableGemFireMockObjects
	@EnableContinuousQueries(errorHandlerBeanName = "mockErrorHandler", phase = 1, poolName = "mockPool",
		queryServiceBeanName = "mockQueryService", taskExecutorBeanName = "mockTaskExecutor")
	static class TestContinuousQueryListenerContainerConfiguration {

		@Bean
		ErrorHandler mockErrorHandler() {
			return mock(ErrorHandler.class);
		}

		@Bean
		Pool mockPool() {

			Pool mockPool = mock(Pool.class);

			when(mockPool.getName()).thenReturn("mockPool");

			return mockPool;
		}

		@Bean
		QueryService mockQueryService() {
			return mock(QueryService.class);
		}

		@Bean
		Executor mockTaskExecutor() {
			return mock(Executor.class);
		}
	}

	@ClientCacheApplication
	@EnableContinuousQueries
	@EnableGemFireMockObjects
	@SuppressWarnings("unused")
	static class TestContinuousQueryRegistrationAndExecutionConfiguration {

		@Bean
		BeanFactoryPostProcessor dependsOnBeanFactoryPostProcessor() {

			return configurableListableBeanFactory ->
				configurableListableBeanFactory.getBeanDefinition("continuousQueryListenerContainer")
					.setDependsOn("continuousQueryComponent");
		}

		@Bean("DEFAULT")
		Pool mockPool() {
			return mock(Pool.class);
		}

		@Bean
		@Order(Ordered.HIGHEST_PRECEDENCE)
		TestContinuousQueryComponent continuousQueryComponent() {
			return new TestContinuousQueryComponent();
		}
	}

	@Configuration
	@EnableGemfireRepositories(basePackageClasses = PersonRepository.class)
	@Import(TestContinuousQueryRegistrationAndExecutionConfiguration.class)
	@SuppressWarnings("unused")
	static class TestContinuousQueryRegistrationAndExecutionOnProxiedBeanConfiguration {

		@Bean
		BeanPostProcessor proxyTestContinuousQueryComponentPostProcessor() {

			return new BeanPostProcessor() {

				@Override
				public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {

					if ("continuousQueryComponent".equals(beanName)) {
						return new ProxyFactory(bean).getProxy(bean.getClass().getClassLoader());
					}

					return bean;
				}
			};
		}

		@Bean("People")
		Region<Long, Person> mockPeopleRegion(GemFireCache gemfireCache,
				@Qualifier("peopleRegionAttributes") RegionAttributes<Long, Person> peopleRegionAttributes) {

			return GemFireMockObjectsSupport.mockRegion(gemfireCache, "People", peopleRegionAttributes);
		}

		@Bean
		@SuppressWarnings("unchecked")
		RegionAttributesFactoryBean peopleRegionAttributes() {

			RegionAttributesFactoryBean peopleRegionAttributes = new RegionAttributesFactoryBean();

			peopleRegionAttributes.setKeyConstraint(Long.class);
			peopleRegionAttributes.setValueConstraint(Person.class);

			return peopleRegionAttributes;
		}

		@Bean("Examples")
		Region<Long, Example> mockExamplesRegion(GemFireCache gemfireCache,
				@Qualifier("examplesRegionAttributes") RegionAttributes<Long, Example> mockExamplesRegionAttributes) {

			return GemFireMockObjectsSupport.mockRegion(gemfireCache, "Examples",
				mockExamplesRegionAttributes);
		}

		@Bean
		@SuppressWarnings("unchecked")
		RegionAttributesFactoryBean examplesRegionAttributes() {

			RegionAttributesFactoryBean examplesRegionAttributes = new RegionAttributesFactoryBean();

			examplesRegionAttributes.setKeyConstraint(Long.class);
			examplesRegionAttributes.setValueConstraint(Example.class);

			return examplesRegionAttributes;
		}

		@Bean
		ExampleDataAccessObject exampleDao() {

			return (ExampleDataAccessObject) Proxy.newProxyInstance(Thread.currentThread().getContextClassLoader(),
				asArray(ExampleDataAccessObject.class), (proxy, method, args) -> null);
		}

		@Bean
		GemfireRepositoryFactoryBean exampleRepository() {

			GemfireRepositoryFactoryBean<ExampleRepository, Example, Long> exampleRepositoryFactory =
				new GemfireRepositoryFactoryBean<>(ExampleRepository.class);

			exampleRepositoryFactory.setGemfireMappingContext(new GemfireMappingContext());
			exampleRepositoryFactory.setLazyInit(false);

			return exampleRepositoryFactory;
		}

		@Bean
		Object exampleService(ExampleRepository exampleRepository, ExampleDataAccessObject exampleDao) {
			assertThat(exampleRepository).isNotNull();
			assertThat(exampleDao).isNotNull();
			return null;
		}

		@Bean
		Object personService(PersonRepository personRepository) {
			assertThat(personRepository).isNotNull();
			return null;
		}
	}

	@Component
	@SuppressWarnings("unused")
	static class TestContinuousQueryComponent {

		@ContinuousQuery(name = "TestQuery", query = "SELECT * FROM /Example")
		public void handle(CqEvent event) {
		}
	}

	@Data
	@org.springframework.data.gemfire.mapping.annotation.Region("Examples")
	static class Example {
		@Id Long id;
	}

	interface ExampleDataAccessObject {
	}

	interface ExampleRepository extends CrudRepository<Example, Long> {
	}
}
