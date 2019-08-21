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
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.springframework.data.gemfire.config.annotation.ClusterConfigurationConfiguration.ClusterSchemaObjectInitializer;
import static org.springframework.data.gemfire.config.annotation.ClusterConfigurationConfiguration.SchemaObjectContext;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Optional;

import org.apache.geode.cache.client.ClientCache;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.core.annotation.Order;
import org.springframework.data.gemfire.config.admin.GemfireAdminOperations;
import org.springframework.data.gemfire.config.admin.remote.RestHttpGemfireAdminTemplate;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.http.client.ClientHttpRequestInterceptor;
import org.springframework.http.client.InterceptingClientHttpRequestFactory;
import org.springframework.lang.Nullable;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.client.RestTemplate;

/**
 * Integration Tests for {@link EnableClusterConfiguration} and {@link ClusterConfigurationConfiguration} asserting that
 * all user-defined {@link ClientHttpRequestInterceptor} beans get applied.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.springframework.data.gemfire.config.annotation.ClusterConfigurationConfiguration
 * @see org.springframework.data.gemfire.config.annotation.EnableClusterConfiguration
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects
 * @see org.springframework.http.client.ClientHttpRequestInterceptor
 * @see org.springframework.http.client.InterceptingClientHttpRequestFactory
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.2.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class ClusterConfigurationWithClientHttpRequestInterceptorsIntegrationTests {

	@Autowired
	private ClientCache clientCache;

	@Autowired
	@Qualifier("mockClientHttpRequestInterceptorOne")
	private ClientHttpRequestInterceptor mockClientHttpRequestInterceptorOne;

	@Autowired
	@Qualifier("mockClientHttpRequestInterceptorTwo")
	private ClientHttpRequestInterceptor mockClientHttpRequestInterceptorTwo;

	@Autowired
	private ClusterConfigurationConfiguration configuration;

	@Autowired
	private ClusterSchemaObjectInitializer initializer;

	@Autowired
	private List<ClientHttpRequestInterceptor> clientHttpRequestInterceptors;

	@SuppressWarnings("unchecked")
	private <T> T getFieldValue(Object target, String fieldName) throws NoSuchFieldException {

		Field field = ReflectionUtils.findField(target.getClass(), fieldName);

		return Optional.ofNullable(field)
			.map(it -> {
				ReflectionUtils.makeAccessible(it);
				return field;
			})
			.map(it -> (T) ReflectionUtils.getField(it, target))
			.orElseThrow(() ->
				new NoSuchFieldException(String.format("Field with name [%s] was not found on Object of type [%s]",
					fieldName, target.getClass().getName())));
	}

	@Before
	public void setup() {

		assertThat(this.clientCache).isNotNull();
		assertThat(this.configuration).isNotNull();
		assertThat(this.initializer).isNotNull();
		assertThat(this.mockClientHttpRequestInterceptorOne).isNotNull();
		assertThat(this.mockClientHttpRequestInterceptorTwo).isNotNull();
		assertThat(this.clientHttpRequestInterceptors).isNotNull();
		assertThat(this.clientHttpRequestInterceptors).hasSize(2);
		assertThat(this.clientHttpRequestInterceptors)
			.containsExactly(this.mockClientHttpRequestInterceptorTwo, this.mockClientHttpRequestInterceptorOne);
	}

	@Test
	public void configurationWasAutowiredWithUserDefinedClientHttpRequestInterceptors() {

		assertThat(this.configuration.resolveClientHttpRequestInterceptors())
			.isEqualTo(this.clientHttpRequestInterceptors);
	}

	@Test
	public void clientHttpRequestInterceptorsRegistered() throws Exception {

		SchemaObjectContext schemaObjectContext = this.initializer.getSchemaObjectContext();

		assertThat(schemaObjectContext).isNotNull();
		assertThat(schemaObjectContext.<ClientCache>getGemfireCache()).isSameAs(this.clientCache);
		assertThat(schemaObjectContext.<GemfireAdminOperations>getGemfireAdminOperations())
			.isInstanceOf(RestHttpGemfireAdminTemplate.class);

		RestHttpGemfireAdminTemplate template = schemaObjectContext.getGemfireAdminOperations();

		RestTemplate restTemplate = getFieldValue(template, "restTemplate");

		assertThat(restTemplate).isNotNull();
		assertThat(restTemplate.getInterceptors())
			.containsExactly(this.mockClientHttpRequestInterceptorTwo, this.mockClientHttpRequestInterceptorOne);
		assertThat(restTemplate.getRequestFactory()).isInstanceOf(InterceptingClientHttpRequestFactory.class);
	}

	@ClientCacheApplication
	@EnableGemFireMockObjects
	@EnableClusterConfiguration(useHttp = true)
	static class TestConfiguration {

		@Bean
		BeanPostProcessor clusterSchemaObjectInitializerBeanPostProcessor() {

			return new BeanPostProcessor() {

				@Nullable @Override
				public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {

					if (bean instanceof ClusterSchemaObjectInitializer) {

						ClusterSchemaObjectInitializer initializer = spy((ClusterSchemaObjectInitializer) bean);

						doReturn(false).when(initializer).isAutoStartup();

						bean = initializer;
					}

					return bean;
				}
			};
		}

		@Bean
		@Order(2)
		ClientHttpRequestInterceptor mockClientHttpRequestInterceptorOne() {
			return mock(ClientHttpRequestInterceptor.class);
		}

		@Bean
		@Order(1)
		ClientHttpRequestInterceptor mockClientHttpRequestInterceptorTwo() {
			return mock(ClientHttpRequestInterceptor.class);
		}
	}
}
