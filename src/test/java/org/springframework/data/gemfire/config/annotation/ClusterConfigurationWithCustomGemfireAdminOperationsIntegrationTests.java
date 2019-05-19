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
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.springframework.data.gemfire.config.annotation.ClusterConfigurationConfiguration.ClusterSchemaObjectInitializer;
import static org.springframework.data.gemfire.config.annotation.ClusterConfigurationConfiguration.SchemaObjectContext;

import org.apache.geode.cache.client.ClientCache;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.config.admin.GemfireAdminOperations;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.lang.Nullable;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration Tests for {@link EnableClusterConfiguration} and {@link ClusterConfigurationConfiguration} asserting that
 * SDG support custom registered, user-defined {@link GemfireAdminOperations}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.springframework.data.gemfire.config.admin.GemfireAdminOperations
 * @see org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 2.2.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class ClusterConfigurationWithCustomGemfireAdminOperationsIntegrationTests {

	@Autowired
	private ClientCache clientCache;

	@Autowired
	private ClusterConfigurationConfiguration configuration;

	@Autowired
	private ClusterSchemaObjectInitializer initializer;

	@Autowired
	private GemfireAdminOperations gemfireAdminOperations;

	@Before
	public void setup() {

		assertThat(this.clientCache).isNotNull();
		assertThat(this.configuration).isNotNull();
		assertThat(this.gemfireAdminOperations).isNotNull();
		assertThat(this.initializer).isNotNull();
		assertThat(this.configuration.resolveGemfireAdminOperations(null, this.clientCache))
			.isSameAs(this.gemfireAdminOperations);
	}

	@Test
	public void customGemfireAdminOperationsRegistered() {

		SchemaObjectContext schemaObjectContext = this.initializer.getSchemaObjectContext();

		assertThat(schemaObjectContext).isNotNull();
		assertThat(schemaObjectContext.<GemfireAdminOperations>getGemfireAdminOperations())
			.isSameAs(this.gemfireAdminOperations);
	}

	@ClientCacheApplication
	@EnableGemFireMockObjects
	@EnableClusterConfiguration
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
		GemfireAdminOperations mockGemfireAdminOperations() {
			return mock(GemfireAdminOperations.class);
		}
	}
}
