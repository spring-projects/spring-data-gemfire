/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.function.config;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;

import org.springframework.beans.PropertyValue;
import org.springframework.beans.factory.config.ConstructorArgumentValues;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;

/**
 * Unit tests for {@link ServerBasedFunctionExecutionBeanDefinitionBuilder}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.function.config.ServerBasedFunctionExecutionBeanDefinitionBuilder
 * @since 1.7.0
 */
public class ServerBasedFunctionExecutionBeanDefinitionBuilderUnitTests {

	@Test
	@SuppressWarnings("unchecked")
	public void getGemfireFunctionOperationsBeanDefinitionBuilder() {

		FunctionExecutionConfiguration mockFunctionExecutionConfiguration =
			mock(FunctionExecutionConfiguration.class, "MockFunctionExecutionConfiguration");

		when(mockFunctionExecutionConfiguration.getAttribute(eq("cache"))).thenReturn(null);
		when(mockFunctionExecutionConfiguration.getAttribute(eq("pool"))).thenReturn("");
		when(mockFunctionExecutionConfiguration.getFunctionExecutionInterface()).thenAnswer(invocation -> Object.class);

		ServerBasedFunctionExecutionBeanDefinitionBuilder builder =
			new ServerBasedFunctionExecutionBeanDefinitionBuilder(mockFunctionExecutionConfiguration) {

				@Override
				protected Class<?> getGemfireFunctionOperationsClass() {
					return Object.class;
				}
			};

		BeanDefinitionBuilder beanDefinitionBuilder =
			builder.getGemfireFunctionOperationsBeanDefinitionBuilder(null);

		assertThat(beanDefinitionBuilder).isNotNull();

		AbstractBeanDefinition beanDefinition = beanDefinitionBuilder.getRawBeanDefinition();

		assertThat(beanDefinition).isNotNull();
		assertThat(beanDefinition.getBeanClass()).isEqualTo(Object.class);

		ConstructorArgumentValues.ValueHolder constructorArgumentValue =
			beanDefinition.getConstructorArgumentValues().getArgumentValue(0, RuntimeBeanReference.class);

		assertThat(constructorArgumentValue).isNotNull();
		assertThat(((RuntimeBeanReference) constructorArgumentValue.getValue()).getBeanName()).isEqualTo("gemfireCache");
		assertThat(beanDefinition.getPropertyValues().getPropertyValue("pool")).isNull();

		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("cache"));
		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("pool"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void getGemfireFunctionOperationsBeanDefinitionBuilderWithCache() {

		FunctionExecutionConfiguration mockFunctionExecutionConfiguration =
			mock(FunctionExecutionConfiguration.class, "MockFunctionExecutionConfiguration");

		when(mockFunctionExecutionConfiguration.getAttribute(eq("cache"))).thenReturn("TestCache");
		when(mockFunctionExecutionConfiguration.getAttribute(eq("pool"))).thenReturn("  ");

		ServerBasedFunctionExecutionBeanDefinitionBuilder builder =
			new ServerBasedFunctionExecutionBeanDefinitionBuilder(mockFunctionExecutionConfiguration) {

				@Override
				protected Class<?> getGemfireFunctionOperationsClass() {
					return Object.class;
				}
			};

		BeanDefinitionBuilder beanDefinitionBuilder =
			builder.getGemfireFunctionOperationsBeanDefinitionBuilder(null);

		assertThat(beanDefinitionBuilder).isNotNull();

		AbstractBeanDefinition beanDefinition = beanDefinitionBuilder.getRawBeanDefinition();

		assertThat(beanDefinition).isNotNull();
		assertThat(beanDefinition.getBeanClass()).isEqualTo(Object.class);

		ConstructorArgumentValues.ValueHolder constructorArgumentValue =
			beanDefinition.getConstructorArgumentValues().getArgumentValue(0, RuntimeBeanReference.class);

		assertThat(constructorArgumentValue).isNotNull();
		assertThat(((RuntimeBeanReference) constructorArgumentValue.getValue()).getBeanName()).isEqualTo("TestCache");
		assertThat(beanDefinition.getPropertyValues().getPropertyValue("pool")).isNull();

		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("cache"));
		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("pool"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void getGemfireFunctionOperationsBeanDefinitionBuilderWithPool() {

		FunctionExecutionConfiguration mockFunctionExecutionConfiguration =
			mock(FunctionExecutionConfiguration.class, "MockFunctionExecutionConfiguration");

		when(mockFunctionExecutionConfiguration.getAttribute(eq("cache"))).thenReturn(null);
		when(mockFunctionExecutionConfiguration.getAttribute(eq("pool"))).thenReturn("TestPool");

		ServerBasedFunctionExecutionBeanDefinitionBuilder builder =
			new ServerBasedFunctionExecutionBeanDefinitionBuilder(mockFunctionExecutionConfiguration) {

				@Override
				protected Class<?> getGemfireFunctionOperationsClass() {
					return Object.class;
				}
			};

		BeanDefinitionBuilder beanDefinitionBuilder =
			builder.getGemfireFunctionOperationsBeanDefinitionBuilder(null);

		assertThat(beanDefinitionBuilder).isNotNull();

		AbstractBeanDefinition beanDefinition = beanDefinitionBuilder.getRawBeanDefinition();

		assertThat(beanDefinition).isNotNull();
		assertThat(beanDefinition.getBeanClass()).isEqualTo(Object.class);

		ConstructorArgumentValues.ValueHolder constructorArgumentValue =
			beanDefinition.getConstructorArgumentValues().getArgumentValue(0, RuntimeBeanReference.class);

		assertThat(constructorArgumentValue).isNotNull();
		assertThat(((RuntimeBeanReference) constructorArgumentValue.getValue()).getBeanName()).isEqualTo("gemfireCache");

		PropertyValue propertyValue = beanDefinition.getPropertyValues().getPropertyValue("pool");

		assertThat(propertyValue).isNotNull();
		assertThat(((RuntimeBeanReference) propertyValue.getValue()).getBeanName()).isEqualTo("TestPool");

		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("cache"));
		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("pool"));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void getGemfireFunctionOperationsBeanDefinitionBuilderWithCacheAndPool() {

		FunctionExecutionConfiguration mockFunctionExecutionConfiguration =
			mock(FunctionExecutionConfiguration.class, "MockFunctionExecutionConfiguration");

		when(mockFunctionExecutionConfiguration.getAttribute(eq("cache"))).thenReturn("TestCache");
		when(mockFunctionExecutionConfiguration.getAttribute(eq("pool"))).thenReturn("TestPool");

		ServerBasedFunctionExecutionBeanDefinitionBuilder builder =
			new ServerBasedFunctionExecutionBeanDefinitionBuilder(mockFunctionExecutionConfiguration) {

				@Override
				protected Class<?> getGemfireFunctionOperationsClass() {
					return Object.class;
				}
			};


		BeanDefinitionBuilder beanDefinitionBuilder =
			builder.getGemfireFunctionOperationsBeanDefinitionBuilder(null);

		assertThat(beanDefinitionBuilder).isNotNull();

		AbstractBeanDefinition beanDefinition = beanDefinitionBuilder.getRawBeanDefinition();

		assertThat(beanDefinition).isNotNull();
		assertThat(beanDefinition.getBeanClass()).isEqualTo(Object.class);

		ConstructorArgumentValues.ValueHolder constructorArgumentValue =
			beanDefinition.getConstructorArgumentValues().getArgumentValue(0, RuntimeBeanReference.class);

		assertThat(constructorArgumentValue).isNotNull();
		assertThat(((RuntimeBeanReference) constructorArgumentValue.getValue()).getBeanName()).isEqualTo("TestCache");

		PropertyValue propertyValue = beanDefinition.getPropertyValues().getPropertyValue("pool");

		assertThat(propertyValue).isNotNull();
		assertThat(((RuntimeBeanReference) propertyValue.getValue()).getBeanName()).isEqualTo("TestPool");

		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("cache"));
		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("pool"));
	}
}
