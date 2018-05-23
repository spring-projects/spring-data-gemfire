/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.function.config;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.data.gemfire.config.xml.GemfireConstants;

/**
 * The ServerBasedExecutionBeanDefinitionBuilderTest class is test suite of test cases testing the contract
 * and functionality of the ServerBasedExecutionBeanDefinitionBuilder class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.function.config.ServerBasedExecutionBeanDefinitionBuilder
 * @since 1.7.0
 */
public class ServerBasedExecutionBeanDefinitionBuilderTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	@Test
	@SuppressWarnings("unchecked")
	public void getGemfireFunctionOperationsBeanDefinitionBuilder() {

		FunctionExecutionConfiguration mockFunctionExecutionConfiguration =
			mock(FunctionExecutionConfiguration.class, "MockFunctionExecutionConfiguration");

		when(mockFunctionExecutionConfiguration.getAttribute(eq("cache"))).thenReturn(null);
		when(mockFunctionExecutionConfiguration.getAttribute(eq("pool"))).thenReturn(" ");
		when(mockFunctionExecutionConfiguration.getFunctionExecutionInterface()).thenAnswer(invocation -> Object.class);

		ServerBasedExecutionBeanDefinitionBuilder builder =
			new ServerBasedExecutionBeanDefinitionBuilder(mockFunctionExecutionConfiguration) {

				@Override
				protected Class<?> getGemfireFunctionOperationsClass() {
					return Object.class;
				}
			};

		BeanDefinitionBuilder beanDefinitionBuilder = builder.getGemfireFunctionOperationsBeanDefinitionBuilder(null);

		assertThat(beanDefinitionBuilder, is(notNullValue()));

		AbstractBeanDefinition beanDefinition = beanDefinitionBuilder.getRawBeanDefinition();

		assertThat(beanDefinition, is(notNullValue()));
		assertThat(beanDefinition.getBeanClass(), is(equalTo(Object.class)));
		assertThat(String.valueOf(beanDefinition.getConstructorArgumentValues()
			.getArgumentValue(0, RuntimeBeanReference.class).getValue()),
				containsString(GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME));

		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("cache"));
		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("pool"));
		verify(mockFunctionExecutionConfiguration, times(1)).getFunctionExecutionInterface();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void getGemfireFunctionOperationsBeanDefinitionBuilderWithCache() {

		FunctionExecutionConfiguration mockFunctionExecutionConfiguration =
			mock(FunctionExecutionConfiguration.class, "MockFunctionExecutionConfiguration");

		when(mockFunctionExecutionConfiguration.getAttribute(eq("cache"))).thenReturn("TestCache");
		when(mockFunctionExecutionConfiguration.getAttribute(eq("pool"))).thenReturn(" ");
		when(mockFunctionExecutionConfiguration.getFunctionExecutionInterface()).thenAnswer(invocation -> Object.class);

		ServerBasedExecutionBeanDefinitionBuilder builder =
			new ServerBasedExecutionBeanDefinitionBuilder(mockFunctionExecutionConfiguration) {

				@Override
				protected Class<?> getGemfireFunctionOperationsClass() {
					return Object.class;
				}
			};

		BeanDefinitionBuilder beanDefinitionBuilder = builder.getGemfireFunctionOperationsBeanDefinitionBuilder(null);

		assertThat(beanDefinitionBuilder, is(notNullValue()));

		AbstractBeanDefinition beanDefinition = beanDefinitionBuilder.getRawBeanDefinition();

		assertThat(beanDefinition, is(notNullValue()));
		assertThat(beanDefinition.getBeanClass(), is(equalTo(Object.class)));
		assertThat(String.valueOf(beanDefinition.getConstructorArgumentValues()
			.getArgumentValue(0, RuntimeBeanReference.class).getValue()), containsString("TestCache"));

		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("cache"));
		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("pool"));
		verify(mockFunctionExecutionConfiguration, times(1)).getFunctionExecutionInterface();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void getGemfireFunctionOperationsBeanDefinitionBuilderWithPool() {

		FunctionExecutionConfiguration mockFunctionExecutionConfiguration =
			mock(FunctionExecutionConfiguration.class, "MockFunctionExecutionConfiguration");

		when(mockFunctionExecutionConfiguration.getAttribute(eq("cache"))).thenReturn(null);
		when(mockFunctionExecutionConfiguration.getAttribute(eq("pool"))).thenReturn("TestPool");
		when(mockFunctionExecutionConfiguration.getFunctionExecutionInterface()).thenAnswer(invocation -> Object.class);

		ServerBasedExecutionBeanDefinitionBuilder builder =
			new ServerBasedExecutionBeanDefinitionBuilder(mockFunctionExecutionConfiguration) {

				@Override
				protected Class<?> getGemfireFunctionOperationsClass() {
					return Object.class;
				}
			};

		BeanDefinitionBuilder beanDefinitionBuilder = builder.getGemfireFunctionOperationsBeanDefinitionBuilder(null);

		assertThat(beanDefinitionBuilder, is(notNullValue()));

		AbstractBeanDefinition beanDefinition = beanDefinitionBuilder.getRawBeanDefinition();

		assertThat(beanDefinition, is(notNullValue()));
		assertThat(beanDefinition.getBeanClass(), is(equalTo(Object.class)));
		assertThat(String.valueOf(beanDefinition.getConstructorArgumentValues()
			.getArgumentValue(0, RuntimeBeanReference.class).getValue()), containsString("TestPool"));

		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("cache"));
		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("pool"));
		verify(mockFunctionExecutionConfiguration, times(1)).getFunctionExecutionInterface();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void getGemfireFunctionOperationsBeanDefinitionBuilderWithCacheAndPool() {

		FunctionExecutionConfiguration mockFunctionExecutionConfiguration =
			mock(FunctionExecutionConfiguration.class, "MockFunctionExecutionConfiguration");

		when(mockFunctionExecutionConfiguration.getAttribute(eq("cache"))).thenReturn("TestCache");
		when(mockFunctionExecutionConfiguration.getAttribute(eq("pool"))).thenReturn("TestPool");
		when(mockFunctionExecutionConfiguration.getFunctionExecutionInterface()).thenAnswer(invocation -> Object.class);

		ServerBasedExecutionBeanDefinitionBuilder builder =
			new ServerBasedExecutionBeanDefinitionBuilder(mockFunctionExecutionConfiguration) {

				@Override
				protected Class<?> getGemfireFunctionOperationsClass() {
					return Object.class;
				}
			};

		expectedException.expect(IllegalStateException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage(is(equalTo("Invalid configuration for interface [java.lang.Object];"
			+ " cannot specify both 'pool' and 'cache'")));

		builder.getGemfireFunctionOperationsBeanDefinitionBuilder(null);

		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("cache"));
		verify(mockFunctionExecutionConfiguration, times(1)).getAttribute(eq("pool"));
		verify(mockFunctionExecutionConfiguration, times(1)).getFunctionExecutionInterface();
	}
}
