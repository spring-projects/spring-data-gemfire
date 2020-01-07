/*
 * Copyright 2002-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function.execution;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.aopalliance.intercept.MethodInvocation;
import org.junit.Before;
import org.junit.Test;

import org.springframework.data.gemfire.function.annotation.FunctionId;

/**
 * Unit tests for {@link GemfireFunctionProxyFactoryBean}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.aopalliance.intercept.MethodInvocation
 * @see org.springframework.data.gemfire.function.execution.GemfireFunctionProxyFactoryBean
 */
public class GemfireFunctionProxyFactoryBeanUnitTests {

	private GemfireFunctionOperations functionOperations;

	@Before
	public void setUp() {
		this.functionOperations = mock(GemfireFunctionOperations.class);
	}

	@Test
	public void invoke() throws Throwable {

		MethodInvocation invocation = new TestMethodInvocation(IFoo.class)
			.withMethodNameAndArgTypes("collections",List.class);

		when(this.functionOperations.executeAndExtract("collections",invocation.getArguments()))
			.thenReturn(Arrays.asList(1, 2, 3));

		GemfireFunctionProxyFactoryBean proxy = new GemfireFunctionProxyFactoryBean(IFoo.class, this.functionOperations);

		Object result = proxy.invoke(invocation);

		assertThat(result).isInstanceOf(List.class);
		assertThat((List) result).hasSize(3);

		verify(this.functionOperations, times(1))
			.executeAndExtract("collections",invocation.getArguments());
	}

	@Test
	public void invokeAndExtractWithAnnotatedFunctionId() throws Throwable {

		MethodInvocation invocation = new TestMethodInvocation(IFoo.class)
			.withMethodNameAndArgTypes("oneArg", String.class);

		when(this.functionOperations.executeAndExtract("oneArg",invocation.getArguments())).thenReturn(1);

		GemfireFunctionProxyFactoryBean proxy = new GemfireFunctionProxyFactoryBean(IFoo.class, this.functionOperations);

		Object result = proxy.invoke(invocation);

		assertThat(result).describedAs(result.getClass().getName()).isInstanceOf(Integer.class);
		assertThat(result).isEqualTo(1);

		verify(this.functionOperations, times(1))
			.executeAndExtract("oneArg", invocation.getArguments());
	}

	@SuppressWarnings("unused")
	private static class TestMethodInvocation implements MethodInvocation {

		private Class<?> type;

		private Class<?>[] argTypes;

		private Object[] arguments;

		private String methodName;

		public TestMethodInvocation(Class<?> type) {
			this.type = type;
		}

		public TestMethodInvocation withArguments(Object ...arguments){

			this.arguments = arguments;

			return this;
		}

		public TestMethodInvocation withMethodNameAndArgTypes(String methodName,Class<?>... argTypes) {

			this.methodName = methodName;
			this.argTypes = argTypes;

			return this;
		}

		/* (non-Javadoc)
		 * @see org.aopalliance.intercept.Invocation#getArguments()
		 */
		@Override
		public Object[] getArguments() {
			return this.arguments;
		}

		/* (non-Javadoc)
		 * @see org.aopalliance.intercept.Joinpoint#proceed()
		 */
		@Override
		public Object proceed() throws Throwable {
			return null;
		}

		/* (non-Javadoc)
		 * @see org.aopalliance.intercept.Joinpoint#getThis()
		 */
		@Override
		public Object getThis() {
			return null;
		}

		/* (non-Javadoc)
		 * @see org.aopalliance.intercept.Joinpoint#getStaticPart()
		 */
		@Override
		public AccessibleObject getStaticPart() {
			return null;
		}

		/* (non-Javadoc)
		 * @see org.aopalliance.intercept.MethodInvocation#getMethod()
		 */
		@Override
		public Method getMethod() {

			try {
				return this.type.getMethod(methodName, argTypes);
			}
			catch (NoSuchMethodException | SecurityException cause) {
				return null;
			}
		}
	}

	@SuppressWarnings("unused")
	public interface IFoo {

		@FunctionId("oneArg")
		Integer oneArg(String key);

		Integer twoArg(String akey, String bkey);

		List<Integer> collections(List<Integer> args);

		Map<String, Integer> getMapWithNoArgs();

	}
}
