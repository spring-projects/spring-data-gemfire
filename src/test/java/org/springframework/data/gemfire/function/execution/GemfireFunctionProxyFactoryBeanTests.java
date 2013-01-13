/*
 * Copyright 2002-2013 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function.execution;

/**
 * @author David Turanski
 *
 */


import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.aopalliance.intercept.MethodInvocation;
import org.junit.Before;
import org.junit.Test;
import org.springframework.data.gemfire.function.config.FunctionId;
 
/**
 * 
 * @author David Turanski
 *
 */

public class GemfireFunctionProxyFactoryBeanTests {
	
	private GemfireFunctionOperations functionOperations;

	@Before
	public void setUp() {
		functionOperations = mock(GemfireFunctionOperations.class);
	}

	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Test
	public void testInvokeAndExtractWithAnnotatedFunctionId() throws Throwable {
		
	
		GemfireFunctionProxyFactoryBean proxy = new GemfireFunctionProxyFactoryBean(IFoo.class,functionOperations); 	
		
		MethodInvocation invocation = new TestInvocation(IFoo.class).withMethodNameAndArgTypes("oneArg",String.class);
	 
		List results = Arrays.asList(new Integer[]{1});
		
		when(functionOperations.execute("oneArg",invocation.getArguments())).thenReturn(results);
		 
		Object result = proxy.invoke(invocation);
		assertTrue(result instanceof Integer);
		assertEquals(new Integer(1),result);		
	}
	
	 
	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Test
	public void testInvoke() throws Throwable {
		
		
		GemfireFunctionProxyFactoryBean proxy = new GemfireFunctionProxyFactoryBean(IFoo.class, functionOperations); 	
	
		MethodInvocation invocation = new TestInvocation(IFoo.class).withMethodNameAndArgTypes("collections",List.class);
	 
		List results = Arrays.asList(new Integer[]{1,2,3});
		
		when(functionOperations.execute("collections",invocation.getArguments())).thenReturn(results);
		 
		Object result = proxy.invoke(invocation);
		assertTrue(result instanceof List);
		assertEquals(3,((List<?>)result).size());		
	}
	
	
	
	static class TestInvocation implements MethodInvocation {
		
		private Class<?>[] argTypes;
		private Class<?> clazz;
		private String methodName;
		private Object[] arguments;

		public TestInvocation(Class<?> clazz) {
			this.clazz = clazz;
		}
		
		public TestInvocation withArguments(Object ...arguments){
			this.arguments = arguments;
			return this;
		}
		
		 
		
		public TestInvocation withMethodNameAndArgTypes(String methodName,Class<?>... argTypes) {
			this.methodName = methodName;
			this.argTypes = argTypes;
			return this;
		}

		/* (non-Javadoc)
		 * @see org.aopalliance.intercept.Invocation#getArguments()
		 */
		@Override
		public Object[] getArguments() {
			// TODO Auto-generated method stub
			return this.arguments;
		}

		/* (non-Javadoc)
		 * @see org.aopalliance.intercept.Joinpoint#proceed()
		 */
		@Override
		public Object proceed() throws Throwable {
			// TODO Auto-generated method stub
			return null;
		}

		/* (non-Javadoc)
		 * @see org.aopalliance.intercept.Joinpoint#getThis()
		 */
		@Override
		public Object getThis() {
			// TODO Auto-generated method stub
			return null;
		}

		/* (non-Javadoc)
		 * @see org.aopalliance.intercept.Joinpoint#getStaticPart()
		 */
		@Override
		public AccessibleObject getStaticPart() {
			// TODO Auto-generated method stub
			return null;
		}

		/* (non-Javadoc)
		 * @see org.aopalliance.intercept.MethodInvocation#getMethod()
		 */
		@Override
		public Method getMethod() {
			Method method = null;
			try {
				method =  clazz.getMethod(methodName, argTypes);
			} catch (SecurityException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (NoSuchMethodException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			return method;
		}
		
	}
	
	public interface IFoo {

		@FunctionId("oneArg")
		public abstract Integer oneArg(String key);

		public abstract Integer twoArg(String akey, String bkey);

		public abstract List<Integer> collections(List<Integer> args);

		public abstract Map<String, Integer> getMapWithNoArgs();

	}
	
	
}


