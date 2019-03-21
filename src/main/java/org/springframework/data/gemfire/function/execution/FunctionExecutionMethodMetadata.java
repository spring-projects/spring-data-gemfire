/*
 * Copyright 2002-2019 the original author or authors.
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

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import org.springframework.data.gemfire.function.annotation.FunctionId;
import org.springframework.util.Assert;
import org.springframework.util.ReflectionUtils;

/**
 * Base class for method level metadata for a function execution interface. This is used at runtime by the 
 * function execution proxy to create the corresponding Gemfire function {@link Execution}
 * 
 * @author David Turanski
 *
 */
abstract class FunctionExecutionMethodMetadata<T extends MethodMetadata > {

	protected final Map<Method, T> methodMetadata = new HashMap<Method, T>();
	private final boolean singletonInterface;

	public FunctionExecutionMethodMetadata(final Class<?> serviceInterface) {
		
		this.singletonInterface = serviceInterface.getMethods().length == 1;

		ReflectionUtils.doWithMethods(serviceInterface, new ReflectionUtils.MethodCallback() {
			@Override
			public void doWith(Method method) throws IllegalArgumentException, IllegalAccessException {
				T mmd = newMetadataInstance(method);
				if (mmd.getFunctionId() == null) {
					mmd.setFunctionId(method.getName());
				}
			 
				methodMetadata.put(method, mmd);
			}
		});
	}
	
	protected abstract T newMetadataInstance(Method method);
	
	T getMethodMetadata(Method method) {
		return methodMetadata.get(method);
	}
	
	boolean isSingletonInterface() {
		return this.singletonInterface;
	}
	
	T getSingletonMethodMetada() {
		Assert.isTrue(isSingletonInterface(),"this is not a singleton interface.");
		return methodMetadata.values().iterator().next();
	}
}


class MethodMetadata {

	private String functionId;

	public MethodMetadata(Method method) {
		String annotatedFunctionId = annotatedFunctionId(method);
		this.functionId = (annotatedFunctionId == null) ? null : annotatedFunctionId;
	}

	/**
	 * @return the functionId
	 */
	public String getFunctionId() {
		return functionId;
	}

	public void setFunctionId(String functionId) {
		this.functionId = functionId;
	}

	private String annotatedFunctionId(Method method) {
		FunctionId functionIdAnnotation = method.getAnnotation(FunctionId.class);
		return (functionIdAnnotation == null) ? null : functionIdAnnotation.value();
	}

}
