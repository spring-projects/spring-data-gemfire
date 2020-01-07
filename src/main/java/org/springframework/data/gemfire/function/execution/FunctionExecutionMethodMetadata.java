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

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import org.apache.geode.cache.execute.Execution;
import org.apache.geode.cache.execute.Function;

import org.springframework.data.gemfire.function.annotation.FunctionId;
import org.springframework.util.ReflectionUtils;

/**
 * Base class for method-level metadata for a {@link Function} {@link Execution} interface.
 *
 * This is used at runtime by the {@link Function} {@link Execution} proxy to create
 * the corresponding {@link Function} {@link Execution}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.function.execution.MethodMetadata
 */
abstract class FunctionExecutionMethodMetadata<T extends MethodMetadata> {

	protected final Map<Method, T> methodMetadata = new HashMap<>();

	public FunctionExecutionMethodMetadata(Class<?> serviceInterface) {

		ReflectionUtils.doWithMethods(serviceInterface, method -> {

			T methodMetadata = newMetadataInstance(method);

			if (methodMetadata.getFunctionId() == null) {
				methodMetadata.setFunctionId(method.getName());
			}

			this.methodMetadata.put(method, methodMetadata);
		});
	}

	protected abstract T newMetadataInstance(Method method);

	T getMethodMetadata(Method method) {
		return this.methodMetadata.get(method);
	}
}

class MethodMetadata {

	private String functionId;

	public MethodMetadata(Method method) {

		FunctionId functionIdAnnotation = method.getAnnotation(FunctionId.class);

		if (functionIdAnnotation != null) {
			this.functionId = functionIdAnnotation.value();
		}
	}

	public void setFunctionId(String functionId) {
		this.functionId = functionId;
	}

	public String getFunctionId() {
		return this.functionId;
	}
}
