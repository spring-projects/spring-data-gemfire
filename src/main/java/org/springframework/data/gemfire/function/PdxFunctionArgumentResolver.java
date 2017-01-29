/*
 * Copyright 2010-2018 the original author or authors.
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

package org.springframework.data.gemfire.function;

import java.lang.reflect.Method;

import org.apache.geode.cache.CacheClosedException;
import org.apache.geode.cache.CacheFactory;
import org.apache.geode.cache.execute.FunctionContext;
import org.apache.geode.pdx.PdxInstance;
import org.springframework.util.ClassUtils;

/**
 * The PdxFunctionArgumentResolver class is a Spring Data GemFire FunctionArgumentResolver that automatically resolves
 * PDX types when GemFire is configured with read-serialized set to true, but the application domain classes
 * are actually on the classpath.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.function.DefaultFunctionArgumentResolver
 * @see org.apache.geode.pdx.PdxInstance
 * @since 1.5.2
 */
@SuppressWarnings("unused")
class PdxFunctionArgumentResolver extends DefaultFunctionArgumentResolver {

	/*
	 * (non-Javadoc)
	 *
	 * @see org.apache.geode.cache.execute.FunctionContext
	 */
	@Override
	public Object[] resolveFunctionArguments(final FunctionContext functionContext) {
		Object[] functionArguments = super.resolveFunctionArguments(functionContext);

		if (isPdxSerializerConfigured()) {
			int index = 0;

			for (Object functionArgument : functionArguments) {
				if (functionArgument instanceof PdxInstance) {
					String className = ((PdxInstance) functionArgument).getClassName();

					if (isDeserializationNecessary(className)) {
						functionArguments[index] = ((PdxInstance) functionArgument).getObject();
					}
				}

				index++;
			}
		}

		return functionArguments;
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.reflect.Method
	 */
	@Override
	public Method getFunctionAnnotatedMethod() {
		throw new UnsupportedOperationException("Not Implemented!");
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.apache.geode.cache.Cache#getPdxSerializer()
	 * @see org.apache.geode.cache.CacheFactory#getAnyInstance()
	 */
	boolean isPdxSerializerConfigured() {
		try {
			return (CacheFactory.getAnyInstance().getPdxSerializer() != null);
		}
		catch (CacheClosedException ignore) {
			return false;
		}
	}

	/*
	 * (non-Javadac)
	 *
	 * @see #isOnClasspath(String)
	 * @see #functionAnnotatedMethodHasParameterOfType(String)
	 */
	boolean isDeserializationNecessary(final String className) {
		return (isOnClasspath(className) && functionAnnotatedMethodHasParameterOfType(className));
	}
	/*
	 * (non-Javadoc)
	 *
	 * @see java.lang.Thread#currentThread()
	 * @see java.lang.Thread#getContextClassLoader()
	 * @see org.springframework.util.ClassUtils#isPresent(String, ClassLoader)
	 */
	boolean isOnClasspath(final String className) {
		return ClassUtils.isPresent(className, Thread.currentThread().getContextClassLoader());
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see #getFunctionAnnotatedMethod()
	 * @see java.lang.reflect.Method#getParameterTypes()
	 */
	boolean functionAnnotatedMethodHasParameterOfType(final String className) {
		for (Class<?> parameterType : getFunctionAnnotatedMethod().getParameterTypes()) {
			if (parameterType.getName().equals(className)) {
				return true;
			}
		}

		return false;
	}

}
