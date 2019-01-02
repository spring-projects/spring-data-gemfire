/*
 * Copyright 2002-2019 the original author or authors.
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

package org.springframework.data.gemfire.function;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.execute.FunctionService;

/**
 * @author David Turanski
 */
public abstract class GemfireFunctionUtils {

	private static Log log = LogFactory.getLog(GemfireFunctionUtils.class);

	/**
	 * Wrap a target object and method in a GemFire Function and register the function to the {@link FunctionService}
	 *
	 * @param target the target object
	 * @param method the method bound to the function
	 * @param attributes function attributes
	 * @param overwrite if true, will replace the existing function
	 */
	public static void registerFunctionForPojoMethod(Object target, Method method, Map<String, Object> attributes,
		boolean overwrite) {

		String id = attributes.containsKey("id") ? (String) attributes.get("id") : "";

		PojoFunctionWrapper function = new PojoFunctionWrapper(target, method, id);

		if (attributes.containsKey("HA")) {
			function.setHA((Boolean) attributes.get("HA"));
		}

		if (attributes.containsKey("optimizeForWrite")) {
			function.setOptimizeForWrite((Boolean) attributes.get("optimizeForWrite"));
		}

		if (attributes.containsKey("batchSize")) {
			int batchSize = (Integer) attributes.get("batchSize");
			Assert.isTrue(batchSize >= 0, String.format("batchSize must be a non-negative value %1$s.%2$s",
				target.getClass().getName(), method.getName()));
			function.setBatchSize(batchSize);
		}

		if (attributes.containsKey("hasResult")) {
			// only set if true  TODO figure out why???
			if (Boolean.TRUE.equals(attributes.get("hasResult"))) {
				function.setHasResult(true);
			}
		}

		if (FunctionService.isRegistered(function.getId())) {
			if (overwrite) {
				if (log.isDebugEnabled()) {
					log.debug("unregistering function definition " + function.getId());
				}
				FunctionService.unregisterFunction(function.getId());
			}
		}

		if (!FunctionService.isRegistered(function.getId())) {
			FunctionService.registerFunction(function);
			if (log.isDebugEnabled()) {
				log.debug("registered function " + function.getId());
			}
		}
		else {
			if (log.isDebugEnabled()) {
				log.debug("function " + function.getId() + "is already registered");
			}
		}
	}

	/**
	 * Determine the order position of a an annotated method parameter
	 *
	 * @param method the {@link Method} instance
	 * @param targetAnnotationType the annotation
	 * @param requiredTypes an array of valid parameter types for the annotation
	 * @return the parameter position or -1 if the annotated parameter is not found
	 */
	public static int getAnnotationParameterPosition(Method method, Class<?> targetAnnotationType,
			Class<?>[] requiredTypes) {

		int position = -1;

		Annotation[][] parameterAnnotations = method.getParameterAnnotations();

		if (parameterAnnotations.length > 0) {
			Class<?>[] parameterTypes = method.getParameterTypes();

			List<Class<?>> requiredTypesList = Arrays.asList(requiredTypes);

			for (int index = 0; index < parameterAnnotations.length; index++) {
				Annotation[] annotations = parameterAnnotations[index];

				if (annotations.length > 0) {
					for (Annotation annotation : annotations) {
						if (annotation.annotationType().equals(targetAnnotationType)) {
							Assert.state(position < 0, String.format(
								"Method %s signature cannot contain more than one parameter annotated with type %s",
									method.getName(), targetAnnotationType.getName()));

							boolean isRequiredType = false;

							for (Class<?> requiredType : requiredTypesList) {
								if (requiredType.isAssignableFrom(parameterTypes[index])) {
									isRequiredType = true;
									break;
								}
							}

							Assert.isTrue(isRequiredType, String.format(
								"Parameter of type %s annotated with %s must be assignable from one of type %s in method %s",
									parameterTypes[index], targetAnnotationType.getName(),
										StringUtils.arrayToCommaDelimitedString(requiredTypes), method.getName()));

							position = index;
						}
					}
				}
			}
		}

		return position;
	}

}
