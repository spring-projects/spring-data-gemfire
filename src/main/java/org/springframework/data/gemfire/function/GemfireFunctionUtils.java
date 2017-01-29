/*
 * Copyright 2002-2018 the original author or authors.
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

import static java.util.Arrays.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;

import java.lang.annotation.Annotation;
import java.lang.reflect.AnnotatedElement;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.execute.FunctionService;
import org.springframework.beans.BeanUtils;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.data.gemfire.function.annotation.GemfireFunction;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;

/**
 * Utility class for registering a POJO as a GemFire/Geode {@link Function}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.execute.Function
 * @see org.apache.geode.cache.execute.FunctionService
 * @see org.springframework.data.gemfire.function.annotation.GemfireFunction
 * @since 1.2.0
 */
public abstract class GemfireFunctionUtils {

	private static Log log = LogFactory.getLog(GemfireFunctionUtils.class);

	/**
	 * Determines whether the given {@link Method} is a POJO, {@link GemfireFunction} annotated {@link Method}.
	 *
	 * @param method {@link Method} to evaluate.
	 * @return a boolean value indicating whether the {@link Method} on a POJO represents a SDG {@link GemfireFunction}.
	 * @see org.springframework.data.gemfire.function.annotation.GemfireFunction
	 * @see java.lang.reflect.Method
	 */
	public static boolean isGemfireFunction(Method method) {
		return (method != null && method.isAnnotationPresent(GemfireFunction.class));
	}

	private static boolean isMatchingGemfireFunction(Method method, String functionId) {
		return getGemfireFunctionId(method).filter(methodFunctionId ->
			ObjectUtils.nullSafeEquals(methodFunctionId, functionId)).isPresent();
	}

	private static AnnotationAttributes getAnnotationAttributes(AnnotatedElement element,
			Class<? extends Annotation> annotationType) {

		return AnnotationAttributes.fromMap(
			AnnotationUtils.getAnnotationAttributes(element, element.getAnnotation(annotationType)));
	}

	/**
	 * Null-safe operation used to determine the GemFire/Geode {@link Function#getId()} Function ID}
	 * of a given {@link GemfireFunction} annotated POJO {@link Method}.
	 *
	 * If the {@link Method} is not {@literal null} and annotated with {@link GemfireFunction} then this method
	 * tries to determine the {@link Function#getId()} from the {@link GemfireFunction#id()} attribute.
	 *
	 * If the {@link GemfireFunction#id()} attribute was not set, then the {@link Method#getName()}
	 * is returned as the {@link Function#getId() Function ID}.
	 *
	 * @param method {@link GemfireFunction} annotated POJO {@link Method} containing the implementation
	 * for a GemFire/Geode {@link Function}.
	 * @return the GemFire/Geode Function ID of the given {@link Method}, or an empty {@link Optional}
	 * if the {@link Method} is not a GemFire/Geode {@link Function}.
	 * @see org.springframework.data.gemfire.function.annotation.GemfireFunction
	 * @see java.lang.reflect.Method
	 * @see #isGemfireFunction(Method)
	 */
	private static Optional<String> getGemfireFunctionId(Method method) {

		return Optional.ofNullable(method)
			.filter(GemfireFunctionUtils::isGemfireFunction)
			.map(it ->
				Optional.of(it.getAnnotation(GemfireFunction.class))
					.map(annotation -> getAnnotationAttributes(it, annotation.getClass()))
					.filter(annotationAttributes -> annotationAttributes.containsKey("id"))
					.map(annotationAttributes -> annotationAttributes.getString("id"))
					.filter(StringUtils::hasText)
					.orElseGet(() -> it.getName())
			);
	}

	private static Object constructInstance(Class<?> type, Object... constructorArguments) {

		return org.springframework.data.util.ReflectionUtils.findConstructor(type, constructorArguments)
			.map(constructor -> BeanUtils.instantiateClass(constructor, constructorArguments))
			.orElseThrow(() -> newIllegalArgumentException(
				"No suitable constructor was found for type [%s] having parameters [%s]", type.getName(),
				stream(nullSafeArray(constructorArguments, Object.class)).map(ObjectUtils::nullSafeClassName)
					.collect(Collectors.toList())));
	}

	private static String nullSafeName(Method method) {

		return Optional.ofNullable(method)
			.map(it -> String.format("%s.%s", method.getDeclaringClass().getName(), method.getName()))
			.orElse(null);
	}

	/**
	 * Bind a {@link Method method} with the given {@link Function#getId() Function ID} on an object
	 * of the given {@link Class type} as a {@link Function} and register it with the {@link FunctionService}.
	 *
	 * @param type {@link Class target type} to evaluate; must not be {@literal null}.
	 * @param functionId {@link String} containing the {@link Function#getId()} identifying the {@link Method}
	 * on the {@link Class target type} to bind as a {@link Function}.
	 * @throws IllegalArgumentException if {@link Class type} is {@literal null}.
	 * @see #registerFunctionForPojoMethod(Object, Method, AnnotationAttributes, boolean)
	 * @see #isMatchingGemfireFunction(Method, String)
	 */
	@SuppressWarnings("unused")
	public static void registerFunctionForPojoMethod(Class<?> type, String functionId) {

		Assert.notNull(type, () -> String.format("Class type of POJO containing %s(s) is required",
			GemfireFunction.class.getName()));

		ReflectionUtils.doWithMethods(type,
			method -> registerFunctionForPojoMethod(constructInstance(type), method,
				getAnnotationAttributes(method, GemfireFunction.class), true),
			method -> isMatchingGemfireFunction(method, functionId));

	}

	/**
	 * Bind a {@link Method method} with the given {@link Function#getId() Function ID} on the given {@link Object target}
	 * as a {@link Function} and register it with the {@link FunctionService}.
	 *
	 * @param target {@link Object target object} to evaluate; must not be {@literal null}.
	 * @param functionId {@link String} containing the {@link Function#getId()} identifying the {@link Method}
	 * on the {@link Object target object} to bind as a {@link Function}.
	 * @throws IllegalArgumentException if {@link Object target} is {@literal null}.
	 * @see #registerFunctionForPojoMethod(Object, Method, AnnotationAttributes, boolean)
	 * @see #isMatchingGemfireFunction(Method, String)
	 */
	public static void registerFunctionForPojoMethod(Object target, String functionId) {

		Assert.notNull(target, "Target object is required");

		ReflectionUtils.doWithMethods(target.getClass(),
			method -> registerFunctionForPojoMethod(target, method,
				getAnnotationAttributes(method, GemfireFunction.class), true),
			method -> isMatchingGemfireFunction(method, functionId));
	}

	/**
	 * Binds the given {@link Method method} on the given {@link Object target} as a {@link Function}
	 * and registers it with the {@link FunctionService}.
	 *
	 * @param target {@link Object target object} to evaluate; must not be {@literal null}.
	 * @param method {@link Method} on {@link Object target} bound as a {@link Function}.
	 * @param overwrite if {@literal true}, will replace any existing {@link Function}
	 * having the same {@link Function#getId() ID}.
	 * @throws IllegalArgumentException if {@link Object target} is {@literal null} or the given {@link Method}
	 * is not a {@link GemfireFunction}.
	 * @see #registerFunctionForPojoMethod(Object, Method, AnnotationAttributes, boolean)
	 * @see #isGemfireFunction(Method)
	 */
	public static void registerFunctionForPojoMethod(Object target, Method method, boolean overwrite) {

		Assert.notNull(target, "Target object is required");

		Assert.isTrue(isGemfireFunction(method), () -> String.format("Method [%s] must be a %s",
			nullSafeName(method), GemfireFunction.class.getName()));

		registerFunctionForPojoMethod(target, method, getAnnotationAttributes(method, GemfireFunction.class), overwrite);

	}

	/**
	 * Wrap the {@link Object target object} and {@link Method method} in a GemFire/Geode {@link Function}
	 * and register the {@link Function} with the {@link FunctionService}.
	 *
	 * @param target {@link Object target object}.
	 * @param method {@link Method} bound to a {@link Function}.
	 * @param gemfireFunctionAttributes {@link GemfireFunction} annotation {@link Map attributes}.
	 * @param overwrite if {@literal true}, will replace any existing {@link Function} having the same ID.
	 * @deprecated use {@link #registerFunctionForPojoMethod(Object, Method, AnnotationAttributes, boolean)} instead.
	 */
	@Deprecated
	public static void registerFunctionForPojoMethod(Object target, Method method,
			Map<String, Object> gemfireFunctionAttributes, boolean overwrite) {

		registerFunctionForPojoMethod(target, method, AnnotationAttributes.fromMap(gemfireFunctionAttributes), overwrite);
	}

	/**
	 * Wrap the {@link Object target object} and {@link Method method} in a GemFire/Geode {@link Function}
	 * and register the {@link Function} with the {@link FunctionService}.
	 *
	 * @param target {@link Object target object}.
	 * @param method {@link Method} bound to a {@link Function}.
	 * @param gemfireFunctionAttributes {@link GemfireFunction} {@link AnnotationAttributes annotation attributes}.
	 * @param overwrite if {@literal true}, will replace any existing {@link Function} having the same ID.
	 */
	public static void registerFunctionForPojoMethod(Object target, Method method,
			AnnotationAttributes gemfireFunctionAttributes, boolean overwrite) {

		String id = gemfireFunctionAttributes.containsKey("id")
			? gemfireFunctionAttributes.getString("id") : "";

		PojoFunctionWrapper function = new PojoFunctionWrapper(target, method, id);

		if (gemfireFunctionAttributes.containsKey("batchSize")) {

			int batchSize = gemfireFunctionAttributes.getNumber("batchSize");

			Assert.isTrue(batchSize >= 0,
				String.format("batchSize [%1$d] specified on [%2$s.%3$s] must be a non-negative value",
					batchSize, target.getClass().getName(), method.getName()));

			function.setBatchSize(batchSize);
		}

		if (gemfireFunctionAttributes.containsKey("HA")) {
			function.setHA(gemfireFunctionAttributes.getBoolean("HA"));
		}

		if (gemfireFunctionAttributes.containsKey("hasResult")) {
			if (Boolean.TRUE.equals(gemfireFunctionAttributes.getBoolean("hasResult"))) {
				function.setHasResult(true);
			}
		}

		if (gemfireFunctionAttributes.containsKey("optimizeForWrite")) {
			function.setOptimizeForWrite(gemfireFunctionAttributes.getBoolean("optimizeForWrite"));
		}

		if (FunctionService.isRegistered(function.getId())) {
			if (overwrite) {

				if (log.isDebugEnabled()) {
					log.debug(String.format("Unregistering Function [%s]", function.getId()));
				}

				FunctionService.unregisterFunction(function.getId());
			}
		}

		if (!FunctionService.isRegistered(function.getId())) {

			FunctionService.registerFunction(function);

			if (log.isDebugEnabled()) {
				log.debug(String.format("Registered Function [%s]", function.getId()));
			}
		}
		else {
			if (log.isDebugEnabled()) {
				log.debug(String.format("Function [%s] is already registered", function.getId()));
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
