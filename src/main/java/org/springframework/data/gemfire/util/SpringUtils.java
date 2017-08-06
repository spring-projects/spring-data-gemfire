/*
 * Copyright 2012 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.util;

import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Supplier;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.util.StringUtils;

/**
 * SpringUtils is a utility class encapsulating common functionality on objects and other class types.
 *
 * @author John Blum
 * @since 1.8.0
 */
@SuppressWarnings("unused")
// TODO rename this utility class using a more descriptive, intuitive and meaningful name
public abstract class SpringUtils {

	/* (non-Javadoc) */
	public static BeanDefinition addDependsOn(BeanDefinition bean, String... beanNames) {

		List<String> dependsOnList = new ArrayList<>();

		Collections.addAll(dependsOnList, nullSafeArray(bean.getDependsOn(), String.class));
		dependsOnList.addAll(Arrays.asList(nullSafeArray(beanNames, String.class)));
		bean.setDependsOn(dependsOnList.toArray(new String[dependsOnList.size()]));

		return bean;
	}

	/* (non-Javadoc) */
	public static String defaultIfEmpty(String value, String defaultValue) {
		return (StringUtils.hasText(value) ? value : defaultValue);
	}

	/* (non-Javadoc) */
	public static <T> T defaultIfNull(T value, T defaultValue) {
		return Optional.ofNullable(value).orElse(defaultValue);
	}

	/* (non-Javadoc) */
	public static <T> T defaultIfNull(T value, Supplier<T> supplier) {
		return Optional.ofNullable(value).orElseGet(supplier);
	}

	/* (non-Javadoc) */
	public static String dereferenceBean(String beanName) {
		return String.format("%1$s%2$s", BeanFactory.FACTORY_BEAN_PREFIX, beanName);
	}

	/* (non-Javadoc) */
	public static boolean equalsIgnoreNull(Object obj1, Object obj2) {
		return (obj1 == null ? obj2 == null : obj1.equals(obj2));
	}

	/* (non-Javadoc) */
	public static boolean nullOrEquals(Object obj1, Object obj2) {
		return (obj1 == null || obj1.equals(obj2));
	}

	/* (non-Javadoc) */
	public static boolean nullSafeEquals(Object obj1, Object obj2) {
		return (obj1 != null && obj1.equals(obj2));
	}

	/* (non-Javadoc) */
	public static <T> T safeGetValue(Supplier<T> valueSupplier) {
		return safeGetValue(valueSupplier, (T) null);
	}

	/* (non-Javadoc) */
	public static <T> T safeGetValue(Supplier<T> valueSupplier, T defaultValue) {
		return safeGetValue(valueSupplier, (Supplier<T>) () -> defaultValue);
	}

	/* (non-Javadoc) */
	public static <T> T safeGetValue(Supplier<T> valueSupplier, Supplier<T> defaultValueSupplier) {
		return safeGetValue(valueSupplier, (Function<Throwable, T>) exception -> defaultValueSupplier.get());
	}

	/* (non-Javadoc) */
	public static <T> T safeGetValue(Supplier<T> valueSupplier, Function<Throwable, T> exceptionHandler) {
		try {
			return valueSupplier.get();
		}
		catch (Throwable cause) {
			return exceptionHandler.apply(cause);
		}
	}
}
