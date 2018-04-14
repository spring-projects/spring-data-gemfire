/*
 * Copyright 2012-2018 the original author or authors.
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
import java.util.function.Function;
import java.util.function.Supplier;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.util.StringUtils;

/**
 * SpringUtils is a utility class encapsulating common functionality on objects and other class types.
 *
 * @author John Blum
 * @since 1.8.0
 */
@SuppressWarnings("unused")
public abstract class SpringUtils {

	public static BeanDefinition addDependsOn(BeanDefinition bean, String... beanNames) {

		List<String> dependsOnList = new ArrayList<>();

		Collections.addAll(dependsOnList, nullSafeArray(bean.getDependsOn(), String.class));
		dependsOnList.addAll(Arrays.asList(nullSafeArray(beanNames, String.class)));
		bean.setDependsOn(dependsOnList.toArray(new String[dependsOnList.size()]));

		return bean;
	}

	public static BeanDefinition setPropertyReference(BeanDefinition beanDefinition,
			String propertyName, String beanName) {

		beanDefinition.getPropertyValues().addPropertyValue(propertyName, new RuntimeBeanReference(beanName));

		return beanDefinition;
	}

	public static BeanDefinition setPropertyValue(BeanDefinition beanDefinition,
			String propertyName, Object propertyValue) {

		beanDefinition.getPropertyValues().addPropertyValue(propertyName, propertyValue);

		return beanDefinition;
	}

	public static String defaultIfEmpty(String value, String defaultValue) {
		return defaultIfEmpty(value, () -> defaultValue);
	}

	public static String defaultIfEmpty(String value, Supplier<String> supplier) {
		return StringUtils.hasText(value) ? value : supplier.get();
	}

	public static <T> T defaultIfNull(T value, T defaultValue) {
		return defaultIfNull(value, () -> defaultValue);
	}

	public static <T> T defaultIfNull(T value, Supplier<T> supplier) {
		return value != null ? value : supplier.get();
	}

	public static String dereferenceBean(String beanName) {
		return String.format("%1$s%2$s", BeanFactory.FACTORY_BEAN_PREFIX, beanName);
	}

	public static boolean equalsIgnoreNull(Object obj1, Object obj2) {
		return obj1 == null ? obj2 == null : obj1.equals(obj2);
	}

	public static boolean nullOrEquals(Object obj1, Object obj2) {
		return obj1 == null || obj1.equals(obj2);
	}

	public static boolean nullSafeEquals(Object obj1, Object obj2) {
		return obj1 != null && obj1.equals(obj2);
	}

	public static String nullSafeName(Class<?> type) {
		return type != null ? type.getName() : null;
	}

	public static String nullSafeSimpleName(Class<?> type) {
		return type != null ? type.getSimpleName() : null;
	}

	public static <T> T safeGetValue(Supplier<T> valueSupplier) {
		return safeGetValue(valueSupplier, (T) null);
	}

	public static <T> T safeGetValue(Supplier<T> valueSupplier, T defaultValue) {
		return safeGetValue(valueSupplier, (Supplier<T>) () -> defaultValue);
	}

	public static <T> T safeGetValue(Supplier<T> valueSupplier, Supplier<T> defaultValueSupplier) {
		return safeGetValue(valueSupplier, (Function<Throwable, T>) exception -> defaultValueSupplier.get());
	}

	public static <T> T safeGetValue(Supplier<T> valueSupplier, Function<Throwable, T> exceptionHandler) {

		try {
			return valueSupplier.get();
		}
		catch (Throwable cause) {
			return exceptionHandler.apply(cause);
		}
	}
}
