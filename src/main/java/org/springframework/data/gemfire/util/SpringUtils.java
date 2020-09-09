/*
 * Copyright 2012-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.beans.PropertyValue;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.annotation.AnnotatedBeanDefinition;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.AnnotationAwareOrderComparator;
import org.springframework.core.annotation.Order;
import org.springframework.core.annotation.OrderUtils;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Abstract utility class encapsulating functionality common to {@link Object Objects}, {@link Class Class types}
 * and Spring beans.
 *
 * @author John Blum
 * @see java.lang.Class
 * @see java.lang.Object
 * @see java.util.function.Function
 * @see java.util.stream.Stream
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.config.RuntimeBeanReference
 * @see org.springframework.core.Ordered
 * @see org.springframework.core.annotation.AnnotationAwareOrderComparator
 * @see org.springframework.core.annotation.Order
 * @since 1.8.0
 */
@SuppressWarnings("unused")
public abstract class SpringUtils {

	/**
	 * Determines whether a given bean registered in the {@link BeanFactory Spring container} matches by
	 * both {@link String name} and {@link Class type}.
	 *
	 * @param beanFactory {@link BeanFactory Spring container} in which to resolve the bean.
	 * @param beanName {@link String name} of the bean.
	 * @param beanType {@link Class type} of the bean.
	 * @return a boolean value indicating whether the {@link BeanFactory Spring container} contains a bean
	 * matching by both {@link String name} and {@link Class type}.
	 * @see org.springframework.beans.factory.BeanFactory
	 * @see java.lang.Class
	 * @see java.lang.String
	 */
	public static boolean isMatchingBean(@NonNull BeanFactory beanFactory, String beanName, Class<?> beanType) {
		return beanFactory.containsBean(beanName) && beanFactory.isTypeMatch(beanName, beanType);
	}

	/**
	 * Adds an array of bean dependencies (by name) to the given {@link BeanDefinition}.
	 *
	 * @param beanDefinition {@link BeanDefinition} to add the bean dependencies to.
	 * @param beanNames {@link String} array containing names of beans to which the {@link BeanDefinition}
	 * has a dependency.
	 * @return the given {@link BeanDefinition}.
	 * @see org.springframework.beans.factory.config.BeanDefinition
	 */
	@NonNull
	public static BeanDefinition addDependsOn(@NonNull BeanDefinition beanDefinition, @Nullable String... beanNames) {

		List<String> dependsOnList = new ArrayList<>();

		Collections.addAll(dependsOnList, ArrayUtils.nullSafeArray(beanDefinition.getDependsOn(), String.class));
		dependsOnList.addAll(Arrays.asList(nullSafeArray(beanNames, String.class)));
		beanDefinition.setDependsOn(dependsOnList.toArray(new String[0]));

		return beanDefinition;
	}

	/**
	 * Returns a {@link List} of beans by the given {@link Class type} in order.
	 *
	 * @param <T> {@link Class type} of the bean.
	 * @param beanFactory {@link ConfigurableListableBeanFactory Spring container} used to acquire the ordered beans.
	 * @param beanType {@link Class type} of beans to acquire.
	 * @return a {@link List} of beans of the given {@link Class type} in order.
	 * @see #getBeansOfTypeOrdered(ConfigurableListableBeanFactory, Class, boolean, boolean)
	 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory
	 * @see java.lang.Class
	 * @see java.util.List
	 */
	@NonNull
	public static <T> List<T> getBeansOfTypeOrdered(@NonNull ConfigurableListableBeanFactory beanFactory,
			@NonNull Class<T> beanType) {

		return getBeansOfTypeOrdered(beanFactory, beanType, true, true);
	}

	/**
	 * Returns a {@link List} of beans by the given {@link Class type} in order.
	 *
	 * @param <T> {@link Class type} of the bean.
	 * @param beanFactory {@link ConfigurableListableBeanFactory Spring container} used to acquire the ordered beans.
	 * @param beanType {@link Class type} of beans to acquire.
	 * @param includeNonSingletons boolean indicating whether to include non-Singleton beans from the Spring container.
	 * @param allowEagerInit boolean indicating whether to eagerly initialize {@link FactoryBean FactoryBeans}.
	 * @return a {@link List} of beans of the given {@link Class type} in order.
	 * @see org.springframework.beans.factory.config.ConfigurableListableBeanFactory
	 * @see java.lang.Class
	 * @see java.util.List
	 */
	@NonNull
	public static <T> List<T> getBeansOfTypeOrdered(@NonNull ConfigurableListableBeanFactory beanFactory,
			@NonNull Class<T> beanType, boolean includeNonSingletons, boolean allowEagerInit) {

		Assert.notNull(beanFactory, "BeanFactory must not be null");
		Assert.notNull(beanType, "Bean type must not be null");

		Map<String, T> beansOfType =
			CollectionUtils.nullSafeMap(beanFactory.getBeansOfType(beanType, includeNonSingletons, allowEagerInit));

		Set<String> beanNamesOfType = new HashSet<>(beansOfType.keySet());

		// Handles @Order annotated beans and beans implementing the Ordered interface
		List<OrderedBeanWrapper<T>> orderedBeansOfType = beansOfType.entrySet().stream()
				.map(SpringUtils::toOrderedBeanWrapper)
				.filter(Objects::nonNull)
				.collect(Collectors.toList());

		Set<String> orderedBeanNamesOfType = orderedBeansOfType.stream()
			.map(OrderedBeanWrapper::getBeanName)
			.collect(Collectors.toSet());

		Set<String> unorderedBeanNamesOfType = new HashSet<>(beanNamesOfType);

		// Set Difference
		unorderedBeanNamesOfType.removeAll(orderedBeanNamesOfType);

		orderedBeansOfType.addAll(orderUnorderedBeans(beanFactory, beansOfType, unorderedBeanNamesOfType));
		orderedBeansOfType.sort(AnnotationAwareOrderComparator.INSTANCE);

		return orderedBeansOfType.stream()
			.map(OrderedBeanWrapper::getBean)
			.collect(Collectors.toList());
	}

	@NonNull
	private static <T> List<OrderedBeanWrapper<T>> orderUnorderedBeans(@NonNull ConfigurableListableBeanFactory beanFactory,
			@NonNull Map<String, T> beansOfType, @NonNull Set<String> unorderedBeanNames) {

		List<OrderedBeanWrapper<T>> orderedBeanWrappers = new ArrayList<>(unorderedBeanNames.size());

		for (String beanName : unorderedBeanNames) {

			Integer order = Optional.ofNullable(beanName)
				.filter(StringUtils::hasText)
				.map(beanFactory::getBeanDefinition)
				.filter(AnnotatedBeanDefinition.class::isInstance)
				.map(AnnotatedBeanDefinition.class::cast)
				.map(AnnotatedBeanDefinition::getFactoryMethodMetadata)
				.filter(methodMetadata -> methodMetadata.isAnnotated(Order.class.getName()))
				.map(methodMetadata -> methodMetadata.getAnnotationAttributes(Order.class.getName()))
				.map(annotationAttributes -> annotationAttributes.getOrDefault("value", Ordered.LOWEST_PRECEDENCE))
				.map(Integer.class::cast)
				.orElse(Ordered.LOWEST_PRECEDENCE);

			orderedBeanWrappers.add(DefaultOrderedBeanWrapper.from(beanName, beansOfType.get(beanName), order));
		}

		return orderedBeanWrappers;
	}

	@Nullable
	private static <T> OrderedBeanWrapper<T> toOrderedBeanWrapper(@NonNull Map.Entry<String, T> beanEntry) {

		T bean = beanEntry.getValue();

		Integer order = getOrder(bean);

		if (order == null) {
			order = bean != null ? OrderUtils.getOrder(bean.getClass()) : null;
		}

		return order != null
			? DefaultOrderedBeanWrapper.from(beanEntry.getKey(), bean, order)
			: null;
	}

	/**
	 * Null-safe operation to return the {@link Integer order} of the given {@link Object} if it is {@link Ordered}
	 * or {@literal null} if the given {@link Object} is not {@link Ordered}.
	 *
	 * @param target {@link Object} to evaluate; may be {@literal null}.
	 * @return the {@link Integer order} of the given {@link Object} if {@link Ordered},
	 * otherwise return {@literal null}.
	 * @see org.springframework.core.Ordered
	 */
	public static @Nullable Integer getOrder(@Nullable Object target) {
		return target instanceof Ordered ? ((Ordered) target).getOrder() : null;
	}

	/**
	 * Returns bean of the given {@link Class type} in an ordered {@link Stream}.
	 *
	 * @param <T> {@link Class type} of the beans.
	 * @param beanFactory {@link BeanFactory} from which to acquire the beans.
	 * @param beanType {@link Class type} of the beans.
	 * @return an ordered {@link Stream} of beans from the {@link BeanFactory} of the given {@link Class type}.
	 * @see org.springframework.beans.factory.BeanFactory
	 * @see java.util.stream.Stream
	 * @see java.lang.Class
	 */
	public static <T> Stream<T> getOrderedStreamOfBeansByType(@NonNull BeanFactory beanFactory,
			@NonNull Class<T> beanType) {

		Assert.notNull(beanFactory, "BeanFactory must not be null");
		Assert.notNull(beanType,"Bean type must not be null");

		return beanFactory.getBeanProvider(beanType).orderedStream();
	}

	public static Optional<Object> getPropertyValue(BeanDefinition beanDefinition, String propertyName) {

		return Optional.ofNullable(beanDefinition)
			.map(BeanDefinition::getPropertyValues)
			.map(propertyValues -> propertyValues.getPropertyValue(propertyName))
			.map(PropertyValue::getValue);
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
		return Objects.equals(obj1, obj2);
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

	public static Class<?> nullSafeType(Object target) {
		return nullSafeType(target, null);
	}

	public static Class<?> nullSafeType(Object target, Class<?> defaultType) {
		return target != null ? target.getClass() : defaultType;
	}

	public static boolean safeDoOperation(VoidReturningThrowableOperation operation) {
		return safeDoOperation(operation, () -> {});
	}

	public static boolean safeDoOperation(VoidReturningThrowableOperation operation, Runnable backupOperation) {

		try {
			operation.run();
			return true;
		}
		catch (Throwable cause) {
			backupOperation.run();
			return false;
		}
	}

	public static <T> T safeGetValue(ValueReturningThrowableOperation<T> operation) {
		return safeGetValue(operation, (T) null);
	}

	public static <T> T safeGetValue(ValueReturningThrowableOperation<T> operation, T defaultValue) {
		return safeGetValue(operation, (Supplier<T>) () -> defaultValue);
	}

	public static <T> T safeGetValue(ValueReturningThrowableOperation<T> operation, Supplier<T> defaultValueSupplier) {
		return safeGetValue(operation, (Function<Throwable, T>) exception -> defaultValueSupplier.get());
	}

	public static <T> T safeGetValue(ValueReturningThrowableOperation<T> operation,
			Function<Throwable, T> exceptionHandler) {

		try {
			return operation.get();
		}
		catch (Throwable cause) {
			return exceptionHandler.apply(cause);
		}
	}

	public static void safeRunOperation(VoidReturningThrowableOperation operation) {
		safeRunOperation(operation, cause -> new InvalidDataAccessApiUsageException("Failed to run operation", cause));
	}

	public static void safeRunOperation(VoidReturningThrowableOperation operation,
			Function<Throwable, RuntimeException> exceptionConverter) {

		try {
			operation.run();
		}
		catch (Throwable cause) {
			throw exceptionConverter.apply(cause);
		}
	}

	private static class DefaultOrderedBeanWrapper<T> implements OrderedBeanWrapper<T> {

		private static <T> OrderedBeanWrapper<T> from(String beanName, T bean) {
			return from(beanName, bean, Ordered.LOWEST_PRECEDENCE);
		}

		private static <T> OrderedBeanWrapper<T> from(String beanName, T bean, int order) {
			return new DefaultOrderedBeanWrapper<>(beanName, bean, order);
		}

		private final int order;

		private final T bean;

		private final String beanName;

		private DefaultOrderedBeanWrapper(String beanName, T bean, int order) {

			Assert.notNull(bean, "Bean must not be null");
			Assert.hasText(beanName, "Bean name is required");

			this.bean = bean;
			this.beanName = beanName;
			this.order = order;
		}

		@Override
		public T getBean() {
			return this.bean;
		}

		@Override
		public String getBeanName() {
			return this.beanName;
		}

		@Override
		public int getOrder() {
			return this.order;
		}
	}

	public interface OrderedBeanWrapper<T> extends Ordered {

		T getBean();

		String getBeanName();

	}

	@FunctionalInterface
	public interface ValueReturningThrowableOperation<T> {
		T get() throws Throwable;
	}

	/**
	 * @deprecated use {@link VoidReturningThrowableOperation}.
	 */
	@Deprecated
	public interface VoidReturningExceptionThrowingOperation extends VoidReturningThrowableOperation { }

	@FunctionalInterface
	public interface VoidReturningThrowableOperation {
		void run() throws Throwable;
	}
}
