/*
 * Copyright 2016 the original author or authors.
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

package org.springframework.data.gemfire.config.annotation;

import static org.springframework.data.gemfire.config.annotation.EnableExpiration.ExpirationPolicy;
import static org.springframework.data.gemfire.config.annotation.EnableExpiration.ExpirationType;

import java.lang.annotation.Annotation;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.gemstone.gemfire.cache.AttributesMutator;
import com.gemstone.gemfire.cache.ExpirationAction;
import com.gemstone.gemfire.cache.ExpirationAttributes;
import com.gemstone.gemfire.cache.Region;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportAware;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.ExpirationActionType;
import org.springframework.data.gemfire.support.AnnotationBasedExpiration;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.util.Assert;

/**
 * {@link ExpirationConfiguration} is a Spring {@link Configuration} class used to configure expiration policies
 * for GemFire/Geode {@link Region Regions}.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.ImportAware
 * @see org.springframework.data.gemfire.config.annotation.EnableExpiration
 * @see com.gemstone.gemfire.cache.ExpirationAttributes
 * @see com.gemstone.gemfire.cache.Region
 * @since 1.9.0
 */
@Configuration
public class ExpirationConfiguration implements ImportAware {

	protected static final int DEFAULT_TIMEOUT = 0;

	protected static final ExpirationActionType DEFAULT_ACTION = ExpirationActionType.DEFAULT;

	protected static final ExpirationType[] DEFAULT_EXPIRATION_TYPES = { ExpirationType.IDLE_TIMEOUT };

	private ExpirationPolicyConfigurer expirationPolicyConfigurer;

	/**
	 * Returns the {@link Annotation} {@link Class type} that enables and configures Expiration.
	 *
	 * @return {@link Annotation} {@link Class type} that enables and configures Expiration.
	 * @see java.lang.annotation.Annotation
	 * @see java.lang.Class
	 */
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableExpiration.class;
	}

	/**
	 * Returns the name of the {@link Annotation} type that enables and configures Expiration.
	 *
	 * @return the name of the {@link Annotation} type that enables and configures Expiration.
	 * @see java.lang.Class#getName()
	 * @see #getAnnotationType()
	 */
	protected String getAnnotationTypeName() {
		return getAnnotationType().getName();
	}

	/**
	 * Returns the simple name of the {@link Annotation} type that enables and configures Expiration.
	 *
	 * @return the simple name of the {@link Annotation} type that enables and configures Expiration.
	 * @see java.lang.Class#getSimpleName()
	 * @see #getAnnotationType()
	 */
	@SuppressWarnings("unused")
	protected String getAnnotationTypeSimpleName() {
		return getAnnotationType().getSimpleName();
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void setImportMetadata(AnnotationMetadata importMetadata) {
		if (importMetadata.hasAnnotation(getAnnotationTypeName())) {
			Map<String, Object> enableExpirationAttributes =
				importMetadata.getAnnotationAttributes(getAnnotationTypeName());

			AnnotationAttributes[] policies =
				(AnnotationAttributes[]) enableExpirationAttributes.get("policies");

			for (AnnotationAttributes expirationPolicyAttributes :
					ArrayUtils.nullSafeArray(policies, AnnotationAttributes.class)) {

				this.expirationPolicyConfigurer = ComposableExpirationPolicyConfigurer.compose(
					this.expirationPolicyConfigurer, ExpirationPolicyMetaData.from(expirationPolicyAttributes));
			}

			this.expirationPolicyConfigurer = (this.expirationPolicyConfigurer != null ? this.expirationPolicyConfigurer
				: ExpirationPolicyMetaData.fromDefaults());
		}
	}

	/**
	 * Determines whether the given bean is a {@link Region}.
	 *
	 * @param bean {@link Object} to evaluate.
	 * @return a boolean value indicating whether the given bean is a {@link Region}.
	 * @see com.gemstone.gemfire.cache.Region
	 */
	protected boolean isRegion(Object bean) {
		return (bean instanceof Region);
	}

	protected ExpirationPolicyConfigurer getExpirationPolicyConfigurer() {
		Assert.state(this.expirationPolicyConfigurer != null,
			"ExpirationPolicyConfigurer was not properly configured and initialized");

		return expirationPolicyConfigurer;
	}

	@Bean
	@SuppressWarnings("unused")
	public BeanPostProcessor expirationBeanPostProcessor() {
		return new BeanPostProcessor() {
			@Override
			public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
				return bean;
			}

			@Override
			@SuppressWarnings("unchecked")
			public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
				return (isRegion(bean) ? getExpirationPolicyConfigurer().configure((Region<Object, Object>) bean)
					: bean);
			}

		};
	}

	/**
	 * Interface defining a contract for implementations that configure a {@link Region Region's} expiration policy.
	 */
	protected interface ExpirationPolicyConfigurer {

		/**
		 * Configures the expiration policy for the given {@link Region}.
		 *
		 * @param <K> {@link Class type} of the {@link Region} keys.
		 * @param <V> {@link Class type} of the {@link Region} values.
		 * @param region {@link Region} who's expiration policy will be configured.
		 * @return the given {@link Region}.
		 * @see com.gemstone.gemfire.cache.Region
		 */
		<K, V> Region<K, V> configure(Region<K, V> region);

	}

	/**
	 * {@link ComposableExpirationPolicyConfigurer} is a {@link ExpirationPolicyConfigurer} implementation
	 * that additionally implements the Composition Software Design Pattern to treat a collection of
	 * {@link ExpirationPolicyConfigurer} objects as a single instace of the {@link ExpirationPolicyConfigurer}.
	 *
	 * @see org.springframework.data.gemfire.config.annotation.ExpirationConfiguration.ExpirationPolicyConfigurer
	 * @see <a href="https://en.wikipedia.org/wiki/Composite_pattern">Composition Software Design Pattern</a>
	 */
	protected static class ComposableExpirationPolicyConfigurer implements ExpirationPolicyConfigurer {

		private final ExpirationPolicyConfigurer one;
		private final ExpirationPolicyConfigurer two;

		/**
		 * Factory method to compose an array of {@link ExpirationPolicyConfigurer} objects.
		 *
		 * @param array array of {@link ComposableExpirationPolicyConfigurer} objects to compose.
		 * @return a composition containing all the {@link ExpirationPolicyConfigurer} objects in the array.
		 * @see org.springframework.data.gemfire.config.annotation.ExpirationConfiguration.ExpirationPolicyConfigurer
		 * @see #compose(Iterable)
		 */
		protected static ExpirationPolicyConfigurer compose(ExpirationPolicyConfigurer[] array) {
			return compose(Arrays.asList(ArrayUtils.nullSafeArray(array, ExpirationPolicyConfigurer.class)));
		}

		/**
		 * Factory method to compose an {@link Iterable} of {@link ExpirationPolicyConfigurer} objects.
		 *
		 * @param iterable {@link Iterable} of {@link ComposableExpirationPolicyConfigurer} objects to compose.
		 * @return a composition containing all the {@link ExpirationPolicyConfigurer} objects in the {@link Iterable}.
		 * @see org.springframework.data.gemfire.config.annotation.ExpirationConfiguration.ExpirationPolicyConfigurer
		 * @see #compose(ExpirationPolicyConfigurer, ExpirationPolicyConfigurer)
		 */
		protected static ExpirationPolicyConfigurer compose(Iterable<ExpirationPolicyConfigurer> iterable) {
			ExpirationPolicyConfigurer current = null;

			for (ExpirationPolicyConfigurer configurer : CollectionUtils.nullSafeIterable(iterable)) {
				current = compose(current, configurer);
			}

			return current;
		}

		/**
		 * Factory method to compose 2 {@link ExpirationPolicyConfigurer} objects.
		 *
		 * @param one first {@link ComposableExpirationPolicyConfigurer} to compose.
		 * @param two second {@link ComposableExpirationPolicyConfigurer} to compose.
		 * @return a composition of the 2 {@link ExpirationPolicyConfigurer} objects.
		 * Returns {@code one} if {@code two} is {@literal null} or {@code two} if {@code one} is {@literal null}.
		 * @see org.springframework.data.gemfire.config.annotation.ExpirationConfiguration.ExpirationPolicyConfigurer
		 */
		protected static ExpirationPolicyConfigurer compose(ExpirationPolicyConfigurer one,
				ExpirationPolicyConfigurer two) {

			return (one == null ? two : (two == null ? one : new ComposableExpirationPolicyConfigurer(one, two)));
		}

		/**
		 * Constructs an instance of the {@link ComposableExpirationPolicyConfigurer} initialized with
		 * 2 {@link ExpirationPolicyConfigurer} objects.
		 *
		 * @param one first {@link ComposableExpirationPolicyConfigurer} to compose.
		 * @param two second {@link ComposableExpirationPolicyConfigurer} to compose.
		 * @see org.springframework.data.gemfire.config.annotation.ExpirationConfiguration.ExpirationPolicyConfigurer
		 */
		private ComposableExpirationPolicyConfigurer(ExpirationPolicyConfigurer one, ExpirationPolicyConfigurer two) {
			this.one = one;
			this.two = two;
		}

		/**
		 * @inheritDoc
		 */
		@Override
		public <K, V> Region<K, V> configure(Region<K, V> region) {
			return this.two.configure(this.one.configure(region));
		}
	}

	/**
	 * {@link ExpirationPolicyMetaData} is a {@link ExpirationPolicyConfigurer} implementation that encapsulates
	 * the expiration configuration meta-data (e.g. expiration timeout and action) necessary to configure
	 * a {@link Region Regions's} expiration policy and behavior.
	 *
	 * This class is meant to capture the expiration configuration meta-data specified in the {@link ExpirationPolicy}
	 * nested annotation in the application-level {@link EnableExpiration} annotation.

	 * @see org.springframework.data.gemfire.config.annotation.ExpirationConfiguration.ExpirationPolicyConfigurer
	 */
	protected static class ExpirationPolicyMetaData implements ExpirationPolicyConfigurer {

		protected static final String[] ALL_REGIONS = new String[0];

		private final ExpirationAttributes defaultExpirationAttributes;

		private final Set<String> regionNames = new HashSet<String>();

		private final Set<ExpirationType> types = new HashSet<ExpirationType>();

		/**
		 * Factory method to construct an instance of {@link ExpirationPolicyMetaData} initialized with
		 * the given {@link AnnotationAttributes} from the nested {@link ExpirationPolicy} annotation
		 * specified in an application-level {@link EnableExpiration} annotation.
		 *
		 * @param expirationPolicyAttributes {@link AnnotationAttributes} from a {@link ExpirationPolicy} annotation.
		 * @return an instance of the {@link ExpirationPolicyMetaData} initialized from
		 * {@link ExpirationPolicy} {@link AnnotationAttributes}.
		 * @throws IllegalArgumentException if {@link AnnotationAttributes#annotationType()} is not assignable to
		 * {@link ExpirationPolicy}.
		 * @see #newExpirationPolicyMetaData(int, ExpirationActionType, String[], ExpirationType[])
		 * @see org.springframework.core.annotation.AnnotationAttributes
		 */
		protected static ExpirationPolicyMetaData from(AnnotationAttributes expirationPolicyAttributes) {
			Assert.isAssignable(ExpirationPolicy.class, expirationPolicyAttributes.annotationType());

			return newExpirationPolicyMetaData((Integer) expirationPolicyAttributes.get("timeout"),
				expirationPolicyAttributes.<ExpirationActionType>getEnum("action"),
					expirationPolicyAttributes.getStringArray("regionNames"),
						(ExpirationType[]) expirationPolicyAttributes.get("types"));
		}

		/**
		 * Factory method to construct an instance of {@link ExpirationPolicyMetaData} initialized with
		 * the given attribute values from the nested {@link ExpirationPolicy} annotation specified in
		 * an application-level {@link EnableExpiration} annotation.
		 *
		 * @param expirationPolicy {@link ExpirationPolicy} annotation containing the attribute values
		 * used to initialize the {@link ExpirationPolicyMetaData} instance.
		 * @return an instance of the {@link ExpirationPolicyMetaData} initialized from
		 * {@link ExpirationPolicy} attributes values.
		 * @see org.springframework.data.gemfire.config.annotation.EnableExpiration.ExpirationPolicy
		 * @see #newExpirationPolicyMetaData(int, ExpirationActionType, String[], ExpirationType[])
		 */
		protected static ExpirationPolicyMetaData from(ExpirationPolicy expirationPolicy) {
			return newExpirationPolicyMetaData(expirationPolicy.timeout(), expirationPolicy.action(),
				expirationPolicy.regionNames(), expirationPolicy.types());
		}

		/**
		 * Factory method to construct an instance of {@link ExpirationPolicyMetaData} using default expiration policy
		 * settings.
		 *
		 * @see #newExpirationPolicyMetaData(int, ExpirationActionType, String[], ExpirationType[])
		 */
		protected static ExpirationPolicyMetaData fromDefaults() {
			return newExpirationPolicyMetaData(DEFAULT_TIMEOUT, DEFAULT_ACTION, ALL_REGIONS, DEFAULT_EXPIRATION_TYPES);
		}

		/**
		 * Factory method used to construct a new instance of the {@link ExpirationAttributes} initialized with
		 * the given expiration timeout and action that is taken when an {@link Region} entry times out.
		 *
		 * @param timeout int value indicating the expiration timeout in seconds.
		 * @param action expiration action to take when the {@link Region} entry times out.
		 * @return a new instance of {@link ExpirationAttributes} initialized with the given expiration timeout
		 * and action.
		 * @see com.gemstone.gemfire.cache.ExpirationAttributes
		 * @see #newExpirationAttributes(int, ExpirationAction)
		 */
		protected static ExpirationAttributes newExpirationAttributes(int timeout, ExpirationActionType action) {
			return newExpirationAttributes(timeout, action.getExpirationAction());
		}

		/**
		 * Factory method used to construct a new instance of the {@link ExpirationAttributes} initialized with
		 * the given expiration timeout and action that is taken when an {@link Region} entry times out.
		 *
		 * @param timeout int value indicating the expiration timeout in seconds.
		 * @param action expiration action to take when the {@link Region} entry times out.
		 * @return a new instance of {@link ExpirationAttributes} initialized with the given expiration timeout
		 * and action.
		 * @see com.gemstone.gemfire.cache.ExpirationAttributes
		 */
		protected static ExpirationAttributes newExpirationAttributes(int timeout, ExpirationAction action) {
			return new ExpirationAttributes(timeout, action);
		}

		/**
		 * Factory method used to construct an instance of {@link ExpirationPolicyMetaData} initialized with
		 * the given expiration policy meta-data.
		 *
		 * @param timeout int value indicating the expiration timeout in seconds.
		 * @param action expiration action taken when the {@link Region} entry expires.
		 * @param regionNames names of {@link Region Regions} configured with the expiration policy meta-data.
		 * @param types type of expiration algorithm/behavior (TTI/TTL) configured for the {@link Region}.
		 * @return an instance of {@link ExpirationPolicyMetaData} initialized with the given expiration policy
		 * meta-data.
		 * @throws IllegalArgumentException if the {@link ExpirationType} array is empty.
		 * @see org.springframework.data.gemfire.config.annotation.EnableExpiration.ExpirationType
		 * @see org.springframework.data.gemfire.ExpirationActionType
		 * @see #ExpirationPolicyMetaData(ExpirationAttributes, Set, Set)
		 * @see #newExpirationAttributes(int, ExpirationActionType)
		 */
		protected static ExpirationPolicyMetaData newExpirationPolicyMetaData(int timeout, ExpirationActionType action,
				String[] regionNames, ExpirationType[] types) {

			return new ExpirationPolicyMetaData(newExpirationAttributes(timeout, action),
				CollectionUtils.asSet(ArrayUtils.nullSafeArray(regionNames, String.class)),
					CollectionUtils.asSet(ArrayUtils.nullSafeArray(types, ExpirationType.class)));
		}

		/**
		 * Resolves the {@link ExpirationAction} used in the expiration policy.  Defaults to
		 * {@link ExpirationActionType#INVALIDATE} if {@code action} is {@literal null}.
		 *
		 * @param action given {@link ExpirationActionType} to evaluate.
		 * @return the resolved {@link ExpirationActionType} or the default if {@code action} is {@literal null}.
		 * @see org.springframework.data.gemfire.ExpirationActionType
		 */
		protected static ExpirationActionType resolveAction(ExpirationActionType action) {
			return SpringUtils.defaultIfNull(action, DEFAULT_ACTION);
		}

		/**
		 * Resolves the expiration timeout used in the expiration policy.  Defaults to {@literal 0} if {@code timeout}
		 * is less than {@literal 0}.
		 *
		 * @param timeout int value expressing the expiration timeout in seconds.
		 * @return the resolved expiration policy timeout.
		 */
		protected static int resolveTimeout(int timeout) {
			return Math.max(timeout, DEFAULT_TIMEOUT);
		}

		/**
		 * Constructs an instance of {@link ExpirationPolicyMetaData} initialized with the given expiration policy
		 * configuraiton meta-data and {@link Region} expiration settings.
		 *
		 * @param timeout int value indicating the expiration timeout in seconds.
		 * @param action expiration action taken when the {@link Region} entry expires.
		 * @param regionNames names of {@link Region Regions} configured with the expiration policy meta-data.
		 * @param types type of expiration algorithm/behavior (TTI/TTL) configured for the {@link Region}.
		 * @throws IllegalArgumentException if the {@link ExpirationType} {@link Set} is empty.
		 * @see org.springframework.data.gemfire.config.annotation.EnableExpiration.ExpirationType
		 * @see org.springframework.data.gemfire.ExpirationActionType
		 * @see #ExpirationPolicyMetaData(ExpirationAttributes, Set, Set)
		 * @see #newExpirationAttributes(int, ExpirationActionType)
		 * @see #resolveAction(ExpirationActionType)
		 * @see #resolveTimeout(int)
		 */
		protected ExpirationPolicyMetaData(int timeout, ExpirationActionType action, Set<String> regionNames,
				Set<ExpirationType> types) {

			this(newExpirationAttributes(resolveTimeout(timeout), resolveAction(action)), regionNames, types);
		}

		/**
		 * Constructs an instance of {@link ExpirationPolicyMetaData} initialized with the given expiration policy
		 * configuraiton meta-data and {@link Region} expiration settings.
		 *
		 * @param expirationAttributes {@link ExpirationAttributes} specifying the expiration timeout in seconds
		 * and expiration action taken when the {@link Region} entry expires.
		 * @param regionNames names of {@link Region Regions} configured with the expiration policy meta-data.
		 * @param types type of expiration algorithm/behaviors (TTI/TTL) configured for the {@link Region}.
		 * @throws IllegalArgumentException if the {@link ExpirationType} {@link Set} is empty.
		 * @see org.springframework.data.gemfire.config.annotation.EnableExpiration.ExpirationType
		 * @see com.gemstone.gemfire.cache.ExpirationAttributes
		 */
		protected ExpirationPolicyMetaData(ExpirationAttributes expirationAttributes, Set<String> regionNames,
				Set<ExpirationType> types) {

			Assert.notEmpty(types, "At least one ExpirationPolicy type [TTI, TTL] must be specified");

			this.defaultExpirationAttributes = expirationAttributes;
			this.regionNames.addAll(CollectionUtils.nullSafeSet(regionNames));
			this.types.addAll(CollectionUtils.nullSafeSet(types));
		}

		/**
		 * Determines whether to apply this expiration policy to the given {@link Region}.
		 *
		 * @param region {@link Region} to evaluate.
		 * @return a boolean value indicating whether the expiration policy applies to the given {@link Region}.
		 * @see com.gemstone.gemfire.cache.Region
		 * @see #accepts(String)
		 */
		protected boolean accepts(Region region) {
			return (region != null && accepts(region.getName()));
		}

		/**
		 * Determines whether to apply this expiration policy to the given {@link Region} identified by name.
		 *
		 * @param regionName name of the {@link Region} to evaluate.
		 * @return a boolean value indicating whether the expiration policy applies to the given {@link Region}
		 * identified by name.
		 */
		protected boolean accepts(String regionName) {
			return (this.regionNames.isEmpty() || this.regionNames.contains(regionName));
		}

		/**
		 * Determines whether Idle Timeout Expiration (TTI) was configured for this expiration policy.
		 *
		 * @return a boolean value indicating whether Idle Timeout Expiration (TTI) was configuration for
		 * this expiration policy.
		 */
		protected boolean isIdleTimeout() {
			return this.types.contains(ExpirationType.IDLE_TIMEOUT);
		}

		/**
		 * Determines whether Time-To-Live Expiration (TTL) was configured for this expiration policy.
		 *
		 * @return a boolean value indicating whether Time-To-Live Expiration (TTL) was configuration for
		 * this expiration policy.
		 */
		protected boolean isTimeToLive() {
			return this.types.contains(ExpirationType.TIME_TO_LIVE);
		}

		/**
		 * @inheritDoc
		 */
		@Override
		public <K, V> Region<K, V> configure(Region<K, V> region) {
			if (accepts(region)) {
				AttributesMutator<K, V> regionAttributesMutator = region.getAttributesMutator();

				ExpirationAttributes defaultExpirationAttributes = defaultExpirationAttributes();

				if (isIdleTimeout()) {
					regionAttributesMutator.setCustomEntryIdleTimeout(
						AnnotationBasedExpiration.<K, V>forIdleTimeout(defaultExpirationAttributes));
				}

				if (isTimeToLive()) {
					regionAttributesMutator.setCustomEntryTimeToLive(
						AnnotationBasedExpiration.<K, V>forTimeToLive(defaultExpirationAttributes));
				}
			}

			return region;
		}

		/**
		 * Returns the default, fallback {@link ExpirationAttributes}.
		 *
		 * @return an {@link ExpirationAttributes} containing the defaults.
		 * @see com.gemstone.gemfire.cache.ExpirationAttributes
		 */
		protected ExpirationAttributes defaultExpirationAttributes() {
			return this.defaultExpirationAttributes;
		}
	}
}
