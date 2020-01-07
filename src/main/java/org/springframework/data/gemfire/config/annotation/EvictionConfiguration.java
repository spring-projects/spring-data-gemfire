/*
 * Copyright 2016-2020 the original author or authors.
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
package org.springframework.data.gemfire.config.annotation;

import static org.springframework.data.gemfire.config.annotation.EnableEviction.EvictionPolicy;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.lang.annotation.Annotation;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;

import org.apache.geode.cache.EvictionAttributes;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.util.ObjectSizer;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportAware;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.PeerRegionFactoryBean;
import org.springframework.data.gemfire.ResolvableRegionFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.eviction.EvictingRegionFactoryBean;
import org.springframework.data.gemfire.eviction.EvictionActionType;
import org.springframework.data.gemfire.eviction.EvictionAttributesFactoryBean;
import org.springframework.data.gemfire.eviction.EvictionPolicyType;
import org.springframework.lang.NonNull;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * The {@link EvictionConfiguration} class is a Spring {@link Configuration @Configuration} annotated class to enable
 * Eviction policy configuration on cache {@link Region Regions}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.EvictionAttributes
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.util.ObjectSizer
 * @see org.springframework.beans.factory.config.BeanPostProcessor
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.ApplicationContextAware
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.ImportAware
 * @see PeerRegionFactoryBean
 * @see org.springframework.data.gemfire.ResolvableRegionFactoryBean
 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport
 * @see org.springframework.data.gemfire.eviction.EvictionActionType
 * @see org.springframework.data.gemfire.eviction.EvictionAttributesFactoryBean
 * @see org.springframework.data.gemfire.eviction.EvictionPolicyType
 * @since 1.9.0
 */
@Configuration
public class EvictionConfiguration extends AbstractAnnotationConfigSupport
		implements ApplicationContextAware, ImportAware {

	private ApplicationContext applicationContext;

	private EvictionPolicyConfigurer evictionPolicyConfigurer;

	/**
	 * Returns the {@link Annotation} {@link Class type} that enables and configures Eviction.
	 *
	 * @return the {@link Annotation} {@link Class type} to enable and configure Eviction.
	 * @see java.lang.annotation.Annotation
	 * @see java.lang.Class
	 */
	@Override
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableEviction.class;
	}

	/**
	 * Sets a reference to the Spring {@link ApplicationContext}.
	 *
	 * @param applicationContext Spring {@link ApplicationContext} in use.
	 * @throws BeansException if an error occurs while storing a reference to the Spring {@link ApplicationContext}.
	 * @see org.springframework.context.ApplicationContextAware#setApplicationContext(ApplicationContext)
	 * @see org.springframework.context.ApplicationContext
	 */
	@Override
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		this.applicationContext = applicationContext;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void setImportMetadata(AnnotationMetadata importMetadata) {

		if (isAnnotationPresent(importMetadata)) {

			AnnotationAttributes enableEvictionAttributes = getAnnotationAttributes(importMetadata);

			AnnotationAttributes[] policies = enableEvictionAttributes.getAnnotationArray("policies");

			for (AnnotationAttributes evictionPolicyAttributes : nullSafeArray(policies, AnnotationAttributes.class)) {
				this.evictionPolicyConfigurer =
					ComposableEvictionPolicyConfigurer.compose(this.evictionPolicyConfigurer,
						EvictionPolicyMetaData.from(evictionPolicyAttributes, this.applicationContext));
			}

			this.evictionPolicyConfigurer = Optional.ofNullable(this.evictionPolicyConfigurer)
				.orElseGet(EvictionPolicyMetaData::fromDefaults);
		}
	}

	/**
	 * Determines whether the Spring bean is an instance of {@link EvictingRegionFactoryBean}.
	 *
	 * @param bean Spring bean to evaluate.
	 * @return a boolean value indicating whether the Spring bean is an instance of {@link EvictingRegionFactoryBean}.
	 * @see org.springframework.data.gemfire.eviction.EvictingRegionFactoryBean
	 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
	 * @see PeerRegionFactoryBean
	 */
	protected static boolean isRegionFactoryBean(Object bean) {
		return bean instanceof EvictingRegionFactoryBean;
	}

	/**
	 * Returns a reference to the configured {@link EvictionPolicyConfigurer} used to configure the Eviction policy
	 * of a {@link Region}.
	 *
	 * @return a reference to the configured {@link EvictionPolicyConfigurer}.
	 * @see org.springframework.data.gemfire.config.annotation.EvictionConfiguration.EvictionPolicyConfigurer
	 */
	protected EvictionPolicyConfigurer getEvictionPolicyConfigurer() {

		return Optional.ofNullable(this.evictionPolicyConfigurer).orElseThrow(() ->
			newIllegalStateException("EvictionPolicyConfigurer was not properly configured and initialized"));
	}

	@Bean
	@SuppressWarnings("unused")
	public BeanPostProcessor evictionBeanPostProcessor() {

		return new BeanPostProcessor() {

			@Override
			public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
				return isRegionFactoryBean(bean) ? getEvictionPolicyConfigurer().configure(bean) : bean;
			}
		};
	}

	/**
	 * {@link EvictionPolicyConfigurer} configures the Eviction policy of a Pivotal GemFire {@link Region}.
	 */
	protected interface EvictionPolicyConfigurer {

		/**
		 * Configure the Eviction policy on the given SDG {@link PeerRegionFactoryBean} or {@link ClientRegionFactoryBean}
		 * used to create a GemFire {@link Region}.
		 *
		 * @param regionFactoryBean {@link PeerRegionFactoryBean} or {@link ClientRegionFactoryBean} used to create
		 * a GemFire {@link Region}.
		 * @return the given {@code regionFactoryBean}.
		 * @see PeerRegionFactoryBean
		 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
		 */
		Object configure(Object regionFactoryBean);

	}

	/**
	 * {@link ComposableEvictionPolicyConfigurer} is a {@link EvictionPolicyConfigurer} implementation that composes
	 * multiple {@link EvictionPolicyConfigurer} objects into a composition using the Composite Software Design Pattern
	 * making the composition appear as a single {@link EvictionPolicyConfigurer}.
	 *
	 * @see org.springframework.data.gemfire.config.annotation.EvictionConfiguration.EvictionPolicyConfigurer
	 */
	protected static class ComposableEvictionPolicyConfigurer implements EvictionPolicyConfigurer {

		private final EvictionPolicyConfigurer one;
		private final EvictionPolicyConfigurer two;

		/**
		 * Composes the array of {@link EvictionPolicyConfigurer} objects into a single
		 * {@link EvictionPolicyConfigurer} implementation using the Composite Software Design Pattern.
		 *
		 * @param array array of {@link EvictionPolicyConfigurer} objects to compose.
		 * @return an {@link EvictionPolicyConfigurer} implementation composed from the array
		 * of {@link EvictionPolicyConfigurer} objects.
		 * @see org.springframework.data.gemfire.config.annotation.EvictionConfiguration.EvictionPolicyConfigurer
		 * @see #compose(Iterable)
		 */
		@SuppressWarnings("unused")
		protected static EvictionPolicyConfigurer compose(EvictionPolicyConfigurer[] array) {
			return compose(Arrays.asList(nullSafeArray(array, EvictionPolicyConfigurer.class)));
		}

		/**
		 * Composes the {@link Iterable} of {@link EvictionPolicyConfigurer} objects into a single
		 * {@link EvictionPolicyConfigurer} implementation using the Composite Software Design Pattern.
		 *
		 * @param iterable {@link Iterable} of {@link EvictionPolicyConfigurer} objects to compose.
		 * @return an {@link EvictionPolicyConfigurer} implementation composed from the {@link Iterable}
		 * of {@link EvictionPolicyConfigurer} objects.
		 * @see org.springframework.data.gemfire.config.annotation.EvictionConfiguration.EvictionPolicyConfigurer
		 * @see #compose(EvictionPolicyConfigurer, EvictionPolicyConfigurer)
		 */
		protected static EvictionPolicyConfigurer compose(Iterable<EvictionPolicyConfigurer> iterable) {

			EvictionPolicyConfigurer current = null;

			for (EvictionPolicyConfigurer evictionPolicyConfigurer : nullSafeIterable(iterable)) {
				current = compose(current, evictionPolicyConfigurer);
			}

			return current;
		}

		/**
		 * Composes two {@link EvictionPolicyConfigurer} objects into a composition object
		 * implementing the {@link EvictionPolicyConfigurer} interface.
		 *
		 * @param one first {@link EvictionPolicyConfigurer} object to compose.
		 * @param two second {@link EvictionPolicyConfigurer} object to compose.
		 * @return an {@link EvictionPolicyConfigurer} object implementation composed of
		 * multiple {@link EvictionPolicyConfigurer} objects using the Composite Software Design Pattern.
		 */
		protected static EvictionPolicyConfigurer compose(EvictionPolicyConfigurer one, EvictionPolicyConfigurer two) {

			return one == null ? two
				: (two == null ? one
				: new ComposableEvictionPolicyConfigurer(one, two));
		}

		/**
		 * Constructs a new instance of the {@link ComposableEvictionPolicyConfigurer} initialized with the two
		 * {@link EvictionPolicyConfigurer} objects.
		 *
		 * @param one first {@link EvictionPolicyConfigurer} object to compose.
		 * @param two second {@link EvictionPolicyConfigurer} object to compose.
		 */
		private ComposableEvictionPolicyConfigurer(EvictionPolicyConfigurer one, EvictionPolicyConfigurer two) {

			this.one = one;
			this.two = two;
		}

		/**
		 * @inheritDoc
		 */
		@Override
		public Object configure(Object regionFactoryBean) {
			return this.two.configure(this.one.configure(regionFactoryBean));
		}
	}

	protected static class EvictionPolicyMetaData implements EvictionPolicyConfigurer {

		protected static final String[] ALL_REGIONS = new String[0];

		private final EvictionAttributes evictionAttributes;

		private final Set<String> regionNames = new HashSet<>();

		protected static EvictionPolicyMetaData from(@NonNull AnnotationAttributes evictionPolicyAttributes,
				@NonNull ApplicationContext applicationContext) {

			Assert.isAssignable(EvictionPolicy.class, evictionPolicyAttributes.annotationType());

			return from(evictionPolicyAttributes.getEnum("type"),
				(Integer) evictionPolicyAttributes.get("maximum"),
				evictionPolicyAttributes.getEnum("action"),
				resolveObjectSizer(evictionPolicyAttributes.getString("objectSizerName"), applicationContext),
				evictionPolicyAttributes.getStringArray("regionNames"));
		}

		protected static EvictionPolicyMetaData from(EvictionPolicy evictionPolicy,
				ApplicationContext applicationContext) {

			return from(evictionPolicy.type(), evictionPolicy.maximum(), evictionPolicy.action(),
				resolveObjectSizer(evictionPolicy.objectSizerName(), applicationContext), evictionPolicy.regionNames());
		}

		protected static EvictionPolicyMetaData from(EvictionPolicyType type, int maximum, EvictionActionType action,
				ObjectSizer objectSizer, String... regionNames) {

			EvictionAttributesFactoryBean factoryBean = new EvictionAttributesFactoryBean();

			factoryBean.setAction(action.getEvictionAction());
			factoryBean.setObjectSizer(objectSizer);
			factoryBean.setThreshold(resolveThreshold(maximum, type));
			factoryBean.setType(type);
			factoryBean.afterPropertiesSet();

			return new EvictionPolicyMetaData(factoryBean.getObject(), regionNames);
		}

		protected static EvictionPolicyMetaData fromDefaults() {
			return new EvictionPolicyMetaData(EvictionAttributes.createLRUEntryAttributes());
		}

		protected static ObjectSizer resolveObjectSizer(String objectSizerName, ApplicationContext applicationContext) {

			boolean resolvable = StringUtils.hasText(objectSizerName)
				&& applicationContext.containsBean(objectSizerName);

			return resolvable ? applicationContext.getBean(objectSizerName, ObjectSizer.class) : null;
		}

		/**
		 * Resolves the Eviction policy threshold (a.k.a. maximum) based on the {@link EvictionPolicyType}.
		 *
		 * For instance {@link EvictionPolicyType#HEAP_PERCENTAGE} does not support maximum/threshold since
		 * the settings are determined by the Pivotal GemFire/Apache Geode cache critical heap percentage and eviction heap percentage
		 * System property settings.
		 *
		 * @param maximum integer value specifying the configured Eviction threshold.
		 * @param type {@link EvictionPolicyType} specifying the type of Eviction algorithm.
		 * @return a resolved value for the Eviction maximum/threshold.
		 * @see org.springframework.data.gemfire.eviction.EvictionPolicyType
		 */
		protected static Integer resolveThreshold(int maximum, EvictionPolicyType type) {
			return EvictionPolicyType.HEAP_PERCENTAGE.equals(type) ? null : maximum;
		}

		/**
		 * Constructs an instance of {@link EvictionPolicyMetaData} initialized with the given
		 * {@link EvictionAttributes} applying to all {@link Region Regions}.
		 *
		 * @param evictionAttributes {@link EvictionAttributes} specifying the Eviction policy configuration
		 * for a {@link Region}.
		 * @see org.apache.geode.cache.EvictionAttributes
		 * @see #EvictionPolicyMetaData(EvictionAttributes, String[])
		 */
		protected EvictionPolicyMetaData(EvictionAttributes evictionAttributes) {
			this(evictionAttributes, ALL_REGIONS);
		}

		/**
		 * Constructs an instance of {@link EvictionPolicyMetaData} initialized with the given
		 * {@link EvictionAttributes} to apply to the specific {@link Region Regions}.
		 *
		 * @param evictionAttributes {@link EvictionAttributes} specifying the Eviction policy configuration
		 * for a {@link Region}.
		 * @param regionNames names of {@link Region Regions} on which the Eviction policy is applied.
		 * @see org.apache.geode.cache.EvictionAttributes
		 */
		protected EvictionPolicyMetaData(EvictionAttributes evictionAttributes, String[] regionNames) {

			this.evictionAttributes = Optional.ofNullable(evictionAttributes).orElseThrow(() ->
				newIllegalArgumentException("EvictionAttributes are required"));

			Collections.addAll(this.regionNames, nullSafeArray(regionNames, String.class));
		}

		/**
		 * Returns an instance of the {@link EvictionAttributes} specifying the Eviction policy configuration
		 * captured in this Eviction policy meta-data.
		 *
		 * @return an instance of the {@link EvictionAttributes} specifying the {@link Region}
		 * Eviction policy configuration.
		 * @throws IllegalStateException if the {@link EvictionAttributes} were not properly initialized.
		 * @see org.apache.geode.cache.EvictionAttributes
		 */
		protected EvictionAttributes getEvictionAttributes() {

			return Optional.ofNullable(this.evictionAttributes).orElseThrow(() ->
				newIllegalStateException("EvictionAttributes was not properly configured and initialized"));
		}

		/**
		 * Determines whether the given {@link Object} (e.g. Spring bean) is accepted for Eviction policy configuration.
		 *
		 * @param regionFactoryBean {@link Object} being evaluated as an Eviction policy configuration candidate.
		 * @return a boolean value indicating whether the {@link Object} is accepted for Eviction policy configuration.
		 * @see #isRegionFactoryBean(Object)
		 * @see #resolveRegionName(Object)
		 * @see #accepts(Supplier)
		 */
		protected boolean accepts(Object regionFactoryBean) {
			return isRegionFactoryBean(regionFactoryBean) && accepts(() -> resolveRegionName(regionFactoryBean));
		}

		/**
		 * Determine whether the {@link Region} identified by name is accepted for Eviction policy configuration.
		 *
		 * @param regionName name of the {@link Region} targeted for Eviction policy configuration.
		 * @return a boolean value if the named {@link Region} is accepted for Eviction policy configuration.
		 */
		protected boolean accepts(Supplier<String> regionName) {
			return this.regionNames.isEmpty() || this.regionNames.contains(regionName.get());
		}

		/**
		 * Resolves the name of a given {@link Region} from the corresponding {@link ResolvableRegionFactoryBean} object.
		 *
		 * @param regionFactoryBean {@link ResolvableRegionFactoryBean} from which to resolve the {@link Region} name.
		 * @return the resolved name of the {@link Region} created from the given {@link ResolvableRegionFactoryBean}.
		 * @see org.springframework.data.gemfire.ResolvableRegionFactoryBean#resolveRegionName()
		 */
		protected String resolveRegionName(Object regionFactoryBean) {

			return regionFactoryBean instanceof ResolvableRegionFactoryBean
				? ((ResolvableRegionFactoryBean) regionFactoryBean).resolveRegionName()
				: null;
		}

		/**
		 * Sets the {@link EvictionAttributes} on the {@link PeerRegionFactoryBean} or {@link ClientRegionFactoryBean}
		 * used to create the targeted {@link Region}.
		 *
		 * @param regionFactoryBean {@link PeerRegionFactoryBean} or {@link ClientRegionFactoryBean} on which to
		 * set the {@link EvictionAttributes} encapsulating the Eviction policy for the targeted {@link Region}.
		 * @return the {@code regionFactoryBean}.
		 * @see org.springframework.data.gemfire.eviction.EvictingRegionFactoryBean#setEvictionAttributes(EvictionAttributes)
		 * @see org.apache.geode.cache.EvictionAttributes
		 * @see #getEvictionAttributes()
		 */
		protected EvictingRegionFactoryBean setEvictionAttributes(EvictingRegionFactoryBean regionFactoryBean) {

			regionFactoryBean.setEvictionAttributes(getEvictionAttributes());

			return regionFactoryBean;
		}

		@Override
		public Object configure(Object regionFactoryBean) {

			return accepts(regionFactoryBean)
				? setEvictionAttributes((EvictingRegionFactoryBean) regionFactoryBean)
				: regionFactoryBean;
		}
	}
}
