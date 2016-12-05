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

import static org.springframework.data.gemfire.config.annotation.EnableEviction.EvictionPolicy;

import java.lang.annotation.Annotation;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.gemstone.gemfire.cache.EvictionAttributes;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.util.ObjectSizer;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportAware;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.EvictionActionType;
import org.springframework.data.gemfire.EvictionAttributesFactoryBean;
import org.springframework.data.gemfire.EvictionPolicyType;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.RegionLookupFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * The {@link EvictionConfiguration} class is a Spring {@link Configuration @Configuration} annotated class to enable
 * Eviction policy configuration on GemFire/Geode {@link Region Regions}.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanPostProcessor
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.ApplicationContextAware
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.ImportAware
 * @see org.springframework.data.gemfire.EvictionActionType
 * @see org.springframework.data.gemfire.EvictionAttributesFactoryBean
 * @see org.springframework.data.gemfire.EvictionPolicyType
 * @see org.springframework.data.gemfire.RegionFactoryBean
 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
 * @see com.gemstone.gemfire.cache.EvictionAttributes
 * @see com.gemstone.gemfire.cache.Region
 * @since 1.9.0
 */
@Configuration
public class EvictionConfiguration implements ApplicationContextAware, ImportAware {

	private ApplicationContext applicationContext;

	private EvictionPolicyConfigurer evictionPolicyConfigurer;

	/**
	 * Determines whether the Spring bean is an instance of {@link RegionFactoryBean}
	 * or {@link ClientRegionFactoryBean}.
	 *
	 * @param bean Spring bean to evaluate.
	 * @return a boolean value indicating whether the Spring bean is an instance of {@link RegionFactoryBean}.
	 * @see org.springframework.data.gemfire.RegionFactoryBean
	 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
	 */
	protected static boolean isRegionFactoryBean(Object bean) {
		return (bean instanceof RegionFactoryBean || bean instanceof ClientRegionFactoryBean);
	}

	/**
	 * Returns the {@link Annotation} {@link Class type} that enables and configures Eviction.
	 *
	 * @return the {@link Annotation} {@link Class type} to enable and configure Eviction.
	 * @see java.lang.annotation.Annotation
	 * @see java.lang.Class
	 */
	protected Class<? extends Annotation> getAnnotationType() {
		return EnableEviction.class;
	}

	/**
	 * Returns the name of the {@link Annotation} type that enables and configures Eviction.
	 *
	 * @return the name of the {@link Annotation} type that enables and configures Eviction.
	 * @see java.lang.Class#getName()
	 * @see #getAnnotationType()
	 */
	protected String getAnnotationTypeName() {
		return getAnnotationType().getName();
	}

	/**
	 * Returns the simple name of the {@link Annotation} type that enables and configures Eviction.
	 *
	 * @return the simple name of the {@link Annotation} type that enables and configures Eviction.
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
	public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
		this.applicationContext = applicationContext;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void setImportMetadata(AnnotationMetadata importMetadata) {
		if (importMetadata.hasAnnotation(getAnnotationTypeName())) {
			Map<String, Object> enableEvictionAttributes =
				importMetadata.getAnnotationAttributes(getAnnotationTypeName());

			AnnotationAttributes[] policies = (AnnotationAttributes[]) enableEvictionAttributes.get("policies");

			for (AnnotationAttributes evictionPolicyAttributes
					: ArrayUtils.nullSafeArray(policies, AnnotationAttributes.class)) {

				this.evictionPolicyConfigurer = ComposableEvictionPolicyConfigurer.compose(
					this.evictionPolicyConfigurer, EvictionPolicyMetaData.from(evictionPolicyAttributes,
						this.applicationContext));
			}

			this.evictionPolicyConfigurer = (this.evictionPolicyConfigurer != null ? this.evictionPolicyConfigurer
				: EvictionPolicyMetaData.fromDefaults());
		}
	}

	/**
	 * Returns a reference to the configured {@link EvictionPolicyConfigurer} used to configure the Eviction policy
	 * of a {@link Region}.
	 *
	 * @return a reference to the configured {@link EvictionPolicyConfigurer}.
	 * @see org.springframework.data.gemfire.config.annotation.EvictionConfiguration.EvictionPolicyConfigurer
	 */
	protected EvictionPolicyConfigurer getEvictionPolicyConfigurer() {
		Assert.state(this.evictionPolicyConfigurer != null,
			"EvictionPolicyConfigurer was not properly configured and initialized");

		return this.evictionPolicyConfigurer;
	}

	@Bean
	@SuppressWarnings("unused")
	public BeanPostProcessor evictionBeanPostProcessor() {
		return new BeanPostProcessor() {
			@Override
			public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
				return (isRegionFactoryBean(bean) ? getEvictionPolicyConfigurer().configure(bean) : bean);
			}

			@Override
			public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
				return bean;
			}
		};
	}

	/**
	 * {@link EvictionPolicyConfigurer} configures the Eviction policy of a GemFire {@link Region}.
	 */
	protected interface EvictionPolicyConfigurer {

		/**
		 * Configure the Eviction policy on the given SDG {@link RegionFactoryBean} or {@link ClientRegionFactoryBean}
		 * used to create a GemFire {@link Region}.
		 *
		 * @param regionFactoryBean {@link RegionFactoryBean} or {@link ClientRegionFactoryBean} used to create
		 * a GemFire {@link Region}.
		 * @return the given {@code regionFactoryBean}.
		 * @see org.springframework.data.gemfire.RegionFactoryBean
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
			return compose(Arrays.asList(ArrayUtils.nullSafeArray(array, EvictionPolicyConfigurer.class)));
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

			for (EvictionPolicyConfigurer evictionPolicyConfigurer : CollectionUtils.nullSafeIterable(iterable)) {
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
			return (one == null ? two : (two == null ? one : new ComposableEvictionPolicyConfigurer(one, two)));
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
			return two.configure(one.configure(regionFactoryBean));
		}
	}

	protected static class EvictionPolicyMetaData implements EvictionPolicyConfigurer {

		protected static final String[] ALL_REGIONS = new String[0];

		private final EvictionAttributes evictionAttributes;

		private final Set<String> regionNames = new HashSet<String>();

		protected static EvictionPolicyMetaData from(AnnotationAttributes evictionPolicyAttributes,
				ApplicationContext applicationContext) {

			return from((Integer) evictionPolicyAttributes.get("maximum"),
				evictionPolicyAttributes.<EvictionPolicyType>getEnum("type"),
					evictionPolicyAttributes.<EvictionActionType>getEnum("action"),
						resolveObjectSizer(evictionPolicyAttributes.getString("objectSizerName"), applicationContext),
							evictionPolicyAttributes.getStringArray("regionNames"));
		}

		protected static EvictionPolicyMetaData from(EvictionPolicy evictionPolicy,
				ApplicationContext applicationContext) {

			return from(evictionPolicy.maximum(), evictionPolicy.type(), evictionPolicy.action(),
				resolveObjectSizer(evictionPolicy.objectSizerName(), applicationContext), evictionPolicy.regionNames());
		}

		protected static EvictionPolicyMetaData from(int maximum, EvictionPolicyType type, EvictionActionType action,
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

			return (resolvable ? applicationContext.getBean(objectSizerName, ObjectSizer.class) : null);
		}

		/**
		 * Resolves the Eviction policy threshold (a.k.a. maximum) based on the {@link EvictionPolicyType}.
		 *
		 * For instance {@link EvictionPolicyType#HEAP_PERCENTAGE} does not support maximum/threshold since
		 * the settings are determined by the GemFire/Geode cache critical heap percentage and eviction heap percentage
		 * System property settings.
		 *
		 * @param maximum integer value specifying the configured Eviction threshold.
		 * @param type {@link EvictionPolicyType} specifying the type of Eviction algorithm.
		 * @return a resolved value for the Eviction maximum/threshold.
		 * @see org.springframework.data.gemfire.EvictionPolicyType
		 */
		protected static Integer resolveThreshold(int maximum, EvictionPolicyType type) {
			return (EvictionPolicyType.HEAP_PERCENTAGE.equals(type) ? null : maximum);
		}

		/**
		 * Constructs an instance of {@link EvictionPolicyMetaData} initialized with the given
		 * {@link EvictionAttributes} applying to all {@link Region Regions}.
		 *
		 * @param evictionAttributes {@link EvictionAttributes} specifying the Eviction policy configuration
		 * for a {@link Region}.
		 * @see com.gemstone.gemfire.cache.EvictionAttributes
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
		 * @see com.gemstone.gemfire.cache.EvictionAttributes
		 */
		protected EvictionPolicyMetaData(EvictionAttributes evictionAttributes, String[] regionNames) {
			Assert.notNull(evictionAttributes, "EvictionAttributes must not be null");

			this.evictionAttributes = evictionAttributes;
			Collections.addAll(this.regionNames, ArrayUtils.nullSafeArray(regionNames, String.class));
		}

		/**
		 * Returns an instance of the {@link EvictionAttributes} specifying the Eviction policy configuration
		 * captured in this Eviction policy meta-data.
		 *
		 * @return an instance of the {@link EvictionAttributes} specifying the {@link Region}
		 * Eviction policy configuration.
		 * @throws IllegalStateException if the {@link EvictionAttributes} were not properly initialized.
		 * @see com.gemstone.gemfire.cache.EvictionAttributes
		 */
		protected EvictionAttributes getEvictionAttributes() {
			Assert.state(this.evictionAttributes != null,
				"EvictionAttributes was not properly configured and initialized");

			return this.evictionAttributes;
		}

		/**
		 * Determines whether the given {@link Object} (e.g. Spring bean) is accepted for Eviction policy configuration.
		 *
		 * @param regionFactoryBean {@link Object} being evaluated as an Eviction policy configuration candidate.
		 * @return a boolean value indicating whether the {@link Object} is accepted for Eviction policy configuration.
		 * @see #isRegionFactoryBean(Object)
		 * @see #resolveRegionName(Object)
		 * @see #accepts(String)
		 */
		protected boolean accepts(Object regionFactoryBean) {
			return (isRegionFactoryBean(regionFactoryBean) && accepts(resolveRegionName(regionFactoryBean)));
		}

		/**
		 * Determine whether the {@link Region} identified by name is accepted for Eviction policy configuration.
		 *
		 * @param regionName name of the {@link Region} targeted for Eviction policy configuration.
		 * @return a boolean value if the named {@link Region} is accepted for Eviction policy configuration.
		 */
		protected boolean accepts(String regionName) {
			return (this.regionNames.isEmpty() || this.regionNames.contains(regionName));
		}

		/**
		 * Resolves the name of a given {@link Region} from the corresponding {@link RegionLookupFactoryBean} object.
		 *
		 * @param regionFactoryBean {@link RegionLookupFactoryBean} from which to resolve the {@link Region} name.
		 * @return the resolved name of the {@link Region} created from the given {@link RegionLookupFactoryBean}.
		 * @see org.springframework.data.gemfire.RegionLookupFactoryBean#resolveRegionName()
		 */
		protected String resolveRegionName(Object regionFactoryBean) {
			return (regionFactoryBean instanceof RegionLookupFactoryBean
				? ((RegionLookupFactoryBean) regionFactoryBean).resolveRegionName() : null);
		}

		/**
		 * Sets the {@link EvictionAttributes} on the {@link RegionFactoryBean} or {@link ClientRegionFactoryBean}
		 * used to create the targeted {@link Region}.
		 *
		 * @param regionFactoryBean {@link RegionFactoryBean} or {@link ClientRegionFactoryBean} on which to
		 * set the {@link EvictionAttributes} encapsulating the Eviction policy for the targeted {@link Region}.
		 * @return the {@code regionFactoryBean}.
		 * @see org.springframework.data.gemfire.RegionFactoryBean#setEvictionAttributes(EvictionAttributes)
		 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean#setEvictionAttributes(EvictionAttributes)
		 * @see com.gemstone.gemfire.cache.EvictionAttributes
		 * @see #getEvictionAttributes()
		 */
		protected Object setEvictionAttributes(Object regionFactoryBean) {
			if (regionFactoryBean instanceof RegionFactoryBean) {
				((RegionFactoryBean) regionFactoryBean).setEvictionAttributes(getEvictionAttributes());
			}
			else {
				((ClientRegionFactoryBean) regionFactoryBean).setEvictionAttributes(getEvictionAttributes());
			}

			return regionFactoryBean;
		}

		/**
		 * @inheritDoc
		 */
		@Override
		public Object configure(Object regionFactoryBean) {
			return (accepts(regionFactoryBean) ? setEvictionAttributes(regionFactoryBean) : regionFactoryBean);
		}
	}
}
