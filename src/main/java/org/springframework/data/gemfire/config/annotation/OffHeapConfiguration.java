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

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.apache.geode.cache.Region;
import org.springframework.beans.BeansException;
import org.springframework.beans.PropertyValue;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.GenericRegionFactoryBean;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.ReplicatedRegionFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport;
import org.springframework.data.gemfire.util.ArrayUtils;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.data.gemfire.util.PropertiesBuilder;

/**
 * The OffHeapConfiguration class is a Spring {@link org.springframework.context.annotation.ImportBeanDefinitionRegistrar}
 * capable of enabling GemFire cache {@link Region Regions} to use Off-Heap memory for data storage.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.EnableOffHeap
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @since 1.9.0
 */
public class OffHeapConfiguration extends EmbeddedServiceConfigurationSupport {

	/* (non-Javadoc) */
	@Override
	protected Class getAnnotationType() {
		return EnableOffHeap.class;
	}

	/* (non-Javadoc) */
	@Override
	protected void registerBeanDefinitions(AnnotationMetadata importingClassMetadata,
			Map<String, Object> annotationAttributes, BeanDefinitionRegistry registry) {

		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(
			OffHeapBeanFactoryPostProcessor.class);

		builder.addConstructorArgValue(annotationAttributes.get("regionNames"));

		registry.registerBeanDefinition(generateBeanName(OffHeapBeanFactoryPostProcessor.class),
			builder.getBeanDefinition());
	}

	/* (non-Javadoc) */
	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {
		PropertiesBuilder gemfireProperties = PropertiesBuilder.create();

		gemfireProperties.setProperty("off-heap-memory-size", annotationAttributes.get("memorySize"));

		return gemfireProperties.build();
	}

	@SuppressWarnings("unused")
	protected static class OffHeapBeanFactoryPostProcessor implements BeanFactoryPostProcessor {

		protected static final Set<String> REGION_FACTORY_BEAN_TYPES = new HashSet<String>(5);

		static {
			REGION_FACTORY_BEAN_TYPES.add(ClientRegionFactoryBean.class.getName());
			REGION_FACTORY_BEAN_TYPES.add(GenericRegionFactoryBean.class.getName());
			REGION_FACTORY_BEAN_TYPES.add(LocalRegionFactoryBean.class.getName());
			REGION_FACTORY_BEAN_TYPES.add(PartitionedRegionFactoryBean.class.getName());
			REGION_FACTORY_BEAN_TYPES.add(ReplicatedRegionFactoryBean.class.getName());
		}

		private final Set<String> regionNames;

		protected OffHeapBeanFactoryPostProcessor(String[] regionNames) {
			this(CollectionUtils.asSet(ArrayUtils.nullSafeArray(regionNames, String.class)));
		}

		protected OffHeapBeanFactoryPostProcessor(Set<String> regionNames) {
			this.regionNames = CollectionUtils.nullSafeSet(regionNames);
		}

		@Override
		public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
			for (String beanName : beanFactory.getBeanDefinitionNames()) {
				BeanDefinition bean = beanFactory.getBeanDefinition(beanName);

				if (isTargetedRegionBean(beanName, bean, beanFactory)) {
					bean.getPropertyValues().addPropertyValue("offHeap", true);
				}
			}
		}

		boolean isTargetedRegionBean(String beanName, BeanDefinition bean,
				ConfigurableListableBeanFactory beanFactory) {

			return (isRegionBean(bean) && isNamedRegion(beanName, bean, beanFactory));
		}

		boolean isRegionBean(BeanDefinition bean) {
			return (bean != null && REGION_FACTORY_BEAN_TYPES.contains(bean.getBeanClassName()));
		}

		boolean isNamedRegion(String beanName, BeanDefinition bean, ConfigurableListableBeanFactory beanFactory) {
			return (CollectionUtils.isEmpty(regionNames) || CollectionUtils.containsAny(regionNames,
				getBeanNames(beanName, bean, beanFactory)));
		}

		Collection<String> getBeanNames(String beanName, BeanDefinition bean, BeanFactory beanFactory) {
			Collection<String> beanNames = new HashSet<String>();

			beanNames.add(beanName);
			Collections.addAll(beanNames, beanFactory.getAliases(beanName));

			PropertyValue regionName = bean.getPropertyValues().getPropertyValue("regionName");

			if (regionName != null) {
				Object regionNameValue = regionName.getValue();

				if (regionNameValue != null) {
					beanNames.add(regionNameValue.toString());
				}
			}

			return beanNames;
		}
	}
}
