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

import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeMap;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.ListableBeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.ManagedList;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.DiskStoreFactoryBean;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.gemfire.util.ArrayUtils;

/**
 * The {@link DiskStoreConfiguration} class is a Spring {@link ImportBeanDefinitionRegistrar} used to register
 * a GemFire/Geode {@link org.apache.geode.cache.DiskStore} bean definition.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.data.gemfire.DiskStoreFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.EnableDiskStore
 * @see org.springframework.data.gemfire.config.annotation.EnableDiskStores
 * @see org.springframework.data.gemfire.config.annotation.DiskStoreConfigurer
 * @see org.apache.geode.cache.DiskStore
 * @since 1.9.0
 */
public class DiskStoreConfiguration implements BeanFactoryAware, ImportBeanDefinitionRegistrar {

	private BeanFactory beanFactory;

	@Autowired(required = false)
	private List<DiskStoreConfigurer> diskStoreConfigurers = Collections.emptyList();

	/**
	 * @inheritDoc
	 */
	@Override
	public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {

		if (importingClassMetadata.hasAnnotation(EnableDiskStore.class.getName())) {

			AnnotationAttributes enableDiskStoreAttributes = AnnotationAttributes.fromMap(
				importingClassMetadata.getAnnotationAttributes(EnableDiskStore.class.getName()));

			registerDiskStoreBeanDefinition(importingClassMetadata, enableDiskStoreAttributes, registry);
		}
	}

	/* (non-Javadoc) */
	protected void registerDiskStoreBeanDefinition(AnnotationMetadata importingClassMetadata,
			AnnotationAttributes enableDiskStoreAttributes, BeanDefinitionRegistry registry) {

		BeanDefinitionBuilder diskStoreFactoryBeanBuilder =
			BeanDefinitionBuilder.genericBeanDefinition(DiskStoreFactoryBean.class);

		String diskStoreName = enableDiskStoreAttributes.getString("name");

		diskStoreFactoryBeanBuilder.addPropertyValue("beanName", diskStoreName);

		diskStoreFactoryBeanBuilder.addPropertyReference("cache", GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME);

		diskStoreFactoryBeanBuilder.addPropertyValue("diskStoreConfigurers", resolveDiskStoreConfigurers());

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "allowForceCompaction",
			enableDiskStoreAttributes.getBoolean("allowForceCompaction"), false);

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "autoCompact",
			enableDiskStoreAttributes.getBoolean("autoCompact"), false);

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "compactionThreshold",
			enableDiskStoreAttributes.<Integer>getNumber("compactionThreshold"), 50);

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "diskUsageCriticalPercentage",
			enableDiskStoreAttributes.<Float>getNumber("diskUsageCriticalPercentage"), 99.0f);

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "diskUsageWarningPercentage",
			enableDiskStoreAttributes.<Float>getNumber("diskUsageWarningPercentage"), 90.0f);

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "maxOplogSize",
			enableDiskStoreAttributes.<Long>getNumber("maxOplogSize"), 1024L);

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "queueSize",
			enableDiskStoreAttributes.<Integer>getNumber("queueSize"), 0);

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "timeInterval",
			enableDiskStoreAttributes.<Long>getNumber("timeInterval"), 1000L);

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "writeBufferSize",
			enableDiskStoreAttributes.<Integer>getNumber("writeBufferSize"), 32768);

		parseDiskStoreDiskDirectories(importingClassMetadata, enableDiskStoreAttributes, diskStoreFactoryBeanBuilder);

		registry.registerBeanDefinition(diskStoreName, diskStoreFactoryBeanBuilder.getBeanDefinition());
	}

	/* (non-Javadoc) */
	private List<DiskStoreConfigurer> resolveDiskStoreConfigurers() {

		return Optional.ofNullable(this.diskStoreConfigurers)
			.filter(diskStoreConfigurers -> !diskStoreConfigurers.isEmpty())
			.orElseGet(() ->
				Optional.of(this.beanFactory)
					.filter(beanFactory -> beanFactory instanceof ListableBeanFactory)
					.map(beanFactory -> {
						Map<String, DiskStoreConfigurer> beansOfType = ((ListableBeanFactory) beanFactory)
							.getBeansOfType(DiskStoreConfigurer.class, true, true);

						return nullSafeMap(beansOfType).values().stream().collect(Collectors.toList());
					})
					.orElseGet(Collections::emptyList)
			);
	}

	/* (non-Javadoc) */
	protected BeanDefinitionBuilder parseDiskStoreDiskDirectories(AnnotationMetadata importingClassMetadata,
			AnnotationAttributes enableDiskStoreAttributes, BeanDefinitionBuilder diskStoreBeanFactoryBuilder) {

		AnnotationAttributes[] diskDirectories = ArrayUtils.nullSafeArray(
			enableDiskStoreAttributes.getAnnotationArray("diskDirectories"), AnnotationAttributes.class);

		ManagedList<BeanDefinition> diskDirectoryBeans = new ManagedList<BeanDefinition>(diskDirectories.length);

		for (AnnotationAttributes diskDirectoryAttributes : diskDirectories) {
			BeanDefinitionBuilder diskDirectoryBuilder =
				BeanDefinitionBuilder.genericBeanDefinition(DiskStoreFactoryBean.DiskDir.class);

			diskDirectoryBuilder.addConstructorArgValue(diskDirectoryAttributes.getString("location"));
			diskDirectoryBuilder.addConstructorArgValue(diskDirectoryAttributes.<Integer>getNumber("maxSize"));

			diskDirectoryBeans.add(diskDirectoryBuilder.getBeanDefinition());
		}

		if (!diskDirectoryBeans.isEmpty()) {
			diskStoreBeanFactoryBuilder.addPropertyValue("diskDirs", diskDirectoryBeans);
		}

		return diskStoreBeanFactoryBuilder;
	}

	/* (non-Javadoc) */
	private <T> BeanDefinitionBuilder setPropertyValueIfNotDefault(BeanDefinitionBuilder beanDefinitionBuilder,
			String propertyName, T value, T defaultValue) {

		return (value != null && !value.equals(defaultValue) ?
			beanDefinitionBuilder.addPropertyValue(propertyName, value) : beanDefinitionBuilder);
	}

	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}
}
