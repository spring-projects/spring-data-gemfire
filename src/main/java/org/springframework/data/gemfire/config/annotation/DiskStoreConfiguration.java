/*
 * Copyright 2016-2018 the original author or authors.
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

import static java.util.Arrays.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeMap;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import org.apache.geode.cache.DiskStore;
import org.apache.geode.cache.DiskStoreFactory;
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
import org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.util.StringUtils;

/**
 * The {@link DiskStoreConfiguration} class is a Spring {@link ImportBeanDefinitionRegistrar} used to register
 * a GemFire/Geode {@link org.apache.geode.cache.DiskStore} bean definition.
 *
 * @author John Blum
 * @see org.apache.geode.cache.DiskStore
 * @see org.springframework.beans.factory.config.BeanDefinition
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.data.gemfire.DiskStoreFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.EnableDiskStore
 * @see org.springframework.data.gemfire.config.annotation.EnableDiskStores
 * @see org.springframework.data.gemfire.config.annotation.DiskStoreConfigurer
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport
 * @since 1.9.0
 */
public class DiskStoreConfiguration extends AbstractAnnotationConfigSupport
		implements ImportBeanDefinitionRegistrar {

	protected static final boolean DEFAULT_ALLOW_FORCE_COMPACTION = DiskStoreFactory.DEFAULT_ALLOW_FORCE_COMPACTION;
	protected static final boolean DEFAULT_AUTO_COMPACT = DiskStoreFactory.DEFAULT_AUTO_COMPACT;

	protected static final float DEFAULT_DISK_USAGE_CRITICAL_PERCENTAGE =
		DiskStoreFactory.DEFAULT_DISK_USAGE_CRITICAL_PERCENTAGE;

	protected static final float DEFAULT_DISK_USAGE_WARNING_PERCENTAGE =
		DiskStoreFactory.DEFAULT_DISK_USAGE_WARNING_PERCENTAGE;

	protected static final int DEFAULT_COMPACTION_THRESHOLD = DiskStoreFactory.DEFAULT_COMPACTION_THRESHOLD;
	protected static final int DEFAULT_QUEUE_SIZE = DiskStoreFactory.DEFAULT_QUEUE_SIZE;
	protected static final int DEFAULT_WRITE_BUFFER_SIZE = DiskStoreFactory.DEFAULT_WRITE_BUFFER_SIZE;

	protected static final long DEFAULT_MAX_OPLOG_SIZE = 1024L;
	protected static final long DEFAULT_TIME_INTERVAL = DiskStoreFactory.DEFAULT_TIME_INTERVAL;

	@Autowired(required = false)
	private List<DiskStoreConfigurer> diskStoreConfigurers = Collections.emptyList();

	/**
	 * Returns the {@link DiskStore} {@link java.lang.annotation.Annotation} type specified in configuration.
	 *
	 * @return the {@link DiskStore} {@link java.lang.annotation.Annotation} type specified in configuration.
	 * @see org.springframework.data.gemfire.config.annotation.EnableDiskStores
	 * @see org.springframework.data.gemfire.config.annotation.EnableDiskStore
	 */
	@Override
	protected Class getAnnotationType() {
		return EnableDiskStore.class;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {

		if (importingClassMetadata.hasAnnotation(getAnnotationTypeName())) {

			AnnotationAttributes enableDiskStoreAttributes =
				AnnotationAttributes.fromMap(importingClassMetadata.getAnnotationAttributes(getAnnotationTypeName()));

			registerDiskStoreBeanDefinition(enableDiskStoreAttributes, registry);
		}
	}

	/* (non-Javadoc) */
	protected void registerDiskStoreBeanDefinition(AnnotationAttributes enableDiskStoreAttributes,
			BeanDefinitionRegistry registry) {

		BeanDefinitionBuilder diskStoreFactoryBeanBuilder =
			BeanDefinitionBuilder.genericBeanDefinition(DiskStoreFactoryBean.class);

		String diskStoreName = enableDiskStoreAttributes.getString("name");

		diskStoreFactoryBeanBuilder.addPropertyValue("beanName", diskStoreName);

		diskStoreFactoryBeanBuilder.addPropertyReference("cache", GemfireConstants.DEFAULT_GEMFIRE_CACHE_NAME);

		diskStoreFactoryBeanBuilder.addPropertyValue("diskStoreConfigurers", resolveDiskStoreConfigurers());

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "allowForceCompaction",
			enableDiskStoreAttributes.getBoolean("allowForceCompaction"), DEFAULT_ALLOW_FORCE_COMPACTION);

		Optional.ofNullable(resolveProperty(namedDiskStoreProperty(diskStoreName, "allow-force-compaction"),
				resolveProperty(diskStoreProperty("allow-force-compaction"), (Boolean) null)))
			.ifPresent(allowForceCompaction ->
				diskStoreFactoryBeanBuilder.addPropertyValue("allowForceCompaction", allowForceCompaction));

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "autoCompact",
			enableDiskStoreAttributes.getBoolean("autoCompact"), DEFAULT_AUTO_COMPACT);

		Optional.ofNullable(resolveProperty(namedDiskStoreProperty(diskStoreName, "auto-compact"),
				resolveProperty(diskStoreProperty("auto-compact"), (Boolean) null)))
			.ifPresent(autoCompact -> diskStoreFactoryBeanBuilder.addPropertyValue("autoCompact", autoCompact));

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "compactionThreshold",
			enableDiskStoreAttributes.<Integer>getNumber("compactionThreshold"),
				DEFAULT_COMPACTION_THRESHOLD);

		Optional.ofNullable(resolveProperty(namedDiskStoreProperty(diskStoreName, "compaction-threshold"),
				resolveProperty(diskStoreProperty("compaction-threshold"), (Integer) null)))
			.ifPresent(compactionThreshold ->
				diskStoreFactoryBeanBuilder.addPropertyValue("compactionThreshold", compactionThreshold));

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "diskUsageCriticalPercentage",
			enableDiskStoreAttributes.<Float>getNumber("diskUsageCriticalPercentage"),
				DEFAULT_DISK_USAGE_CRITICAL_PERCENTAGE);

		Optional.ofNullable(resolveProperty(namedDiskStoreProperty(diskStoreName, "disk-usage-critical-percentage"),
				resolveProperty(diskStoreProperty("disk-usage-critical-percentage"), (Float) null)))
			.ifPresent(diskUsageCriticalPercentage ->
				diskStoreFactoryBeanBuilder.addPropertyValue("diskUsageCriticalPercentage", diskUsageCriticalPercentage));

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "diskUsageWarningPercentage",
			enableDiskStoreAttributes.<Float>getNumber("diskUsageWarningPercentage"),
				DEFAULT_DISK_USAGE_WARNING_PERCENTAGE);

		Optional.ofNullable(resolveProperty(namedDiskStoreProperty(diskStoreName, "disk-usage-warning-percentage"),
				resolveProperty(diskStoreProperty("disk-usage-warning-percentage"), (Float) null)))
			.ifPresent(diskUsageWarningPercentage ->
				diskStoreFactoryBeanBuilder.addPropertyValue("diskUsageWarningPercentage", diskUsageWarningPercentage));

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "maxOplogSize",
			enableDiskStoreAttributes.<Long>getNumber("maxOplogSize"), DEFAULT_MAX_OPLOG_SIZE);

		Optional.ofNullable(resolveProperty(namedDiskStoreProperty(diskStoreName, "max-oplog-size"),
				resolveProperty(diskStoreProperty("max-oplog-size"), (Long) null)))
			.ifPresent(maxOplogSize ->
				diskStoreFactoryBeanBuilder.addPropertyValue("maxOplogSize", maxOplogSize));

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "queueSize",
			enableDiskStoreAttributes.<Integer>getNumber("queueSize"), DEFAULT_QUEUE_SIZE);

		Optional.ofNullable(resolveProperty(namedDiskStoreProperty(diskStoreName, "queue-size"),
				resolveProperty(diskStoreProperty("queue-size"), (Integer) null)))
			.ifPresent(queueSize ->
				diskStoreFactoryBeanBuilder.addPropertyValue("queueSize", queueSize));

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "timeInterval",
			enableDiskStoreAttributes.<Long>getNumber("timeInterval"), DEFAULT_TIME_INTERVAL);

		Optional.ofNullable(resolveProperty(namedDiskStoreProperty(diskStoreName, "time-interval"),
				resolveProperty(diskStoreProperty("time-interval"), (Long) null)))
			.ifPresent(timeInterval ->
				diskStoreFactoryBeanBuilder.addPropertyValue("timeInterval", timeInterval));

		setPropertyValueIfNotDefault(diskStoreFactoryBeanBuilder, "writeBufferSize",
			enableDiskStoreAttributes.<Integer>getNumber("writeBufferSize"), DEFAULT_WRITE_BUFFER_SIZE);

		Optional.ofNullable(resolveProperty(namedDiskStoreProperty(diskStoreName, "write-buffer-size"),
				resolveProperty(diskStoreProperty("write-buffer-size"), (Integer) null)))
			.ifPresent(writeBufferSize ->
				diskStoreFactoryBeanBuilder.addPropertyValue("writeBufferSize", writeBufferSize));

		resolveDiskStoreDirectories(diskStoreName, enableDiskStoreAttributes, diskStoreFactoryBeanBuilder);

		registry.registerBeanDefinition(diskStoreName, diskStoreFactoryBeanBuilder.getBeanDefinition());
	}

	/* (non-Javadoc) */
	private List<DiskStoreConfigurer> resolveDiskStoreConfigurers() {

		return Optional.ofNullable(this.diskStoreConfigurers)
			.filter(diskStoreConfigurers -> !diskStoreConfigurers.isEmpty())
			.orElseGet(() ->
				Optional.of(this.getBeanFactory())
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
	protected BeanDefinitionBuilder resolveDiskStoreDirectories(String diskStoreName,
			AnnotationAttributes enableDiskStoreAttributes, BeanDefinitionBuilder diskStoreFactoryBeanBuilder) {

		ManagedList<BeanDefinition> diskStoreDirectoryBeans = new ManagedList<>();

		String diskStoreDirectoryLocation =
			resolveProperty(diskStoreProperty("directory.location"), String.class);

		String namedDiskStoreDirectoryLocation =
			resolveProperty(namedDiskStoreProperty(diskStoreName, "directory.location"), String.class);

		Integer diskStoreDirectorySize =
			resolveProperty(diskStoreProperty("directory.size"), Integer.class);

		Integer namedDiskStoreDirectorySize =
			resolveProperty(namedDiskStoreProperty(diskStoreName, "directory.size"), Integer.class);

		List<String> namedDiskStoreDirectoryLocationProperties = arrayOfPropertyNamesFor(
			namedDiskStoreProperty(diskStoreName, "directory"), "location");

		if (!namedDiskStoreDirectoryLocationProperties.isEmpty()) {

			AtomicInteger index = new AtomicInteger(0);

			namedDiskStoreDirectoryLocationProperties.forEach(property -> {

				String location = requireProperty(property, String.class);

				Integer maxSize = resolveProperty(asArrayProperty(
					namedDiskStoreProperty(diskStoreName, "directory"), index.getAndIncrement(),
						"size"), Integer.class);

				maxSize = resolveDiskStoreDirectorySize(maxSize, namedDiskStoreDirectorySize, diskStoreDirectorySize);

				diskStoreDirectoryBeans.add(newDiskStoreDirectoryBean(location, maxSize));
			});
		}
		else if (Optional.ofNullable(namedDiskStoreDirectoryLocation).filter(StringUtils::hasText).isPresent()) {
			diskStoreDirectoryBeans.add(newDiskStoreDirectoryBean(namedDiskStoreDirectoryLocation,
				resolveDiskStoreDirectorySize(namedDiskStoreDirectorySize, diskStoreDirectorySize)));
		}
		else {
			List<String> diskStoreDirectoryLocationProperties =
				arrayOfPropertyNamesFor(diskStoreProperty("directory"), "location");

			if (!diskStoreDirectoryLocationProperties.isEmpty()) {

				AtomicInteger index = new AtomicInteger(0);

				diskStoreDirectoryLocationProperties.forEach(property -> {

					String location = requireProperty(property, String.class);

					Integer maxSize = resolveProperty(asArrayProperty(diskStoreProperty("directory"),
						index.getAndIncrement(), "size"), Integer.class);

					maxSize = resolveDiskStoreDirectorySize(namedDiskStoreDirectorySize, maxSize,
						diskStoreDirectorySize);

					diskStoreDirectoryBeans.add(newDiskStoreDirectoryBean(location, maxSize));
				});
			}
			else if (Optional.ofNullable(diskStoreDirectoryLocation).filter(StringUtils::hasText).isPresent()) {
				diskStoreDirectoryBeans.add(newDiskStoreDirectoryBean(diskStoreDirectoryLocation,
					resolveDiskStoreDirectorySize(namedDiskStoreDirectorySize, diskStoreDirectorySize)));
			}
			else {
				diskStoreDirectoryBeans.addAll(parseDiskStoreDirectories(enableDiskStoreAttributes));
			}
		}

		Optional.of(diskStoreDirectoryBeans)
			.filter(beans -> !beans.isEmpty())
			.ifPresent(beans ->  diskStoreFactoryBeanBuilder.addPropertyValue("diskDirs", diskStoreDirectoryBeans));

		return diskStoreFactoryBeanBuilder;
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unused")
	private String resolveDiskStoreDirectoryLocation(String... locations) {

		return stream(nullSafeArray(locations, String.class))
			.filter(StringUtils::hasText)
			.findFirst()
			.orElse(".");
	}

	/* (non-Javadoc) */
	private Integer resolveDiskStoreDirectorySize(Integer... sizes) {

		return stream(nullSafeArray(sizes, Integer.class))
			.filter(Objects::nonNull)
			.findFirst()
			.orElse(Integer.MAX_VALUE);
	}

	/* (non-Javadoc) */
	protected ManagedList<BeanDefinition> parseDiskStoreDirectories(AnnotationAttributes enableDiskStoreAttributes) {

		AnnotationAttributes[] diskDirectories =
			enableDiskStoreAttributes.getAnnotationArray("diskDirectories");

		ManagedList<BeanDefinition> diskDirectoryBeans = new ManagedList<>(diskDirectories.length);

		stream(nullSafeArray(diskDirectories, AnnotationAttributes.class)).forEach(diskDirectoryAttributes ->
			diskDirectoryBeans.add(newDiskStoreDirectoryBean(diskDirectoryAttributes.getString("location"),
				diskDirectoryAttributes.getNumber("maxSize"))));

		return diskDirectoryBeans;
	}

	/* (non-Javadoc) */
	private BeanDefinition newDiskStoreDirectoryBean(String location, Integer maxSize) {

		BeanDefinitionBuilder diskDirectoryBuilder =
			BeanDefinitionBuilder.genericBeanDefinition(DiskStoreFactoryBean.DiskDir.class);

		diskDirectoryBuilder.addConstructorArgValue(location);
		diskDirectoryBuilder.addConstructorArgValue(maxSize);

		return diskDirectoryBuilder.getBeanDefinition();
	}

	/* (non-Javadoc) */
	private <T> BeanDefinitionBuilder setPropertyValueIfNotDefault(BeanDefinitionBuilder beanDefinitionBuilder,
			String propertyName, T value, T defaultValue) {

		return (value != null && !value.equals(defaultValue) ?
			beanDefinitionBuilder.addPropertyValue(propertyName, value) : beanDefinitionBuilder);
	}
}
