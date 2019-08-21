/*
 * Copyright 2010-2019 the original author or authors.
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
 */
package org.springframework.data.gemfire;

import static java.util.stream.StreamSupport.stream;
import static org.springframework.data.gemfire.util.ArrayUtils.nullSafeArray;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeCollection;
import static org.springframework.data.gemfire.util.CollectionUtils.nullSafeIterable;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.io.File;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.apache.geode.cache.DiskStore;
import org.apache.geode.cache.DiskStoreFactory;
import org.apache.geode.cache.GemFireCache;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.data.gemfire.config.annotation.DiskStoreConfigurer;
import org.springframework.data.gemfire.support.AbstractFactoryBeanSupport;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} used to create {@link DiskStore}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.DiskStore
 * @see org.apache.geode.cache.DiskStoreFactory
 * @see org.apache.geode.cache.GemFireCache
 * @see org.springframework.beans.factory.FactoryBean
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.data.gemfire.config.annotation.DiskStoreConfigurer
 * @see org.springframework.data.gemfire.support.AbstractFactoryBeanSupport
 */
@SuppressWarnings("unused")
public class DiskStoreFactoryBean extends AbstractFactoryBeanSupport<DiskStore> implements InitializingBean {

	private Boolean allowForceCompaction;
	private Boolean autoCompact;

	private DiskStore diskStore;

	private GemFireCache cache;

	private Integer compactionThreshold;
	private Integer queueSize;
	private Integer writeBufferSize;

	private Float diskUsageCriticalPercentage;
	private Float diskUsageWarningPercentage;

	private Long maxOplogSize;
	private Long timeInterval;

	private List<DiskStoreConfigurer> diskStoreConfigurers = Collections.emptyList();

	private DiskStoreConfigurer compositeDiskStoreConfigurer = (beanName, bean) ->
		nullSafeCollection(diskStoreConfigurers).forEach(diskStoreConfigurer ->
			diskStoreConfigurer.configure(beanName, bean));

	private List<DiskDir> diskDirs;

	@Override
	public void afterPropertiesSet() throws Exception {

		String diskStoreName = resolveDiskStoreName();

		applyDiskStoreConfigurers(diskStoreName);

		GemFireCache cache = resolveCache(diskStoreName);

		DiskStoreFactory diskStoreFactory = postProcess(configure(createDiskStoreFactory(cache)));

		this.diskStore = postProcess(newDiskStore(diskStoreFactory, diskStoreName));
	}

	/* (non-Javadoc) */
	private void applyDiskStoreConfigurers(String diskStoreName) {
		applyDiskStoreConfigurers(diskStoreName, getCompositeDiskStoreConfigurer());
	}

	/**
	 * Null-safe operation to apply the given array of {@link DiskStoreConfigurer DiskStoreConfigurers}
	 * to this {@link DiskStoreFactoryBean}.
	 *
	 * @param diskStoreName {@link String} containing the name of the {@link DiskStore}.
	 * @param diskStoreConfigurers array of {@link DiskStoreConfigurer DiskStoreConfigurers} applied
	 * to this {@link DiskStoreFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.DiskStoreConfigurer
	 * @see #applyDiskStoreConfigurers(String, Iterable)
	 */
	protected void applyDiskStoreConfigurers(String diskStoreName, DiskStoreConfigurer... diskStoreConfigurers) {
		applyDiskStoreConfigurers(diskStoreName,
			Arrays.asList(nullSafeArray(diskStoreConfigurers, DiskStoreConfigurer.class)));
	}

	/**
	 * Null-safe operation to apply the given {@link Iterable} of {@link DiskStoreConfigurer DiskStoreConfigurers}
	 * to this {@link DiskStoreFactoryBean}.
	 *
	 * @param diskStoreName {@link String} containing the name of the {@link DiskStore}.
	 * @param diskStoreConfigurers {@link Iterable} of {@link DiskStoreConfigurer DiskStoreConfigurers} applied
	 * to this {@link DiskStoreFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.DiskStoreConfigurer
	 */
	protected void applyDiskStoreConfigurers(String diskStoreName, Iterable<DiskStoreConfigurer> diskStoreConfigurers) {
		stream(nullSafeIterable(diskStoreConfigurers).spliterator(), false)
			.forEach(diskStoreConfigurer -> diskStoreConfigurer.configure(diskStoreName, this));
	}

	/* (non-Javadoc) */
	private GemFireCache resolveCache(String diskStoreName) {
		return Optional.ofNullable(this.cache)
			.orElseThrow(() -> newIllegalStateException("Cache is required to create DiskStore [%s]", diskStoreName));
	}

	/* (non-Javadoc) */
	final String resolveDiskStoreName() {
		return Optional.ofNullable(getBeanName()).filter(StringUtils::hasText)
			.orElse(DiskStoreFactory.DEFAULT_DISK_STORE_NAME);
	}

	/**
	 * Creates an instance of {@link DiskStoreFactory} using the given {@link GemFireCache} in order to
	 * construct, configure and initialize a new {@link DiskStore}.
	 *
	 * @param cache reference to the {@link GemFireCache} used to create the {@link DiskStoreFactory}.
	 * @return a new instance of {@link DiskStoreFactory}.
	 * @see org.apache.geode.cache.GemFireCache#createDiskStoreFactory()
	 * @see org.apache.geode.cache.DiskStoreFactory
	 */
	protected DiskStoreFactory createDiskStoreFactory(GemFireCache cache) {
		return cache.createDiskStoreFactory();
	}

	/**
	 * Configures the given {@link DiskStoreFactory} with the configuration settings present
	 * on this {@link DiskStoreFactoryBean}
	 *
	 * @param diskStoreFactory {@link DiskStoreFactory} to configure.
	 * @return the given {@link DiskStoreFactory}
	 * @see org.apache.geode.cache.DiskStoreFactory
	 */
	protected DiskStoreFactory configure(DiskStoreFactory diskStoreFactory) {

		Optional.ofNullable(this.allowForceCompaction).ifPresent(diskStoreFactory::setAllowForceCompaction);
		Optional.ofNullable(this.autoCompact).ifPresent(diskStoreFactory::setAutoCompact);
		Optional.ofNullable(this.compactionThreshold).ifPresent(diskStoreFactory::setCompactionThreshold);
		Optional.ofNullable(this.diskUsageCriticalPercentage).ifPresent(diskStoreFactory::setDiskUsageCriticalPercentage);
		Optional.ofNullable(this.diskUsageWarningPercentage).ifPresent(diskStoreFactory::setDiskUsageWarningPercentage);
		Optional.ofNullable(this.maxOplogSize).ifPresent(diskStoreFactory::setMaxOplogSize);
		Optional.ofNullable(this.queueSize).ifPresent(diskStoreFactory::setQueueSize);
		Optional.ofNullable(this.timeInterval).ifPresent(diskStoreFactory::setTimeInterval);
		Optional.ofNullable(this.writeBufferSize).ifPresent(diskStoreFactory::setWriteBufferSize);

		Optional.ofNullable(this.diskDirs).filter(diskDirs -> !CollectionUtils.isEmpty(diskDirs))
			.ifPresent(diskDirs -> {

				File[] diskDirFiles = new File[diskDirs.size()];
				int[] diskDirSizes = new int[diskDirs.size()];

				for (int index = 0; index < diskDirs.size(); index++) {
					DiskDir diskDir = diskDirs.get(index);
					diskDirFiles[index] = new File(diskDir.location);
					diskDirSizes[index] = Optional.ofNullable(diskDir.maxSize)
						.orElse(DiskStoreFactory.DEFAULT_DISK_DIR_SIZE);
				}

				diskStoreFactory.setDiskDirsAndSizes(diskDirFiles, diskDirSizes);
			});

		return diskStoreFactory;
	}

	/**
	 * Constructs a new instance of {@link DiskStore} with the given {@link String name}
	 * using the provided {@link DiskStoreFactory}
	 *
	 * @param diskStoreFactory {@link DiskStoreFactory} used to create the {@link DiskStore}.
	 * @param diskStoreName {@link String} containing the name of the new {@link DiskStore}.
	 * @return a new instance of {@link DiskStore} with the given {@link String name}.
	 * @see org.apache.geode.cache.DiskStoreFactory
	 * @see org.apache.geode.cache.DiskStore
	 */
	protected DiskStore newDiskStore(DiskStoreFactory diskStoreFactory, String diskStoreName) {
		return diskStoreFactory.create(diskStoreName);
	}

	/**
	 * Post-process the {@link DiskStoreFactory} with any custom {@link DiskStoreFactory} or {@link DiskStore}
	 * configuration settings as required by the application.
	 *
	 * @param diskStoreFactory {@link DiskStoreFactory} to process.
	 * @return the given {@link DiskStoreFactory}.
	 * @see org.apache.geode.cache.DiskStoreFactory
	 */
	protected DiskStoreFactory postProcess(DiskStoreFactory diskStoreFactory) {
		return diskStoreFactory;
	}

	/**
	 * Post-process the provided {@link DiskStore} constructed, configured and initialized
	 * by this {@link DiskStoreFactoryBean}.
	 *
	 * @param diskStore {@link DiskStore} to process.
	 * @return the given {@link DiskStore}.
	 * @see org.apache.geode.cache.DiskStore
	 */
	protected DiskStore postProcess(DiskStore diskStore) {
		return diskStore;
	}

	/**
	 * Returns a reference to the Composite {@link DiskStoreConfigurer} used to apply additional configuration
	 * to this {@link DiskStoreFactoryBean} on Spring container initialization.
	 *
	 * @return the Composite {@link DiskStoreConfigurer}.
	 * @see org.springframework.data.gemfire.config.annotation.DiskStoreConfigurer
	 */
	protected DiskStoreConfigurer getCompositeDiskStoreConfigurer() {
		return this.compositeDiskStoreConfigurer;
	}

	@Override
	public DiskStore getObject() throws Exception {
		return this.diskStore;
	}

	@Override
	@SuppressWarnings("unchecked")
	public Class<?> getObjectType() {
		return Optional.ofNullable(this.diskStore).map(DiskStore::getClass).orElse((Class) DiskStore.class);
	}

	public void setCache(GemFireCache cache) {
		this.cache = cache;
	}

	public void setAllowForceCompaction(Boolean allowForceCompaction) {
		this.allowForceCompaction = allowForceCompaction;
	}

	public void setAutoCompact(Boolean autoCompact) {
		this.autoCompact = autoCompact;
	}

	public void setCompactionThreshold(Integer compactionThreshold) {
		validateCompactionThreshold(compactionThreshold);
		this.compactionThreshold = compactionThreshold;
	}

	protected void validateCompactionThreshold(Integer compactionThreshold) {
		Assert.isTrue(compactionThreshold == null || (compactionThreshold >= 0 && compactionThreshold <= 100),
			String.format("The DiskStore's (%1$s) compaction threshold (%2$d) must be an integer value between 0 and 100 inclusive.",
				resolveDiskStoreName(), compactionThreshold));
	}

	public void setDiskDirs(List<DiskDir> diskDirs) {
		this.diskDirs = diskDirs;
	}

	/**
	 * Null-safe operation to set an array of {@link DiskStoreConfigurer DiskStoreConfigurers} used to
	 * apply additional configuration to this {@link DiskStoreFactoryBean} when using Annotation-based configuration.
	 *
	 * @param diskStoreConfigurers array of {@link DiskStoreConfigurer DiskStoreConfigurers} used to apply
	 * additional configuration to this {@link DiskStoreFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.DiskStoreConfigurer
	 * @see #setDiskStoreConfigurers(List)
	 */
	public void setDiskStoreConfigurers(DiskStoreConfigurer... diskStoreConfigurers) {
		setDiskStoreConfigurers(Arrays.asList(nullSafeArray(diskStoreConfigurers, DiskStoreConfigurer.class)));
	}

	/**
	 * Null-safe operation to set an {@link Iterable} of {@link DiskStoreConfigurer DiskStoreConfigurers}
	 * used to apply additional configuration to this {@link DiskStoreFactoryBean}
	 * when using Annotation-based configuration.
	 *
	 * @param diskStoreConfigurers {@link Iterable } of {@link DiskStoreConfigurer DiskStoreConfigurers} used to
	 * apply additional configuration to this {@link DiskStoreFactoryBean}.
	 * @see org.springframework.data.gemfire.config.annotation.DiskStoreConfigurer
	 */
	public void setDiskStoreConfigurers(List<DiskStoreConfigurer> diskStoreConfigurers) {
		this.diskStoreConfigurers = Optional.ofNullable(diskStoreConfigurers).orElseGet(Collections::emptyList);
	}

	public void setDiskUsageCriticalPercentage(Float diskUsageCriticalPercentage) {
		this.diskUsageCriticalPercentage = diskUsageCriticalPercentage;
	}

	public void setDiskUsageWarningPercentage(Float diskUsageWarningPercentage) {
		this.diskUsageWarningPercentage = diskUsageWarningPercentage;
	}

	public void setMaxOplogSize(Long maxOplogSize) {
		this.maxOplogSize = maxOplogSize;
	}

	public void setQueueSize(Integer queueSize) {
		this.queueSize = queueSize;
	}

	public void setTimeInterval(Long timeInterval) {
		this.timeInterval = timeInterval;
	}

	public void setWriteBufferSize(Integer writeBufferSize) {
		this.writeBufferSize = writeBufferSize;
	}

	public static class DiskDir {

		final Integer maxSize;
		final String location;

		public DiskDir(String location) {
			this.location = location;
			this.maxSize = null;
		}

		public DiskDir(String location, int maxSize) {
			this.location = location;
			this.maxSize = maxSize;
		}
	}
}
