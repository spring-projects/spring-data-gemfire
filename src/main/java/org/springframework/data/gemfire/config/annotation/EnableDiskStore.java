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

import static org.springframework.data.gemfire.config.annotation.DiskStoreConfiguration.DEFAULT_ALLOW_FORCE_COMPACTION;
import static org.springframework.data.gemfire.config.annotation.DiskStoreConfiguration.DEFAULT_AUTO_COMPACT;
import static org.springframework.data.gemfire.config.annotation.DiskStoreConfiguration.DEFAULT_COMPACTION_THRESHOLD;
import static org.springframework.data.gemfire.config.annotation.DiskStoreConfiguration.DEFAULT_DISK_USAGE_CRITICAL_PERCENTAGE;
import static org.springframework.data.gemfire.config.annotation.DiskStoreConfiguration.DEFAULT_DISK_USAGE_WARNING_PERCENTAGE;
import static org.springframework.data.gemfire.config.annotation.DiskStoreConfiguration.DEFAULT_MAX_OPLOG_SIZE;
import static org.springframework.data.gemfire.config.annotation.DiskStoreConfiguration.DEFAULT_QUEUE_SIZE;
import static org.springframework.data.gemfire.config.annotation.DiskStoreConfiguration.DEFAULT_TIME_INTERVAL;
import static org.springframework.data.gemfire.config.annotation.DiskStoreConfiguration.DEFAULT_WRITE_BUFFER_SIZE;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.apache.geode.cache.DiskStore;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.core.annotation.AliasFor;

/**
 * The {@link EnableDiskStore} annotation marks a Spring {@link Configuration @Configuration} annotated {@link Class}
 * to configure a single GemFire/Geode {@link org.apache.geode.cache.DiskStore} bean in the Spring application context
 * in which to persist or overflow data from 1 or more cache {@link org.apache.geode.cache.Region Regions}.
 *
 * @author John Blum
 * @see org.apache.geode.cache.DiskStore
 * @see org.apache.geode.cache.Region
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.core.annotation.AliasFor
 * @see org.springframework.data.gemfire.config.annotation.DiskStoreConfiguration
 * @see org.springframework.data.gemfire.config.annotation.DiskStoreConfigurer
 * @see org.springframework.data.gemfire.config.annotation.EnableDiskStores
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(DiskStoreConfiguration.class)
@SuppressWarnings({ "unused" })
public @interface EnableDiskStore {

	/**
	 * Name of the {@link DiskStore}.
	 *
	 * Required!
	 */
	@AliasFor(attribute = "name")
	String value() default "";

	/**
	 * Name of the {@link DiskStore}.
	 *
	 * Required!
	 *
	 * This value of this attribute is also used to resolve {@link DiskStore} specific properties defined in
	 * {@literal application.properties}.
	 */
	@AliasFor(attribute = "value")
	String name() default "";

	/**
	 * Set to true to allow disk compaction to be forced on this disk store.
	 *
	 * Defaults to {@literal false}.
	 *
	 * Use either the {@literal spring.data.gemfire.disk.store.<diskStoreName>.allow-force-compaction} property
	 * or the {@literal spring.data.gemfire.disk.store.allow-force-compaction} property
	 * in {@literal application.properties}.
	 */
	boolean allowForceCompaction() default DEFAULT_ALLOW_FORCE_COMPACTION;

	/**
	 * Set to true to automatically compact the disk files.
	 *
	 * Defaults to {@literal true}.
	 *
	 * Use either the {@literal spring.data.gemfire.disk.store.<diskStoreName>.auto-compact} property
	 * or the {@literal spring.data.gemfire.disk.store.auto-compact} property
	 * in {@literal application.properties}.
	 */
	boolean autoCompact() default DEFAULT_AUTO_COMPACT;

	/**
	 * The threshold at which an oplog will become compactable. Until it reaches this threshold the oplog
	 * will not be compacted.
	 *
	 * The threshold is a percentage in the range 0 to 100.
	 *
	 * Defaults to {@literal 50} percent.
	 *
	 * Use either the {@literal spring.data.gemfire.disk.store.<diskStoreName>.compaction-threshold} property
	 * or the {@literal spring.data.gemfire.disk.store.compaction-threshold} property
	 * in {@literal application.properties}.
	 */
	int compactionThreshold() default DEFAULT_COMPACTION_THRESHOLD;

	/**
	 * File system directory location(s) in which the {@link org.apache.geode.cache.DiskStore} files are stored.
	 *
	 * Defaults to current working directory with 2 petabytes of storage capacity maximum size.
	 *
	 * Use either the {@literal spring.data.gemfire.disk.store.<diskStoreName>.directory[#].location},
	 * {@literal spring.data.gemfire.disk.store.<diskStoreName>.directory[#].size},
	 * {@literal spring.data.gemfire.disk.store.<diskStoreName>.directory.location},
	 * {@literal spring.data.gemfire.disk.store.<diskStoreName>.directory.size} properties,
	 * or the {@literal spring.data.gemfire.disk.store.directory[#].location}
	 * {@literal spring.data.gemfire.disk.store.directory[#].size},
	 * {@literal spring.data.gemfire.disk.store.directory.location},
	 * {@literal spring.data.gemfire.disk.store.directory.size} properties,
	 * in {@literal application.properties}.
	 */
	DiskDirectory[] diskDirectories() default {};

	/**
	 * Disk usage above this threshold generates an error message and shuts down the member's cache.
	 *
	 * For example, if the threshold is set to 99%, then falling under 10 GB of free disk space on a 1 TB drive
	 * generates the error and shuts down the cache.
	 *
	 * Set to "0" (zero) to disable.
	 *
	 * Defaults to {@literal 99} percent.
	 *
	 * Use either the {@literal spring.data.gemfire.disk.store.<diskStoreName>.disk-usage-critical-percentage} property
	 * or the {@literal spring.data.gemfire.disk.store.disk-usage-critical-percentage} property
	 * in {@literal application.properties}.
	 */
	float diskUsageCriticalPercentage() default DEFAULT_DISK_USAGE_CRITICAL_PERCENTAGE;

	/**
	 * Disk usage above this threshold generates a warning message.
	 *
	 * For example, if the threshold is set to 90%, then on a 1 TB drive falling under 100 GB of free disk space
	 * generates the warning.
	 *
	 * Set to "0" (zero) to disable.
	 *
	 * Defaults to {@literal 90} percent.
	 *
	 * Use either the {@literal spring.data.gemfire.disk.store.<diskStoreName>.disk-usage-warning-percentage} property
	 * or the {@literal spring.data.gemfire.disk.store.disk-usage-warning-percentage} property
	 * in {@literal application.properties}.
	 */
	float diskUsageWarningPercentage() default DEFAULT_DISK_USAGE_WARNING_PERCENTAGE;

	/**
	 * The maximum size, in megabytes, of an oplog (operation log) file.
	 *
	 * Defaults to {@literal 1024} MB.
	 *
	 * Use either the {@literal spring.data.gemfire.disk.store.<diskStoreName>.max-oplog-size} property
	 * or the {@literal spring.data.gemfire.disk.store.max-oplog-size} property
	 * in {@literal application.properties}.
	 */
	long maxOplogSize() default DEFAULT_MAX_OPLOG_SIZE;

	/**
	 * Maximum number of operations that can be asynchronously queued to be written to disk.
	 *
	 * Defaults to {@literal 0} (unlimited).
	 *
	 * Use either the {@literal spring.data.gemfire.disk.store.<diskStoreName>.queue-size} property
	 * or the {@literal spring.data.gemfire.disk.store.queue-size} property
	 * in {@literal application.properties}.
	 */
	int queueSize() default DEFAULT_QUEUE_SIZE;

	/**
	 * The number of milliseconds that can elapse before unwritten data is written to disk.
	 *
	 * Defaults to {@literal 1000} ms.
	 *
	 * Use either the {@literal spring.data.gemfire.disk.store.<diskStoreName>.time-interval} property
	 * or the {@literal spring.data.gemfire.disk.store.time-interval} property
	 * in {@literal application.properties}.
	 */
	long timeInterval() default DEFAULT_TIME_INTERVAL;

	/**
	 * The size of the write buffer that this disk store uses when writing data to disk.
	 *
	 * Larger values may increase performance but use more memory. The disk store allocates
	 * one direct memory buffer of this size.
	 *
	 * Defaults to {@literal 32768} bytes.
	 *
	 * Use either the {@literal spring.data.gemfire.disk.store.<diskStoreName>.write-buffer-size} property
	 * or the {@literal spring.data.gemfire.disk.store.write-buffer-size} property
	 * in {@literal application.properties}.
	 */
	int writeBufferSize() default DEFAULT_WRITE_BUFFER_SIZE;

	@interface DiskDirectory {

		/**
		 * File system directory location of the {@link org.apache.geode.cache.DiskStore} files.
		 *
		 * Defaults to current working directory.
		 */
		String location() default ".";

		/**
		 * Maximum amount of space to use for the disk store, in megabytes.
		 *
		 * Defaults to {@literal 2} petabytes.
		 */
		int maxSize() default Integer.MAX_VALUE;

	}
}
