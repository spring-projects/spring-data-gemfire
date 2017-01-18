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

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.context.annotation.Import;
import org.springframework.core.annotation.AliasFor;

/**
 * The {@link EnableDiskStore} annotation marks a Spring {@link org.springframework.context.annotation.Configuration @Configuration}
 * annotated application class to configure a single GemFire/Geode {@link org.apache.geode.cache.DiskStore} bean
 * in the Spring context in which to persist or overflow data from 1 or more GemFire/Geode
 * {@link org.apache.geode.cache.Region Regions}
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
	 * Name of the {@link org.apache.geode.cache.DiskStore}.
	 */
	@AliasFor(attribute = "name")
	String value() default "";

	/**
	 * Name of the {@link org.apache.geode.cache.DiskStore}.
	 */
	@AliasFor(attribute = "value")
	String name() default "";

	/**
	 * Set to true to allow disk compaction to be forced on this disk store.
	 *
	 * Default is {@literal false}.
	 */
	boolean allowForceCompaction() default false;

	/**
	 * Set to true to automatically compact the disk files.
	 *
	 * Default is {@literal false}.
	 */
	boolean autoCompact() default false;

	/**
	 * The threshold at which an oplog will become compactable. Until it reaches this threshold the oplog
	 * will not be compacted.
	 *
	 * The threshold is a percentage in the range 0 to 100.
	 *
	 * Defaults to {@literal 50} percent.
	 */
	int compactionThreshold() default 50;

	/**
	 * File system directory location(s) in which the {@link org.apache.geode.cache.DiskStore} files are stored.
	 *
	 * Defaults to current working directory with 2 petabytes of storage capacity maximum size.
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
	 */
	float diskUsageCriticalPercentage() default 99.0f;

	/**
	 * Disk usage above this threshold generates a warning message.
	 *
	 * For example, if the threshold is set to 90%, then on a 1 TB drive falling under 100 GB of free disk space
	 * generates the warning.
	 *
	 * Set to "0" (zero) to disable.
	 *
	 * Defaults to {@literal 90} percent.
	 */
	float diskUsageWarningPercentage() default 90.0f;

	/**
	 * The maximum size, in megabytes, of an oplog (operation log) file.
	 *
	 * Defaults to {@literal 1024} MB.
	 */
	long maxOplogSize() default 1024L;

	/**
	 * Maximum number of operations that can be asynchronously queued to be written to disk.
	 *
	 * Defaults to {@literal 0} (unlimited).
	 */
	int queueSize() default 0;

	/**
	 * The number of milliseconds that can elapse before unwritten data is written to disk.
	 *
	 * Defaults to {@literal 1000} ms.
	 */
	long timeInterval() default 1000L;

	/**
	 * The size of the write buffer that this disk store uses when writing data to disk.
	 *
	 * Larger values may increase performance but use more memory. The disk store allocates
	 * one direct memory buffer of this size.
	 *
	 * Defaults to {@literal 32768} bytes.
	 */
	int writeBufferSize() default 32768;

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
