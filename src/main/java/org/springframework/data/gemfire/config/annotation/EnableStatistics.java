/*
 * Copyright 2012 the original author or authors.
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

/**
 * The EnableStatistics annotation marks a Spring {@link org.springframework.context.annotation.Configuration @Configuration}
 * annotated class to configure and enable statistics and runtime metrics of a running GemFire/Geode system.
 *
 * Sets {@literal statistic-sampling-enabled} to {@literal true}.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.StatisticsConfiguration
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(StatisticsConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableStatistics {

	/**
	 * Maximum size (in megabytes) of all inactive statistic archive files combined. If this limit is exceeded,
	 * inactive archive files are deleted, oldest first, until the total size is within the limit. If set to zero,
	 * disk space use is unlimited.
	 *
	 * Defaults to {@literal 0} MB.
	 */
	int archiveDiskSpaceLimit() default StatisticsConfiguration.DEFAULT_ARCHIVE_DISK_SPACE_LIMIT;

	/**
	 * The file to which the running system member writes statistic samples. For example: “StatisticsArchiveFile.gfs”.
	 * An empty string disables archiving. Adding .gz suffix to the file name causes it to be compressed.
	 *
	 * Defaults to unset.
	 */
	String archiveFile() default "";

	/**
	 * The maximum size (in megabytes) of a single statistic archive file. Once this limit is exceeded,
	 * a new statistic archive file is created, and the current archive file becomes inactive. If set to zero,
	 * file size is unlimited.
	 *
	 * Defaults to {@literal 0} MB.
	 */
	int archiveFileSizeLimit() default StatisticsConfiguration.DEFAULT_ARCHIVE_FILE_SIZE_LIMIT;

	/**
	 * Boolean instructing the system to track time-based statistics for the distributed system and caching.
	 * Disabled by default for performance reasons and not recommended for production environments.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean enableTimeStatistics() default StatisticsConfiguration.DEFAULT_ENABLE_TIME_STATISTICS;

	/**
	 * How often to sample statistics, in milliseconds.
	 *
	 * Valid values are in the range 100..60000.
	 *
	 * Defaults to {@literal 1000} milliseconds.
	 */
	long sampleRate() default StatisticsConfiguration.DEFAULT_STATISTIC_SAMPLE_RATE;

}
