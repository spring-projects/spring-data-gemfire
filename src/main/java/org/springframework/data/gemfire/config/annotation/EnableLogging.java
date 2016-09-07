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
 * The EnableLogging annotation marks a Spring {@link org.springframework.context.annotation.Configuration @Configuration}
 * annotated class to configure and enable GemFire/Geode system logging.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.LoggingConfiguration
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(LoggingConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableLogging {

	/**
	 * Maximum size in megabytes of all inactive log files combined. If this limit is exceeded, inactive log files
	 * are deleted, oldest first, until the total size is within the limit. If set to zero, disk space use is unlimited.
	 *
	 * Defaults to {@literal 0} MB.
	 */
	int logDiskSpaceLimit() default LoggingConfiguration.DEFAULT_LOG_DISK_SPACE_LIMIT;

	/**
	 * File to which a running system member writes log messages.  Logs to standard out by default.
	 *
	 * Defaults to unset.
	 */
	String logFile() default "";

	/**
	 * Maximum size in megabytes of a log file before it is closed and logging rolls on to a new (child) log file.
	 * If set to 0, log rolling is disabled.
	 *
	 * Defaults to {@literal 0} MB.
	 */
	int logFileSizeLimit() default LoggingConfiguration.DEFAULT_LOG_FILE_SIZE_LIMIT;

	/**
	 * Level of detail of the messages written to the system member’s log. Setting {@literal log-level} to one
	 * of the ordered levels causes all messages of that level and greater severity to be printed.
	 *
	 * Valid values from lowest to highest are {@literal fine}, {@literal config}, {@literal info}, {@literal warning},
	 * {@literal error}, {@literal severe}, and {@literal none}.
	 *
	 * Defaults to {@literal config}.
	 */
	String logLevel() default "config";

}
