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

/**
 * The {@link EnableDiskStores} annotation marks a Spring {@link org.springframework.context.annotation.Configuration @Configuration}
 * annotated application class to configure 1 or more GemFire/Geode {@link org.apache.geode.cache.DiskStore} beans
 * in the Spring context in which to persist or overflow data from 1 or more GemFire/Geode
 * {@link org.apache.geode.cache.Region Regions}
 *
 * @author John Blum
 * @see org.apache.geode.cache.DiskStore
 * @see org.apache.geode.cache.Region
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.DiskStoresConfiguration
 * @see org.springframework.data.gemfire.config.annotation.DiskStoreConfigurer
 * @see org.springframework.data.gemfire.config.annotation.EnableDiskStore
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(DiskStoresConfiguration.class)
@SuppressWarnings({ "unused" })
public @interface EnableDiskStores {

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
	 * The maximum size, in megabytes, of an oplog (operation log) file.
	 *
	 * Defaults to {@literal 1024} MB.
	 */
	long maxOplogSize() default 1024L;

	/**
	 * Defines 1 or more GemFire/Geode {@link org.apache.geode.cache.DiskStore DiskStores}.
	 */
	EnableDiskStore[] diskStores();

}
