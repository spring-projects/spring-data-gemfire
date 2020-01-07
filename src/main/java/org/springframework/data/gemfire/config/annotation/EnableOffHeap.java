/*
 * Copyright 2016-2020 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.apache.geode.cache.Region;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * The {@link EnableOffHeap} annotation marks a Spring {@link Configuration @Configuration} annotated application
 * {@link Class} to configure and enable Off-Heap Memory data storage in cache {@link Region Regions}.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.OffHeapConfiguration
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(OffHeapConfiguration.class)
@UsesGemFireProperties
@SuppressWarnings("unused")
public @interface EnableOffHeap {

	/**
	 * Specifies the size of off-heap memory in megabytes (m) or gigabytes (g).
	 *
	 * For example:
	 *
	 * <pre>
	 *     <code>
	 *       off-heap-memory-size=4096m
	 *       off-heap-memory-size=120g
	 *     </code>
	 * </pre>
	 *
	 * Defaults to unset.
	 *
	 * Use the {@literal spring.data.gemfire.cache.off-heap.memory-size} property in {@literal application.properties}.
	 */
	String memorySize();

	/**
	 * Identifies all the {@link Region Regions} by name in which the Off-Heap Memory settings will be applied.
	 *
	 * Defaults to all {@link Region Regions}.
	 *
	 * Use the {@literal spring.data.gemfire.cache.off-heap.region-names} property in {@literal application.properties}.
	 */
	String[] regionNames() default {};

}
