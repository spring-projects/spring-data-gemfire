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
 * The EnableOffHeap annotation marks a Spring {@link org.springframework.context.annotation.Configuration @Configuration}
 * annotated class to configure and enable Apache Geode Off-Heap Memory support and data storage
 * in Geode's cache {@link com.gemstone.gemfire.cache.Region Regions}.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.OffHeapConfiguration
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(OffHeapConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableOffHeap {

	/**
	 * Specifies the size of off-heap memory in megabytes (m) or gigabytes (g). For example:
	 *
	 * <pre>
	 *     <code>
	 *       off-heap-memory-size=4096m
	 *       off-heap-memory-size=120g
	 *     </code>
	 * </pre>
	 *
	 * Defaults to unset.
	 */
	String memorySize();

	/**
	 * Idenfitied the Regions by name in which the off-heap memory setting will be applied.
	 *
	 * Defaults to all Regions.
	 */
	String[] regionNames() default {};

}
