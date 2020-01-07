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

import org.apache.geode.cache.EvictionAttributes;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.util.ObjectSizer;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.eviction.EvictionActionType;
import org.springframework.data.gemfire.eviction.EvictionPolicyType;

/**
 * The {@link EnableEviction} annotation marks a Spring {@link Configuration @Configuration} annotated class
 * to enable {@link Region} Eviction.
 *
 * @author John Blum
 * @see org.apache.geode.cache.EvictionAttributes
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.util.ObjectSizer
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.EvictionConfiguration
 * @see org.springframework.data.gemfire.eviction.EvictionActionType
 * @see org.springframework.data.gemfire.eviction.EvictionPolicyType
 * @see org.apache.geode.cache.Region
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(EvictionConfiguration.class)
@SuppressWarnings({ "unused" })
public @interface EnableEviction {

	/**
	 * Defines individual {@link Region} Eviction policies or customizes the default Eviction policy applied
	 * to all {@link Region Regions}.
	 *
	 * Defaults to empty.
	 */
	EvictionPolicy[] policies() default {};

	/**
	 * Definition for a specific Eviction policy that can be applied to 1 or more {@link Region Regions}.
	 *
	 * An Eviction policy defines the maximum (a.k.a. threshold) along with {@link ObjectSizer} used to size
	 * {@link Region} entry values and the action applied when {@link Region} entries are to be evicted.
	 *
	 * Additionally, the Eviction policy defines the algorithm used (eviction based on entry count, JVM Heap percentage
	 * or system memory size used) to determine when an Eviction should occur.
	 */
	@interface EvictionPolicy {

		/**
		 * Action to take on an {@link Region} entry when evicted.
		 *
		 * Defaults to {@link EvictionActionType#LOCAL_DESTROY}.
		 *
		 * @see org.springframework.data.gemfire.eviction.EvictionActionType
		 */
		EvictionActionType action() default EvictionActionType.LOCAL_DESTROY;

		/**
		 * Threshold applied for entry count Eviction.
		 *
		 * Defaults to {@link EvictionAttributes#DEFAULT_ENTRIES_MAXIMUM}
		 */
		int maximum() default EvictionAttributes.DEFAULT_ENTRIES_MAXIMUM;

		/**
		 * Name of a Spring bean of type {@link ObjectSizer} defined in the Spring application context
		 * used to size {@link Region} entry values.
		 *
		 * Defaults to empty.
		 *
		 * @see org.apache.geode.cache.util.ObjectSizer
		 */
		String objectSizerName() default "";

		/**
		 * Names of all the {@link Region Regions} in which this Eviction policy will be applied.
		 *
		 * Defaults to empty.
		 */
		String[] regionNames() default {};

		/**
		 * Eviction algorithm used during Eviction.
		 *
		 * Defaults to {@link EvictionPolicyType#ENTRY_COUNT}.
		 *
		 * @see org.springframework.data.gemfire.eviction.EvictionPolicyType
		 */
		EvictionPolicyType type() default EvictionPolicyType.ENTRY_COUNT;

	}
}
