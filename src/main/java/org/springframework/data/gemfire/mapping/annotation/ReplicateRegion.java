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

package org.springframework.data.gemfire.mapping.annotation;

import java.lang.annotation.Annotation;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.core.annotation.AliasFor;
import org.springframework.data.gemfire.ScopeType;

/**
 * {@link Annotation} defining the Replicate {@link Region} in which the application persistent entity will be stored.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.EnableEntityDefinedRegions
 * @see org.springframework.data.gemfire.config.annotation.EntityDefinedRegionsConfiguration
 * @see org.springframework.data.gemfire.mapping.annotation.Region
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Region
@SuppressWarnings("unused")
public @interface ReplicateRegion {

	/**
	 * Name, or fully-qualified bean name of the {@link org.apache.geode.cache.Region}
	 * in which the application persistent entity will be stored (e.g. "Users", or "/Local/Admin/Users").
	 *
	 * Defaults to simple name of the application persistent entity defined by {@link java.lang.Class#getSimpleName()}.
	 *
	 * @return the name or fully-qualified path of the {@link Region} in which the application persistent entity
	 * will be stored.
	 */
	@AliasFor(annotation = Region.class, attribute = "name")
	String name() default "";

	/**
	 * Name, or fully-qualified bean name of the {@link org.apache.geode.cache.Region}
	 * in which the application persistent entity will be stored (e.g. "Users", or "/Local/Admin/Users").
	 *
	 * Defaults to simple name of the application persistent entity defined by {@link java.lang.Class#getSimpleName()}.
	 *
	 * @return the name or fully-qualified path of the {@link Region} in which the application persistent entity
	 * will be stored.
	 */
	@AliasFor(annotation = Region.class, attribute = "value")
	String value() default "";

	/**
	 * Name of the {@link org.apache.geode.cache.DiskStore} in which this persistent entity's data is overflowed
	 * and/or persisted.
	 *
	 * Maybe the name of a Spring bean defined in the Spring context.
	 *
	 * Defaults to unset.
	 */
	String diskStoreName() default "";

	/**
	 * Determines whether disk-based operations (used in overflow and persistence) are synchronous or asynchronous.
	 *
	 * Defaults to {@literal synchronous}.
	 */
	boolean diskSynchronous() default true;

	/**
	 * Determines whether an entity annotated with this Region annotation will ignore any existing Region definition
	 * identified by the given {@link #name()} for this entity.
	 *
	 * Defaults to {@literal true}.
	 */
	boolean ignoreIfExists() default true;

	/**
	 * Determines whether this {@link org.apache.geode.cache.Region Region's} data access operations participates in
	 * any existing, Global JTA transaction in progress.
	 *
	 * Defaults to {@literal false} (will NOT ignore JTA).
	 */
	boolean ignoreJta() default false;

	/**
	 * Determines whether this persistent entity's {@link org.apache.geode.cache.Region} is persistent,
	 * storing data to disk.
	 *
	 * Note, this setting independent of whether or not the {@link org.apache.geode.cache.Region} associated
	 * with this persistent entity overflows data to disk during eviction due to entry/heap/memory constraints.
	 *
	 * A {@link org.apache.geode.cache.Region} can also be persistent without an explicit
	 * {@link org.apache.geode.cache.DiskStore} defined; in that case, GemFire/Geode writes to the "DEFAULT"
	 * {@link org.apache.geode.cache.DiskStore}.
	 *
	 * Defaults to {@literal false}.
	 *
	 * @see org.apache.geode.cache.DataPolicy
	 */
	boolean persistent() default false;

	/**
	 * Defines the {@link org.apache.geode.cache.Scope} used by this persistent entity's
	 * {@link org.apache.geode.cache.DataPolicy#REPLICATE} {@link org.apache.geode.cache.Region} to
	 * acknowledge messages sent between peers.
	 *
	 * Defaults to {@link ScopeType#DISTRIBUTED_NO_ACK}
	 */
	ScopeType scope() default ScopeType.DISTRIBUTED_NO_ACK;

}
