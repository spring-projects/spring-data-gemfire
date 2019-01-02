/*
 * Copyright 2016-2019 the original author or authors.
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
import java.util.Arrays;
import java.util.List;

import org.springframework.core.annotation.AliasFor;

/**
 * {@link Annotation} defining the {@link Region} in which the application persistent entity will be stored.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see org.apache.geode.cache.Region
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@SuppressWarnings("unused")
public @interface Region {

	@SuppressWarnings("unchecked")
	List<Class<? extends Annotation>> REGION_ANNOTATION_TYPES =
		Arrays.asList(ClientRegion.class, LocalRegion.class, PartitionRegion.class, ReplicateRegion.class, Region.class);

	/**
	 * Name, or fully-qualified bean name of the {@link org.apache.geode.cache.Region}
	 * in which the application persistent entity will be stored (e.g. "Users", or "/Local/Admin/Users").
	 *
	 * Defaults to simple name of the application persistent entity defined by {@link java.lang.Class#getSimpleName()}.
	 *
	 * @return the name or fully-qualified path of the {@link Region} in which the application persistent entity
	 * will be stored.
	 */
	@AliasFor(attribute = "value")
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
	@AliasFor(attribute = "name")
	String value() default "";

	/**
	 * Determines whether an entity annotated with this Region annotation will ignore any existing Region definition
	 * identified by the given {@link #name()} for this entity.
	 *
	 * Defaults to {@literal true}.
	 */
	boolean ignoreIfExists() default true;

}
