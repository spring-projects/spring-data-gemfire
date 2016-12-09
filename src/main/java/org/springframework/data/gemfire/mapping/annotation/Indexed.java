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

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.apache.geode.cache.query.Index;
import org.springframework.core.annotation.AliasFor;
import org.springframework.data.gemfire.IndexType;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.gemfire.mapping.GemfirePersistentProperty;

/**
 * The {@link Indexed} annotation is used to index a {@link GemfirePersistentEntity} {@link GemfirePersistentProperty},
 * which creates a GemFire/Geode {@link Index} on a GemFire/Geode {@link org.apache.geode.cache.Region}.
 *
 * @author John Blum
 * @see org.springframework.core.annotation.AliasFor
 * @see org.springframework.data.gemfire.IndexType
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.query.Index
 * @since 1.9.0
 */
@Target({ ElementType.FIELD, ElementType.METHOD })
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@SuppressWarnings({ "unused" })
public @interface Indexed {

	/**
	 * Name of the Index.
	 */
	@AliasFor(attribute = "name")
	String value() default "";

	/**
	 * Name of the Index.
	 */
	@AliasFor(attribute = "value")
	String name() default "";

	/**
	 * Expression to index.
	 */
	String expression() default "";

	/**
	 * The GemFire/Geode {@link org.apache.geode.cache.Region} on which the Index is created.
	 */
	String from() default "";

	/**
	 * Type of Index to create.
	 *
	 * Defaults to {@link IndexType#HASH}.
	 */
	IndexType type() default IndexType.HASH;

}
