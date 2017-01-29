/*
 * Copyright 2016-2018 the original author or authors.
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

import org.apache.geode.cache.lucene.LuceneIndex;
import org.springframework.core.annotation.AliasFor;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.gemfire.mapping.GemfirePersistentProperty;

/**
 * The {@link LuceneIndexed} annotation is used to index a {@link GemfirePersistentEntity}
 * {@link GemfirePersistentProperty}, creating a GemFire/Geode {@link LuceneIndex}
 * on a GemFire/Geode {@link org.apache.geode.cache.Region}.
 *
 * @author John Blum
 * @see org.springframework.core.annotation.AliasFor
 * @see org.springframework.data.gemfire.IndexType
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.lucene.LuceneIndex
 * @since 1.1.0
 */
@Target({ ElementType.FIELD, ElementType.METHOD })
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@SuppressWarnings("unused")
public @interface LuceneIndexed {

	/**
	 * {@link String Name} of the {@link LuceneIndex}.
	 *
	 * @return a {@link String} containing the name of the {@link LuceneIndex}
	 */
	@AliasFor(attribute = "name")
	String value() default "";

	/**
	 * {@link String Name} of the {@link LuceneIndex}.
	 *
	 * @return a {@link String} containing the name of the {@link LuceneIndex}
	 */
	@AliasFor(attribute = "value")
	String name() default "";

	/**
	 * Determine whether the {@link LuceneIndex} should be destroy when the application shutsdown.
	 *
	 * Default is {@literal false}.
	 */
	boolean destroy() default false;

}
