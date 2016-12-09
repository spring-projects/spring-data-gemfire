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

import com.gemstone.gemfire.cache.query.Index;

/**
 * The {@link EnableIndexes} annotation marks a Spring {@link org.springframework.context.annotation.Configuration @Configuration}
 * annotated application class to enable the creation of GemFire/Geode Indexes based on application persistent entity
 * field/property annotations, such as the {@link @Id} and @Indexed annotations.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.IndexConfiguration
 * @see com.gemstone.gemfire.cache.query.Index
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@SuppressWarnings({ "unused" })
public @interface EnableIndexes {

	/**
	 * Determines whether all GemFire/Geode {@link Index Indexes} will be defined before created.
	 * If set to {@literal true}, then all {@link Index Indexes} are defined first and the created
	 * in a single, bulk operation, thereby improving index creation efficiency.
	 *
	 * Defaults to false.
	 */
	boolean define() default false;

}
