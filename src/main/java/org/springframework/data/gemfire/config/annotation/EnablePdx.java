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

/**
 * The EnablePdx annotation marks a Spring {@link org.springframework.context.annotation.Configuration @Configuration}
 * annotated class to enable the GemFire PDX features and functionality in a GemFire server/data node
 * or GemFire cache client application.
 *
 * @author John Blum
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@SuppressWarnings("unused")
public @interface EnablePdx {

	/**
	 * Configures the disk store that is used for PDX meta data.
	 */
	String diskStoreName() default "";

	/**
	 * Configures whether pdx ignores fields that were unread during deserialization.
	 *
	 * Default is {@literal false}.
	 */
	boolean ignoreUnreadFields() default false;

	/**
	 * Configures whether the type metadata for PDX objects is persisted to disk.
	 *
	 * Default is {@literal false}.
	 */
	boolean persistent() default false;

	/**
	 * Configures the object preference to {@link com.gemstone.gemfire.pdx.PdxInstance} type or {@link Object}.
	 *
	 * Default is {@literal false}.
	 */
	boolean readSerialized() default false;

	/**
	 * Configures the PDX serializer to be used by the cache to serialize object data.
	 */
	String serializerBeanName() default "";

}
