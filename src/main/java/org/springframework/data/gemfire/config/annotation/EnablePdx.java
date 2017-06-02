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

import org.springframework.context.annotation.Configuration;
import org.springframework.core.type.AnnotationMetadata;

/**
 * The {@link EnablePdx} annotation marks a Spring {@link Configuration @Configuration} annotated {@link Class}
 * to enable the Pivotal GemFire/Apache Geode PDX features and functionality in this peer cache, cluster member
 * or cache client application.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see org.springframework.data.gemfire.config.annotation.AbstractCacheConfiguration#configurePdx(AnnotationMetadata)
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
	 *
	 * Use the {@literal spring.data.gemfire.pdx.disk-store-name} property in {@literal application.properties}.
	 */
	String diskStoreName() default "";

	/**
	 * Configures whether pdx ignores fields that were unread during deserialization.
	 *
	 * Default is {@literal false}.
	 *
	 * Use the {@literal spring.data.gemfire.pdx.ignore-unread-fields} property in {@literal application.properties}.
	 */
	boolean ignoreUnreadFields() default false;

	/**
	 * Configures whether the type metadata for PDX objects is persisted to disk.
	 *
	 * Default is {@literal false}.
	 *
	 * Use the {@literal spring.data.gemfire.pdx.persistent} property in {@literal application.properties}.
	 */
	boolean persistent() default false;

	/**
	 * Configures the object preference to {@link org.apache.geode.pdx.PdxInstance} type or {@link Object}.
	 *
	 * Default is {@literal false}.
	 *
	 * Use the {@literal spring.data.gemfire.pdx.read-serialized} property in {@literal application.properties}.
	 */
	boolean readSerialized() default false;

	/**
	 * Configures the PDX serializer to be used by the cache to serialize object data.
	 */
	String serializerBeanName() default "";

}
