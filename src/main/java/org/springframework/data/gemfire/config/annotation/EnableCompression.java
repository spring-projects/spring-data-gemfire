/*
 * Copyright 2017-2018 the original author or authors.
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
 */

package org.springframework.data.gemfire.config.annotation;

import static org.springframework.data.gemfire.config.annotation.CompressionConfiguration.SNAPPY_COMPRESSOR_BEAN_NAME;

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
 * The {@link EnableCompression} annotation marks a Spring {@link Configuration @Configuration} annotated application
 * {@link Class} to configure and enable Pivotal GemFire/Apache Geode {@link Region} data compression.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.CompressionConfiguration
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(CompressionConfiguration.class)
public @interface EnableCompression {

	/**
	 * Reference to the {@link String name} of a bean having type {@link org.apache.geode.compression.Compressor}
	 * registered in the Spring container to handle {@link Region} compression.
	 *
	 * Defaults to {@literal snappyCompressor}.
	 *
	 * Set the {@literal spring.data.gemfire.cache.compression.compressor-bean-name}
	 * in {@literal application.properties}.
	 */
	String compressorBeanName() default SNAPPY_COMPRESSOR_BEAN_NAME;

	/**
	 * Identifies all the {@link Region Regions} by name in which the data compression will be enabled.
	 *
	 * Defaults to all {@link Region Regions}.
	 *
	 * Set the {@literal spring.data.gemfire.cache.compression.region-names} property
	 * in {@literal application.properties}.
	 */
	String[] regionNames() default {};

}
