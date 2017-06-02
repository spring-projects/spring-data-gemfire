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

import org.springframework.context.annotation.Import;

/**
 * The {@link EnableAutoRegionLookup} annotation configures a Spring {@link org.springframework.context.annotation.Configuration}
 * annotated class with the ability to automatically look up and register GemFire {@link org.apache.geode.cache.Region Regions}
 * which may have be defined in {@literal cache.xml} or by using GemFire's Cluster Configuration Service.
 *
 * This annotation defines the {@code enabled} attribute to allow users to dynamically change the behavior
 * of auto {@link org.apache.geode.cache.Region} lookup at application configuration time using either a SpEL
 * expression or a Spring property placeholder.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.RegionFactoryBean#setLookupEnabled(Boolean)
 * @see org.springframework.data.gemfire.config.annotation.AutoRegionLookupConfiguration
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(AutoRegionLookupConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableAutoRegionLookup {

	/**
	 * Attribute indicating whether auto {@link org.apache.geode.cache.Region} lookup should be enabled;
	 *
	 * Defaults to {@literal true}.
	 *
	 * Use the {@literal spring.data.gemfire.enable-auto-region-lookup} in {@literal application.properties}
	 * to dynamically customize this configuration setting.
	 */
	boolean enabled() default true;

}
