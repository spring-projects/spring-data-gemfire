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

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.client.Pool;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.cache.config.EnableGemfireCaching;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.mapping.annotation.ClientRegion;

/**
 * The {@link EnableCachingDefinedRegions} annotation marks a Spring {@link Configuration @Configuration} application
 * annotated class to enable the creation of GemFire/Geode {@link Region Regions} based on Spring's Cache Abstraction
 * Annotations applied to application service methods and types.
 *
 * Additionally, this annotation enables Spring's Cache Abstraction with SDG's {@link EnableGemfireCaching} annotation,
 * which declares Spring's {@link org.springframework.cache.annotation.EnableCaching} annotation as well as declares
 * the SDG {@link org.springframework.data.gemfire.cache.GemfireCacheManager} bean definition.
 *
 * @author John Blum
 * @see java.lang.annotation.Documented
 * @see java.lang.annotation.Inherited
 * @see java.lang.annotation.Retention
 * @see java.lang.annotation.Target
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.RegionShortcut
 * @see org.apache.geode.cache.client.ClientRegionShortcut
 * @see org.apache.geode.cache.client.Pool
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.cache.config.EnableGemfireCaching
 * @see org.springframework.data.gemfire.config.annotation.CachingDefinedRegionsConfiguration
 * @since 2.0.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@EnableGemfireCaching
@Import(CachingDefinedRegionsConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableCachingDefinedRegions {

	/**
	 * When this annotation is applied to a cache client application, the {@literal clientRegionShortcut} attribute
	 * specifies the data management policy applied to client {@link Region Regions} where persistent entities are
	 * only annotated with the generic {@link org.springframework.data.gemfire.mapping.annotation.Region}
	 * mapping annotation, or the non-data policy specific mapping annotation.
	 *
	 * Defaults to {@link ClientRegionShortcut#PROXY}.
	 */
	ClientRegionShortcut clientRegionShortcut() default ClientRegionShortcut.PROXY;

	/**
	 * When this annotation is applied to a cache client application, the {@literal poolName} attribute refers to
	 * the default name of the GemFire/Geode {@link Pool} assigned to client {@link Region Region(s)}.
	 *
	 * This value can be overridden by annotating entities with the {@link ClientRegion} annotation.
	 *
	 * Defaults to {@literal DEFAULT}.
	 */
	String poolName() default ClientRegionFactoryBean.DEFAULT_POOL_NAME;

	/**
	 * When this annotation is applied to a peer cache application, the {@literal serverRegionShortcut} attribute
	 * specifies the data management policy applied to server {@link Region Regions} where persistent entities are
	 * only annotated with the generic {@link org.springframework.data.gemfire.mapping.annotation.Region}
	 * mapping annotation, or the non-data policy specific mapping annotation.
	 *
	 * Defaults to {@link RegionShortcut#PARTITION}.
	 */
	RegionShortcut serverRegionShortcut() default RegionShortcut.PARTITION;

}
