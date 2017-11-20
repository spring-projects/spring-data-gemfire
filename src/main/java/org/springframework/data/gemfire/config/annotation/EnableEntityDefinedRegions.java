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

import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.cache.client.Pool;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.core.annotation.AliasFor;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.support.CacheTypeAwareRegionFactoryBean;
import org.springframework.data.gemfire.mapping.annotation.ClientRegion;

/**
 * The {@link EnableEntityDefinedRegions} annotation marks a Spring {@link Configuration @Configuration} application
 * annotated class to enable the creation of GemFire/Geode {@link Region Regions} based on
 * the application persistent entities.
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
 * @see org.springframework.context.annotation.ComponentScan
 * @see org.springframework.context.annotation.ComponentScan.Filter
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.core.annotation.AliasFor
 * @see org.springframework.data.gemfire.RegionFactoryBean
 * @see org.springframework.data.gemfire.client.ClientRegionFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.EntityDefinedRegionsConfiguration
 * @see org.springframework.data.gemfire.config.annotation.IndexConfiguration
 * @see CacheTypeAwareRegionFactoryBean
 * @see org.springframework.data.gemfire.mapping.annotation.ClientRegion
 * @see org.springframework.data.gemfire.mapping.annotation.LocalRegion
 * @see org.springframework.data.gemfire.mapping.annotation.PartitionRegion
 * @see org.springframework.data.gemfire.mapping.annotation.Region
 * @see org.springframework.data.gemfire.mapping.annotation.ReplicateRegion
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(IndexConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableEntityDefinedRegions {

	/**
	 * Alias for {@link #basePackages()} attribute.
	 *
	 * @return a {@link String} array specifying the packages to search for application persistent entities.
	 * @see #basePackages()
	 */
	@AliasFor(attribute = "basePackages")
	String[] value() default {};

	/**
	 * Base packages to scan for {@link org.springframework.data.gemfire.mapping.annotation.Region @Region} annotated
	 * application persistent entities.
	 *
	 * The {@link #value()} attribute is an alias for this attribute.
	 *
	 * Use {@link #basePackageClasses()} for a type-safe alternative to String-based package names.
	 *
	 * @return a {@link String} array specifying the packages to search for application persistent entities.
	 * @see #value()
	 */
	@AliasFor(attribute = "value")
	String[] basePackages() default {};

	/**
	 * Type-safe alternative to the {@link #basePackages()} attribute for specifying the packages to scan for
	 * {@link org.springframework.data.gemfire.mapping.annotation.Region @Region} annotated application persistent entities.
	 *
	 * The package of each class specified will be scanned.
	 *
	 * Consider creating a special no-op marker class or interface in each package that serves no other purpose
	 * than being referenced by this attribute.
	 *
	 * @return an array of {@link Class classes} used to determine the packages to scan
	 * for application persistent entities.
	 */
	Class<?>[] basePackageClasses() default {};

	/**
	 * Specifies which types are not eligible for component scanning.
	 *
	 * @return an array of {@link org.springframework.context.annotation.ComponentScan.Filter Filters} used to
	 * specify application persistent entities to be excluded during the component scan.
	 */
	ComponentScan.Filter[] excludeFilters() default {};

	/**
	 * Specifies which types are eligible for component scanning.
	 *
	 * Further narrows the set of candidate components from everything in {@link #basePackages()}
	 * or {@link #basePackageClasses()} to everything in the base packages that matches the given filter or filters.
	 *
	 * @return an array {@link ComponentScan.Filter} of Filters used to specify application persistent entities
	 * to be included during the component scan.
	 */
	ComponentScan.Filter[] includeFilters() default {};

	/**
	 * When this annotation is applied to a cache client application, the {@literal clientRegionShortcut} attribute
	 * indicates the default data policy applied to client {@link Region Regions} where the persistent entities
	 * are only annotated with the generic {@link org.springframework.data.gemfire.mapping.annotation.Region}
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
	 * indicates the default data policy applied to server {@link Region Regions} where the persistent entities
	 * are only annotated with the generic {@link org.springframework.data.gemfire.mapping.annotation.Region}
	 * mapping annotation, or the non-data policy specific mapping annotation.
	 *
	 * Defaults to {@link RegionShortcut#PARTITION}.
	 */
	RegionShortcut serverRegionShortcut() default RegionShortcut.PARTITION;

	/**
	 * Determines whether the created {@link Region} will have strongly-typed key and value constraints
	 * based on the ID and {@link Class} type of application persistent entity.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean strict() default false;

}
