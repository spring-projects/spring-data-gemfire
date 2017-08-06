/*
 * Copyright 2017 the original author or authors.
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

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionShortcut;
import org.springframework.context.annotation.Import;

/**
 * The {@link EnableClusterConfiguration} annotation enables Apache Geode / Pivotal GemFire schema-like definitions
 * defined in a Spring [Boot], Geode/GemFire cache client application using Spring config to be pushed to
 * a Geode/GemFire cluster, similar to how schema commands (e.g. `create region`) in Gfsh are processed by
 * an Geode/GemFire Manager.
 *
 * @author John Blum
 * @see java.lang.annotation.Documented
 * @see java.lang.annotation.Inherited
 * @see java.lang.annotation.Retention
 * @see java.lang.annotation.Target
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.ClusterConfigurationConfiguration
 * @since 2.0.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(ClusterConfigurationConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableClusterConfiguration {

	/**
	 * Configures the bind address used by the Spring, GemFire/Geode cache client application to locate
	 * the Manager's HTTP Service and access the Management REST API.  This configuration setting is only used
	 * when {@link #useHttp()} is set to {@literal true}.
	 *
	 * Alternatively, you can configure this setting using the {@literal spring.data.gemfire.management.http.host}
	 * property in {@literal application.properties}.
	 *
	 * Defaults to {@literal localhost}.
	 */
	String host() default ClusterConfigurationConfiguration.DEFAULT_MANAGEMENT_HTTP_HOST;

	/**
	 * Configures the port used by the Spring, GemFire/Geode cache client application to locate
	 * the Manager's HTTP Service and access the Management REST API.  This configuration setting is only used
	 * when {@link #useHttp()} is set to {@literal true}.
	 *
	 * Alternatively, you can configure this setting using the {@literal spring.data.gemfire.management.http.port}
	 * property in {@literal application.properties}.
	 *
	 * Defaults to {@literal 7070}.
	 */
	int port() default ClusterConfigurationConfiguration.DEFAULT_MANAGEMENT_HTTP_PORT;

	/**
	 * Configuration setting used to specify the data management policy used when creating {@link Region Regions}
	 * on the servers in the Geode/GemFire cluster.
	 *
	 * The data management policy is expressed with a {@link RegionShortcut}, but corresponds to the various
	 * different {@link DataPolicy DataPolicies} available.
	 *
	 * Alternatively, you can configure this setting using the {@literal spring.data.gemfire.cluster.region.type}
	 * property in {@literal application.properties}.
	 *
	 * Defaults to {@link RegionShortcut#PARTITION}.
	 */
	RegionShortcut serverRegionShortcut() default RegionShortcut.PARTITION;

	/**
	 * Configures whether connectivity between the Spring, GemFire/Geode application should be established using HTTP.
	 *
	 * Alternatively, you can configure this setting using the {@literal spring.data.gemfire.management.use-http}
	 * property in {@literal application.properties}.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean useHttp() default ClusterConfigurationConfiguration.DEFAULT_MANAGEMENT_USE_HTTP;

}
