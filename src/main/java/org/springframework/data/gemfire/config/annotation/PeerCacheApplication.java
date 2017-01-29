/*
 * Copyright 2012-2018 the original author or authors.
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

import org.apache.geode.cache.control.ResourceManager;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.support.GemfireBeanFactoryLocator;

/**
 * The {@link PeerCacheApplication} annotation enables an embedded GemFire peer {@link org.apache.geode.cache.Cache}
 * instance in a Spring Data GemFire based application.
 *
 * @author John Blum
 * @see org.apache.geode.cache.control.ResourceManager
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.PeerCacheConfiguration
 * @see org.springframework.data.gemfire.support.GemfireBeanFactoryLocator
 * @since 1.9.0
 */
@Target({ ElementType.ANNOTATION_TYPE, ElementType.TYPE})
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Configuration
@Import(PeerCacheConfiguration.class)
@SuppressWarnings("unused")
public @interface PeerCacheApplication {

	/**
	 * Indicates whether the "copy on read" is enabled for this cache.
	 *
	 * Defaults to {@literal false}.
	 *
	 * Use {@literal spring.data.gemfire.cache.copy-on-read} property in {@literal application.properties}.
	 */
	boolean copyOnRead() default false;

	/**
	 * Configures the percentage of heap at or above which the cache is considered in danger of becoming inoperable.
	 *
	 * Defaults to {@link ResourceManager#DEFAULT_CRITICAL_PERCENTAGE}.
	 *
	 * Use {@literal spring.data.gemfire.cache.critical-heap-percentage} property in {@literal application.properties}.
	 */
	float criticalHeapPercentage() default ResourceManager.DEFAULT_CRITICAL_PERCENTAGE;

	/**
	 * By default, a GemFire member (both locators and servers) will attempt to reconnect and reinitialize the cache
	 * after it has been forced out of the distributed system by a network partition event or has otherwise been
	 * shunned by other members. Use this property to enable the auto-reconnect behavior.
	 *
	 * Defaults to {@literal false}.
	 *
	 * Use {@literal spring.data.gemfire.cache.peer.enable-auto-reconnect} property
	 * in {@literal application.properties}.
	 */
	boolean enableAutoReconnect() default false;

	/**
	 * Configures the percentage of heap at or above which the eviction should begin on Regions configured
	 * for HeapLRU eviction.
	 *
	 * Defaults to {@link ResourceManager#DEFAULT_EVICTION_PERCENTAGE}.
	 *
	 * Use {@literal spring.data.gemfire.cache.eviction-heap-percentage} property in {@literal application.properties}.
	 */
	float evictionHeapPercentage() default ResourceManager.DEFAULT_EVICTION_PERCENTAGE;

	/**
	 * Configures the list of Locators defining the cluster to which this Spring cache application will connect.
	 *
	 * Use {@literal spring.data.gemfire.cache.peer.locators} property in {@literal application.properties}.
	 */
	String locators() default "";

	/**
	 * Configures the length, in seconds, of distributed lock leases obtained by this cache.
	 *
	 * Defaults to {@literal 120} seconds.
	 *
	 * Use {@literal spring.data.gemfire.cache.peer.lock-lease} property in {@literal application.properties}.
	 */
	int lockLease() default 120;

	/**
	 * Configures the number of seconds a cache operation will wait to obtain a distributed lock lease.
	 *
	 * Defaults to {@literal 60} seconds.
	 *
	 * Use {@literal spring.data.gemfire.cache.peer.lock-timeout} property in {@literal application.properties}.
	 */
	int lockTimeout() default 60;

	/**
	 * Configures the log level used to output log messages at GemFire cache runtime.
	 *
	 * Defaults to {@literal config}.
	 *
	 * Use {@literal spring.data.gemfire.cache.log-level} property in {@literal application.properties}.
	 */
	String logLevel() default PeerCacheConfiguration.DEFAULT_LOG_LEVEL;

	/**
	 * Configures the frequency (in seconds) at which a message will be sent by the primary cache-server to all
	 * the secondary cache-server nodes to remove the events which have already been dispatched from the queue.
	 *
	 * Defaults to {@literal 1} second.
	 *
	 * Use {@literal spring.data.gemfire.cache.peer.message-sync-interval} property
	 * in {@literal application.properties}.
	 */
	int messageSyncInterval() default 1;

	/**
	 * Configures the name of this GemFire member in the cluster (distributed system).
	 *
	 * Defaults to {@literal SpringBasedPeerCacheApplication}.
	 *
	 * Use either the {@literal spring.data.gemfire.name} or the {@literal spring.data.gemfire.cache.name} property
	 * in {@literal application.properties}.
	 */
	String name() default PeerCacheConfiguration.DEFAULT_NAME;

	/**
	 * Configures the number of seconds a cache get operation can spend searching for a value before it times out.
	 *
	 * Defaults to {@literal 300} seconds, or 5 minutes.
	 *
	 * Use {@literal spring.data.gemfire.cache.peer.search-timeout} property in {@literal application.properties}.
	 */
	int searchTimeout() default 300;

	/**
	 * Determines whether the {@link GemfireBeanFactoryLocator} should be enabled to lookup
	 * the Spring {@link BeanFactory} to auto-wire and configure/initialize GemFire components
	 * created in a non-Spring managed, GemFire context.
	 *
	 * Defaults to {@literal false}.
	 *
	 * Use {@literal spring.data.gemfire.use-bean-factory-locator} property in {@literal application.properties}.
	 */
	boolean useBeanFactoryLocator() default false;

	/**
	 * Configures whether this GemFire cache member node would pull it's configuration meta-data
	 * from the cluster-based Cluster Configuration service.
	 *
	 * Defaults to {@literal false}.
	 *
	 * Use {@literal spring.data.gemfire.cache.peer.use-cluster-configuration} property
	 * in {@literal application.properties}.
	 */
	boolean useClusterConfiguration() default false;

}
