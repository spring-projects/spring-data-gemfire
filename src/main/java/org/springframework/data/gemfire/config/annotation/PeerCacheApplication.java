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

import com.gemstone.gemfire.cache.control.ResourceManager;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * The PeerCacheApplication annotation enables an embedded GemFire peer {@link com.gemstone.gemfire.cache.Cache}
 * instance in a Spring Data GemFire based application.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.PeerCacheConfiguration
 * @see com.gemstone.gemfire.cache.control.ResourceManager
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
	 * Default is {@literal false}.
	 */
	boolean copyOnRead() default false;

	/**
	 * Configures the percentage of heap at or above which the cache is considered in danger of becoming inoperable.
	 *
	 * @see com.gemstone.gemfire.cache.control.ResourceManager#DEFAULT_CRITICAL_HEAP_PERCENTAGE
	 */
	float criticalHeapPercentage() default ResourceManager.DEFAULT_CRITICAL_HEAP_PERCENTAGE;

	/**
	 * By default, a GemFire member (both locators and servers) will attempt to reconnect and reinitialize the cache
	 * after it has been forced out of the distributed system by a network partition event or has otherwise been
	 * shunned by other members. Use this property to enable the auto-reconnect behavior.
	 *
	 * Default is {@literal false}.
	 */
	boolean enableAutoReconnect() default false;

	/**
	 * Configures the percentage of heap at or above which the eviction should begin on Regions configured
	 * for HeapLRU eviction.
	 *
	 * @see com.gemstone.gemfire.cache.control.ResourceManager#DEFAULT_EVICTION_HEAP_PERCENTAGE
	 */
	float evictionHeapPercentage() default ResourceManager.DEFAULT_EVICTION_HEAP_PERCENTAGE;

	/**
	 * Configures the list of GemFire Locators defining the cluster to which this GemFire cache data node
	 * should connect.
	 */
	String locators() default "";

	/**
	 * Configures the length, in seconds, of distributed lock leases obtained by this cache.
	 *
	 * Default is {@literal 120} seconds.
	 */
	int lockLease() default 120;

	/**
	 * Configures the number of seconds a cache operation will wait to obtain a distributed lock lease.
	 *
	 * Default is {@literal 60} seconds.
	 */
	int lockTimeout() default 60;

	/**
	 * Configures the log level used to output log messages at GemFire cache runtime.
	 *
	 * Default is {@literal config}.
	 */
	String logLevel() default PeerCacheConfiguration.DEFAULT_LOG_LEVEL;

	/**
	 * Configures the frequency (in seconds) at which a message will be sent by the primary cache-server to all
	 * the secondary cache-server nodes to remove the events which have already been dispatched from the queue.
	 *
	 * Default is {@literal 1} second.
	 */
	int messageSyncInterval() default 1;

	/**
	 * Configures the name of this GemFire member in the cluster (distributed system).
	 *
	 * Default is {@literal SpringBasedPeerCacheApplication}.
	 */
	String name() default PeerCacheConfiguration.DEFAULT_NAME;

	/**
	 * Configures the number of seconds a cache get operation can spend searching for a value before it times out.
	 *
	 * Default is {@literal 300} seconds.
	 */
	int searchTimeout() default 300;

	/**
	 * Configures whether this GemFire cache member node would pull it's configuration meta-data
	 * from the cluster-based Cluster Configuration service.
	 *
	 * Default is {@literal false}.
	 */
	boolean useClusterConfiguration() default false;

}
