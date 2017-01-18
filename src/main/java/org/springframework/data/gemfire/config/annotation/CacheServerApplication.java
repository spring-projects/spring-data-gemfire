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

import org.apache.geode.cache.control.ResourceManager;
import org.apache.geode.cache.server.CacheServer;
import org.apache.geode.cache.server.ClientSubscriptionConfig;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.server.SubscriptionEvictionPolicy;

/**
 * The CacheServerApplication annotation enables an embedded GemFire
 * {@link org.apache.geode.cache.server.CacheServer} instance in a Spring Data GemFire based application.
 *
 * In addition, this also implies an embedded GemFire peer {@link org.apache.geode.cache.Cache} must exist
 * and therefore will be configured, constructed and initialized as a Spring bean in the application context.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.CacheServerConfiguration
 * @see org.springframework.data.gemfire.config.annotation.EnableCacheServers
 * @see org.springframework.data.gemfire.config.annotation.EnableCacheServer
 * @see org.apache.geode.cache.control.ResourceManager
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.apache.geode.cache.server.ClientSubscriptionConfig
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Configuration
@Import(CacheServerConfiguration.class)
@SuppressWarnings("unused")
public @interface CacheServerApplication {

	/**
	 * Configures whether the {@link CacheServer} should start automatically at runtime.
	 *
	 * Default is {@literal true).
	 */
	boolean autoStartup() default true;

	/**
	 * Configures the ip address or host name that this cache server will listen on.
	 *
	 * @see org.apache.geode.cache.server.CacheServer#DEFAULT_BIND_ADDRESS
	 */
	String bindAddress() default CacheServer.DEFAULT_BIND_ADDRESS;

	/**
	 * Indicates whether the "copy on read" is enabled for this cache.
	 *
	 * Default is {@literal false}.
	 */
	boolean copyOnRead() default false;

	/**
	 * Configures the percentage of heap at or above which the cache is considered in danger of becoming inoperable.
	 *
	 * @see org.apache.geode.cache.control.ResourceManager#DEFAULT_CRITICAL_PERCENTAGE
	 */
	float criticalHeapPercentage() default ResourceManager.DEFAULT_CRITICAL_PERCENTAGE;

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
	 * @see org.apache.geode.cache.control.ResourceManager#DEFAULT_EVICTION_PERCENTAGE
	 */
	float evictionHeapPercentage() default ResourceManager.DEFAULT_EVICTION_PERCENTAGE;

	/**
	 * Configures the ip address or host name that server locators will tell clients that this cache server
	 * is listening on.
	 *
	 * @see org.apache.geode.cache.server.CacheServer#DEFAULT_HOSTNAME_FOR_CLIENTS
	 */
	String hostnameForClients() default CacheServer.DEFAULT_HOSTNAME_FOR_CLIENTS;

	/**
	 * Configures the frequency in milliseconds to poll the load probe on this cache server.
	 *
	 * @see org.apache.geode.cache.server.CacheServer#DEFAULT_LOAD_POLL_INTERVAL
	 */
	long loadPollInterval() default CacheServer.DEFAULT_LOAD_POLL_INTERVAL;

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
	 * Default is {@literal 60} seconds
	 */
	int lockTimeout() default 60;

	/**
	 * Configures the log level used to output log messages at GemFire cache runtime.
	 *
	 * Default is {@literal config}.
	 */
	String logLevel() default CacheServerConfiguration.DEFAULT_LOG_LEVEL;

	/**
	 * Configures the maximum allowed client connections.
	 *
	 * @see org.apache.geode.cache.server.CacheServer#DEFAULT_MAX_CONNECTIONS
	 */
	int maxConnections() default CacheServer.DEFAULT_MAX_CONNECTIONS;

	/**
	 * Configures he maximum number of messages that can be enqueued in a client-queue.
	 *
	 * @see org.apache.geode.cache.server.CacheServer#DEFAULT_MAXIMUM_MESSAGE_COUNT
	 */
	int maxMessageCount() default CacheServer.DEFAULT_MAXIMUM_MESSAGE_COUNT;

	/**
	 * Configures the maximum number of threads allowed in this cache server to service client requests.
	 *
	 * @see org.apache.geode.cache.server.CacheServer#DEFAULT_MAX_THREADS
	 */
	int maxThreads() default CacheServer.DEFAULT_MAX_THREADS;

	/**
	 * Configures the maximum amount of time between client pings.
	 *
	 * @see org.apache.geode.cache.server.CacheServer#DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS
	 */
	int maxTimeBetweenPings() default CacheServer.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS;

	/**
	 * Configures the frequency (in seconds) at which a message will be sent by the primary cache-server to all
	 * the secondary cache-server nodes to remove the events which have already been dispatched from the queue.
	 *
	 * Default is {@literal 1} second.
	 */
	int messageSyncInterval() default 1;

	/**
	 * Configures the time (in seconds ) after which a message in the client queue will expire.
	 *
	 * @see org.apache.geode.cache.server.CacheServer#DEFAULT_MESSAGE_TIME_TO_LIVE
	 */
	int messageTimeToLive() default CacheServer.DEFAULT_MESSAGE_TIME_TO_LIVE;

	/**
	 * Configures the name of this GemFire member in the cluster (distributed system).
	 *
	 * Default is {@literal SpringBasedCacheClientApplication}.
	 */
	String name() default CacheServerConfiguration.DEFAULT_NAME;

	/**
	 * Configures the port on which this cache server listens for clients.
	 *
	 * @see org.apache.geode.cache.server.CacheServer#DEFAULT_PORT
	 */
	int port() default CacheServer.DEFAULT_PORT;

	/**
	 * Configures the number of seconds a cache get operation can spend searching for a value before it times out.
	 *
	 * Default is {@literal 300} seconds.
	 */
	int searchTimeout() default 300;

	/**
	 * Configures the configured buffer size of the socket connection for this CacheServer.
	 *
	 * @see org.apache.geode.cache.server.CacheServer#DEFAULT_SOCKET_BUFFER_SIZE
	 */
	int socketBufferSize() default CacheServer.DEFAULT_SOCKET_BUFFER_SIZE;

	/**
	 * Configures the capacity of the client queue.
	 *
	 * @see org.apache.geode.cache.server.ClientSubscriptionConfig#DEFAULT_CAPACITY
	 */
	int subscriptionCapacity() default ClientSubscriptionConfig.DEFAULT_CAPACITY;

	/**
	 * Configures the disk store name for overflow.
	 */
	String subscriptionDiskStoreName() default "";

	/**
	 * Configures the eviction policy that is executed when capacity of the client queue is reached.
	 *
	 * Defaults to {@link SubscriptionEvictionPolicy#NONE}.
	 */
	SubscriptionEvictionPolicy subscriptionEvictionPolicy() default SubscriptionEvictionPolicy.NONE;

	/**
	 * Determines whether the Spring {@link BeanFactory} locator should be enabled to lookup
	 * the Spring {@link BeanFactory} to auto-wire and configure/initialize GemFire components
	 * created in a non-Spring managed, GemFire context.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean useBeanFactoryLocator() default false;

	/**
	 * Configures whether this GemFire cache member node would pull it's configuration meta-data
	 * from the cluster-based Cluster Configuration service.
	 *
	 * Default is {@literal false}.
	 */
	boolean useClusterConfiguration() default false;

}
