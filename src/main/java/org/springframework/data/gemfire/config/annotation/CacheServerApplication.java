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
 * @see org.apache.geode.cache.control.ResourceManager
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.apache.geode.cache.server.ClientSubscriptionConfig
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.CacheServerConfiguration
 * @see org.springframework.data.gemfire.config.annotation.EnableCacheServers
 * @see org.springframework.data.gemfire.config.annotation.EnableCacheServer
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
	 * Defaults to {@literal true).
	 *
	 * Use {@literal spring.data.gemfire.cache.server.auto-startup} property in {@literal application.properties}.
	 */
	boolean autoStartup() default true;

	/**
	 * Configures the ip address or host name that this cache server will listen on.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_BIND_ADDRESS}.
	 *
	 * Use {@literal spring.data.gemfire.cache.server.bind-address} property in {@literal application.properties}.
	 */
	String bindAddress() default CacheServer.DEFAULT_BIND_ADDRESS;

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
	 * Configures the percentage of off-heap at or above which the cache is considered in danger of becoming inoperable.
	 *
	 * Defaults to {@literal 0.0}.
	 *
	 * Use {@literal spring.data.gemfire.cache.critical-off-heap-percentage} property
	 * in {@literal application.properties}.
	 */
	float criticalOffHeapPercentage() default 0.0f;

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
	 * Configures the percentage of off-heap at or above which the eviction should begin on Regions configured
	 * for HeapLRU eviction.
	 *
	 * Defaults to {@literal 0.0}.
	 *
	 * Use {@literal spring.data.gemfire.cache.eviction-off-heap-percentage} property
	 * in {@literal application.properties}.
	 */
	float evictionOffHeapPercentage() default 0.0f;

	/**
	 * Configures the ip address or host name that server locators will tell clients that this cache server
	 * is listening on.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_HOSTNAME_FOR_CLIENTS}.
	 *
	 * Use {@literal spring.data.gemfire.cache.server.hostname-for-clients} property
	 * in {@literal application.properties}.
	 */
	String hostnameForClients() default CacheServer.DEFAULT_HOSTNAME_FOR_CLIENTS;

	/**
	 * Configures the frequency in milliseconds to poll the load probe on this cache server.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_LOAD_POLL_INTERVAL}.
	 *
	 * Use {@literal spring.data.gemfire.cache.server.load-poll-interval} property in {@literal application.properties}.
	 */
	long loadPollInterval() default CacheServer.DEFAULT_LOAD_POLL_INTERVAL;

	/**
	 * Configures the list of Locators defining the cluster to which this Spring cache application will connect.
	 *
	 * Use {@literal spring.data.gemfire.locators} property in {@literal application.properties}.
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
	String logLevel() default CacheServerConfiguration.DEFAULT_LOG_LEVEL;

	/**
	 * Configures the maximum allowed client connections.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_MAX_CONNECTIONS}.
	 *
	 * Use {@literal spring.data.gemfire.cache.server.max-connections} property in {@literal application.properties}.
	 */
	int maxConnections() default CacheServer.DEFAULT_MAX_CONNECTIONS;

	/**
	 * Configures he maximum number of messages that can be enqueued in a client-queue.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_MAXIMUM_MESSAGE_COUNT}.
	 *
	 * Use {@literal spring.data.gemfire.cache.server.max-message-count} property in {@literal application.properties}.
	 */
	int maxMessageCount() default CacheServer.DEFAULT_MAXIMUM_MESSAGE_COUNT;

	/**
	 * Configures the maximum number of threads allowed in this cache server to service client requests.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_MAX_THREADS}.
	 *
	 * Use {@literal spring.data.gemfire.cache.server.max-threads} property in {@literal application.properties}.
	 */
	int maxThreads() default CacheServer.DEFAULT_MAX_THREADS;

	/**
	 * Configures the maximum amount of time between client pings.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS}.
	 *
	 * Use {@literal spring.data.gemfire.cache.server.max-time-between-pings} property
	 * in {@literal application.properties}.
	 */
	int maxTimeBetweenPings() default CacheServer.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS;

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
	 * Configures the time (in seconds ) after which a message in the client queue will expire.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_MESSAGE_TIME_TO_LIVE}.
	 *
	 * Use {@literal spring.data.gemfire.cache.server.message-time-to-live} property
	 * in {@literal application.properties}.
	 */
	int messageTimeToLive() default CacheServer.DEFAULT_MESSAGE_TIME_TO_LIVE;

	/**
	 * Configures the name of this GemFire member in the cluster (distributed system).
	 *
	 * Defaults to {@literal SpringBasedCacheServerApplication}.
	 *
	 * Use either the {@literal spring.data.gemfire.name} or the {@literal spring.data.gemfire.cache.name} property
	 * in {@literal application.properties}.
	 */
	String name() default CacheServerConfiguration.DEFAULT_NAME;

	/**
	 * Configures the port on which this cache server listens for clients.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_PORT}.
	 *
	 * Use {@literal spring.data.gemfire.cache.server.port} property in {@literal application.properties}.
	 */
	int port() default CacheServer.DEFAULT_PORT;

	/**
	 * Configures the number of seconds a cache get operation can spend searching for a value before it times out.
	 *
	 * Defaults to {@literal 300} seconds, or 5 minutes.
	 *
	 * Use {@literal spring.data.gemfire.cache.peer.search-timeout} property in {@literal application.properties}.
	 */
	int searchTimeout() default 300;

	/**
	 * Configures the configured buffer size of the socket connection for this CacheServer.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_SOCKET_BUFFER_SIZE}.
	 *
	 * Use {@literal spring.data.gemfire.cache.server.socket-buffer-size} property in {@literal application.properties}.
	 */
	int socketBufferSize() default CacheServer.DEFAULT_SOCKET_BUFFER_SIZE;

	/**
	 * Configures the capacity of the client queue.
	 *
	 * Defaults to {@link ClientSubscriptionConfig#DEFAULT_CAPACITY}.
	 *
	 * Use {@literal spring.data.gemfire.cache.server.subscription-capacity} property
	 * in {@literal application.properties}.
	 */
	int subscriptionCapacity() default ClientSubscriptionConfig.DEFAULT_CAPACITY;

	/**
	 * Configures the disk store name for overflow.
	 *
	 * Use {@literal spring.data.gemfire.cache.server.subscription-disk-store-name} property
	 * in {@literal application.properties}.
	 */
	String subscriptionDiskStoreName() default "";

	/**
	 * Configures the eviction policy that is executed when capacity of the client queue is reached.
	 *
	 * Defaults to {@link SubscriptionEvictionPolicy#NONE}.
	 *
	 * Use {@literal spring.data.gemfire.cache.server.subscription-eviction-policy} property
	 * in {@literal application.properties}.
	 */
	SubscriptionEvictionPolicy subscriptionEvictionPolicy() default SubscriptionEvictionPolicy.NONE;

	/**
	 * Determines whether the Spring {@link BeanFactory} locator should be enabled to lookup
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

	/**
	 * Configures the tcpNoDelay setting of sockets used to send messages to clients.
	 *
	 * TcpNoDelay is enabled by default.
	 *
	 * Use either the {@literal spring.data.gemfire.cache.server.<beanName>.tcp-no-delay} property
	 * or the {@literal spring.data.gemfire.cache.server.tcp-no-delay} property
	 * in {@literal application.properties}.
	 */
	boolean tcpNoDelay() default CacheServer.DEFAULT_TCP_NO_DELAY;

}
