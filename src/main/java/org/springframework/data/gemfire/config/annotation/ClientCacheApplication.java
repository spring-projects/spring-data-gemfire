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

import org.apache.geode.cache.client.PoolFactory;
import org.apache.geode.cache.control.ResourceManager;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.support.GemfireBeanFactoryLocator;

/**
 * The {@link ClientCacheApplication} annotation enables a Spring Data GemFire based application to become
 * a GemFire cache client (i.e. {@link org.apache.geode.cache.client.ClientCache}).
 *
 * @author John Blum
 * @see org.apache.geode.cache.client.PoolFactory
 * @see org.apache.geode.cache.control.ResourceManager
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.ClientCacheConfiguration
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Configuration
@Import(ClientCacheConfiguration.class)
@SuppressWarnings("unused")
public @interface ClientCacheApplication {

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
	 * Used only for clients in a client/server installation. If set, this indicates that the client is durable
	 * and identifies the client. The ID is used by servers to reestablish any messaging that was interrupted
	 * by client downtime.
	 */
	String durableClientId() default "";

	/**
	 * Used only for clients in a client/server installation. Number of seconds this client can remain disconnected
	 * from its server and have the server continue to accumulate durable events for it.
	 */
	int durableClientTimeout() default 300;

	/**
	 * Configures the percentage of heap at or above which the eviction should begin on Regions configured
	 * for HeapLRU eviction.
	 *
	 * @see org.apache.geode.cache.control.ResourceManager#DEFAULT_EVICTION_PERCENTAGE
	 */
	float evictionHeapPercentage() default ResourceManager.DEFAULT_EVICTION_PERCENTAGE;

	/**
	 * Configures the free connection timeout for this pool.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_FREE_CONNECTION_TIMEOUT
	 */
	int freeConnectionTimeout() default PoolFactory.DEFAULT_FREE_CONNECTION_TIMEOUT;

	/**
	 * Configures the amount of time a connection can be idle before expiring the connection.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_IDLE_TIMEOUT
	 */
	long idleTimeout() default PoolFactory.DEFAULT_IDLE_TIMEOUT;

	/**
	 * Configures whether to keep the client queues alive on the server when the client is disconnected
	 *
	 * Default is {@literal false}.
	 */
	boolean keepAlive() default false;

	/**
	 * Configures the load conditioning interval for this pool.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_LOAD_CONDITIONING_INTERVAL
	 */
	int loadConditioningInterval() default PoolFactory.DEFAULT_LOAD_CONDITIONING_INTERVAL;

	/**
	 * Configures the GemFire {@link org.apache.geode.distributed.Locator}s to which
	 * this cache client will connect.
	 */
	Locator[] locators() default {};

	/**
	 * Configures the log level used to output log messages at GemFire cache runtime.
	 *
	 * Default is {@literal config}.
	 */
	String logLevel() default ClientCacheConfiguration.DEFAULT_LOG_LEVEL;

	/**
	 * Configures the max number of client to server connections that the pool will create.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_MAX_CONNECTIONS
	 */
	int maxConnections() default PoolFactory.DEFAULT_MAX_CONNECTIONS;

	/**
	 * Configures the minimum number of connections to keep available at all times.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_MIN_CONNECTIONS
	 */
	int minConnections() default PoolFactory.DEFAULT_MIN_CONNECTIONS;

	/**
	 * If set to true then the created pool can be used by multiple users.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_MULTIUSER_AUTHENTICATION
	 */
	boolean multiUserAuthentication() default PoolFactory.DEFAULT_MULTIUSER_AUTHENTICATION;

	/**
	 * Configures the name of this GemFire member in the cluster (distributed system).
	 *
	 * Default is {@literal SpringBasedCacheClientApplication}.
	 */
	String name() default ClientCacheConfiguration.DEFAULT_NAME;

	/**
	 * Configures how often to ping servers to verify that they are still alive.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_PING_INTERVAL
	 */
	long pingInterval() default PoolFactory.DEFAULT_PING_INTERVAL;

	/**
	 * By default {@code prSingleHopEnabled} is {@literal true} in which case the client is aware of the location
	 * of partitions on servers hosting Regions with {@link org.apache.geode.cache.DataPolicy#PARTITION}.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_PR_SINGLE_HOP_ENABLED
	 */
	boolean prSingleHopEnabled() default PoolFactory.DEFAULT_PR_SINGLE_HOP_ENABLED;

	/**
	 * Configures the number of milliseconds to wait for a response from a server before timing out the operation
	 * and trying another server (if any are available).
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_READ_TIMEOUT
	 */
	int readTimeout() default PoolFactory.DEFAULT_READ_TIMEOUT;

	/**
	 * Notifies the server that this durable client is ready to receive updates.
	 *
	 * Default is {@literal false}.
	 */
	boolean readyForEvents() default false;

	/**
	 * Configures the number of times to retry a request after timeout/exception.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_RETRY_ATTEMPTS
	 */
	int retryAttempts() default PoolFactory.DEFAULT_RETRY_ATTEMPTS;

	/**
	 * Configures the group that all servers in which this pool connects to must belong to.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_SERVER_GROUP
	 */
	String serverGroup() default PoolFactory.DEFAULT_SERVER_GROUP;

	/**
	 * Configures the GemFire {@link org.apache.geode.cache.server.CacheServer}s to which
	 * this cache client will connect.
	 */
	Server[] servers() default {};

	/**
	 * Configures the socket buffer size for each connection made in this pool.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_SOCKET_BUFFER_SIZE
	 */
	int socketBufferSize() default PoolFactory.DEFAULT_SOCKET_BUFFER_SIZE;

	/**
	 * Configures how often to send client statistics to the server.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_STATISTIC_INTERVAL
	 */
	int statisticInterval() default PoolFactory.DEFAULT_STATISTIC_INTERVAL;

	/**
	 * Configures the interval in milliseconds to wait before sending acknowledgements to the cache server
	 * for events received from the server subscriptions.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_SUBSCRIPTION_ACK_INTERVAL
	 */
	int subscriptionAckInterval() default PoolFactory.DEFAULT_SUBSCRIPTION_ACK_INTERVAL;

	/**
	 * If set to true then the created pool will have server-to-client subscriptions enabled.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_SUBSCRIPTION_ENABLED
	 */
	boolean subscriptionEnabled() default PoolFactory.DEFAULT_SUBSCRIPTION_ENABLED;

	/**
	 * Configures the messageTrackingTimeout attribute which is the time-to-live period, in milliseconds,
	 * for subscription events the client has received from the server.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT
	 */
	int subscriptionMessageTrackingTimeout() default PoolFactory.DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT;

	/**
	 * Configures the redundancy level for this pools server-to-client subscriptions.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_SUBSCRIPTION_REDUNDANCY
	 */
	int subscriptionRedundancy() default PoolFactory.DEFAULT_SUBSCRIPTION_REDUNDANCY;

	/**
	 * Configures the thread local connections policy for this pool.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_THREAD_LOCAL_CONNECTIONS
	 */
	boolean threadLocalConnections() default PoolFactory.DEFAULT_THREAD_LOCAL_CONNECTIONS;

	/**
	 * Determines whether the {@link GemfireBeanFactoryLocator} should be enabled to lookup
	 * the Spring {@link BeanFactory} to auto-wire and configure/initialize GemFire components
	 * created in a non-Spring managed, GemFire context.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean useBeanFactoryLocator() default false;

	@interface Locator {

		String host() default "localhost";

		int port() default GemfireUtils.DEFAULT_LOCATOR_PORT;

	}

	@interface Server {

		String host() default "localhost";

		int port() default GemfireUtils.DEFAULT_CACHE_SERVER_PORT;

	}
}
