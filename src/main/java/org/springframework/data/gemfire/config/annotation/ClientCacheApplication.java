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
	 * Used only for clients in a client/server installation. If set, this indicates that the client is durable
	 * and identifies the client. The ID is used by servers to reestablish any messaging that was interrupted
	 * by client downtime.
	 *
	 * Use {@literal spring.data.gemfire.cache.client.durable-client-id} property in {@literal application.properties}.
	 */
	String durableClientId() default "";

	/**
	 * Used only for clients in a client/server installation. Number of seconds this client can remain disconnected
	 * from its server and have the server continue to accumulate durable events for it.
	 *
	 * Defaults to {@literal 300} seconds, or 5 minutes.
	 *
	 * Use {@literal spring.data.gemfire.cache.client.durable-client-timeout} property
	 * in {@literal application.properties}.
	 */
	int durableClientTimeout() default 300;

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
	 * Configures the free connection timeout for this pool.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_FREE_CONNECTION_TIMEOUT}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.free-connection-timeout} property
	 * or the {@literal spring.data.gemfire.pool.free-connection-timeout} property in {@literal application.properties}.
	 */
	int freeConnectionTimeout() default PoolFactory.DEFAULT_FREE_CONNECTION_TIMEOUT;

	/**
	 * Configures the amount of time a connection can be idle before expiring the connection.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_IDLE_TIMEOUT}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.idle-timeout} property
	 * or the {@literal spring.data.gemfire.pool.idle-timeout} property in {@literal application.properties}.
	 */
	long idleTimeout() default PoolFactory.DEFAULT_IDLE_TIMEOUT;

	/**
	 * Configures whether to keep the client queues alive on the server when the client is disconnected
	 *
	 * Defaults to {@literal false}.
	 *
	 * Use {@literal spring.data.gemfire.cache.client.keep-alive} property in {@literal application.properties}.
	 */
	boolean keepAlive() default false;

	/**
	 * Configures the load conditioning interval for this pool.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_LOAD_CONDITIONING_INTERVAL}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.load-conditioning-interval} property
	 * or the {@literal spring.data.gemfire.pool.load-conditioning-interval} property
	 * in {@literal application.properties}.
	 */
	int loadConditioningInterval() default PoolFactory.DEFAULT_LOAD_CONDITIONING_INTERVAL;

	/**
	 * Configures the GemFire {@link org.apache.geode.distributed.Locator Locators} to which
	 * this cache client will connect.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.locators} property
	 * or the {@literal spring.data.gemfire.pool.locators} property in {@literal application.properties}.
	 */
	Locator[] locators() default {};

	/**
	 * Configures the log level used to output log messages at GemFire cache runtime.
	 *
	 * Defaults to {@literal config}.
	 *
	 * Use {@literal spring.data.gemfire.cache.log-level} property in {@literal application.properties}.
	 */
	String logLevel() default ClientCacheConfiguration.DEFAULT_LOG_LEVEL;

	/**
	 * Configures the max number of client to server connections that the pool will create.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_MAX_CONNECTIONS}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.max-connections} property
	 * or the {@literal spring.data.gemfire.pool.max-connections} property in {@literal application.properties}.
	 */
	int maxConnections() default PoolFactory.DEFAULT_MAX_CONNECTIONS;

	/**
	 * Configures the minimum number of connections to keep available at all times.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_MIN_CONNECTIONS}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.min-connections} property
	 * or the {@literal spring.data.gemfire.pool.min-connections} property in {@literal application.properties}.
	 */
	int minConnections() default PoolFactory.DEFAULT_MIN_CONNECTIONS;

	/**
	 * If set to true then the created pool can be used by multiple users.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_MULTIUSER_AUTHENTICATION}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.multi-user-authentication} property
	 * or the {@literal spring.data.gemfire.pool.multi-user-authentication} property
	 * in {@literal application.properties}.
	 */
	boolean multiUserAuthentication() default PoolFactory.DEFAULT_MULTIUSER_AUTHENTICATION;

	/**
	 * Configures the name of this GemFire member in the cluster (distributed system).
	 *
	 * Defaults to {@literal SpringBasedCacheClientApplication}.
	 *
	 * Use either the {@literal spring.data.gemfire.name} or the {@literal spring.data.gemfire.cache.name} property
	 * in {@literal application.properties}.
	 */
	String name() default ClientCacheConfiguration.DEFAULT_NAME;

	/**
	 * Configures how often to ping servers to verify that they are still alive.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_PING_INTERVAL}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.ping-interval} property
	 * or the {@literal spring.data.gemfire.pool.ping-interval} property in {@literal application.properties}.
	 */
	long pingInterval() default PoolFactory.DEFAULT_PING_INTERVAL;

	/**
	 * By default {@code prSingleHopEnabled} is {@literal true} in which case the client is aware of the location
	 * of partitions on servers hosting Regions with {@link org.apache.geode.cache.DataPolicy#PARTITION}.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_PR_SINGLE_HOP_ENABLED}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.pr-single-hop-enabled} property
	 * or the {@literal spring.data.gemfire.pool.pr-single-hop-enabled} property in {@literal application.properties}.
	 */
	boolean prSingleHopEnabled() default PoolFactory.DEFAULT_PR_SINGLE_HOP_ENABLED;

	/**
	 * Configures the number of milliseconds to wait for a response from a server before timing out the operation
	 * and trying another server (if any are available).
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_READ_TIMEOUT}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.read-timeout} property
	 * or the {@literal spring.data.gemfire.pool.read-timeout} property in {@literal application.properties}.
	 */
	int readTimeout() default PoolFactory.DEFAULT_READ_TIMEOUT;

	/**
	 * Notifies the server that this durable client is ready to receive updates.
	 *
	 * Defaults to {@literal false}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.ready-for-events} property
	 * or the {@literal spring.data.gemfire.pool.ready-for-events} property in {@literal application.properties}.
	 */
	boolean readyForEvents() default false;

	/**
	 * Configures the number of times to retry a request after timeout/exception.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_RETRY_ATTEMPTS}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.retry-attempts} property
	 * or the {@literal spring.data.gemfire.pool.retry-attempts} property in {@literal application.properties}.
	 */
	int retryAttempts() default PoolFactory.DEFAULT_RETRY_ATTEMPTS;

	/**
	 * Configures the group that all servers in which this pool connects to must belong to.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_SERVER_GROUP}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.server-group} property
	 * or the {@literal spring.data.gemfire.pool.server-group} property in {@literal application.properties}.
	 */
	String serverGroup() default PoolFactory.DEFAULT_SERVER_GROUP;

	/**
	 * Configures the GemFire {@link org.apache.geode.cache.server.CacheServer CacheServers} to which
	 * this cache client will connect.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.servers} property
	 * or the {@literal spring.data.gemfire.pool.servers} property in {@literal application.properties}.
	 */
	Server[] servers() default {};

	/**
	 * Configures the socket buffer size for each connection made in this pool.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_SOCKET_BUFFER_SIZE}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.socket-buffer-size} property
	 * or the {@literal spring.data.gemfire.pool.socket-buffer-size} property in {@literal application.properties}.
	 */
	int socketBufferSize() default PoolFactory.DEFAULT_SOCKET_BUFFER_SIZE;

	/**
	 * Configures how often to send client statistics to the server.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_STATISTIC_INTERVAL}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.statistic-interval} property
	 * or the {@literal spring.data.gemfire.pool.statistic-interval} property in {@literal application.properties}.
	 */
	int statisticInterval() default PoolFactory.DEFAULT_STATISTIC_INTERVAL;

	/**
	 * Configures the interval in milliseconds to wait before sending acknowledgements to the cache server
	 * for events received from the server subscriptions.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_SUBSCRIPTION_ACK_INTERVAL}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.subscription-ack-interval} property
	 * or the {@literal spring.data.gemfire.pool.subscription-ack-interval} property
	 * in {@literal application.properties}.
	 */
	int subscriptionAckInterval() default PoolFactory.DEFAULT_SUBSCRIPTION_ACK_INTERVAL;

	/**
	 * If set to true then the created pool will have server-to-client subscriptions enabled.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_SUBSCRIPTION_ENABLED}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.subscription-enabled} property
	 * or the {@literal spring.data.gemfire.pool.subscription-enabled} property in {@literal application.properties}.
	 */
	boolean subscriptionEnabled() default PoolFactory.DEFAULT_SUBSCRIPTION_ENABLED;

	/**
	 * Configures the messageTrackingTimeout attribute which is the time-to-live period, in milliseconds,
	 * for subscription events the client has received from the server.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.subscription-message-tracking-timeout} property
	 * or the {@literal spring.data.gemfire.pool.subscription-message-tracking-timeout} property
	 * in {@literal application.properties}.
	 */
	int subscriptionMessageTrackingTimeout() default PoolFactory.DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT;

	/**
	 * Configures the redundancy level for this pools server-to-client subscriptions.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_SUBSCRIPTION_REDUNDANCY}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.subscription-redundancy} property
	 * or the {@literal spring.data.gemfire.pool.subscription-redundancy} property in {@literal application.properties}.
	 */
	int subscriptionRedundancy() default PoolFactory.DEFAULT_SUBSCRIPTION_REDUNDANCY;

	/**
	 * Configures the thread local connections policy for this pool.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_THREAD_LOCAL_CONNECTIONS}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.default.thread-local-connections} property
	 * or the {@literal spring.data.gemfire.pool.thread-local-connections} property
	 * in {@literal application.properties}.
	 */
	boolean threadLocalConnections() default PoolFactory.DEFAULT_THREAD_LOCAL_CONNECTIONS;

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

	@interface Locator {

		String host() default "localhost";

		int port() default GemfireUtils.DEFAULT_LOCATOR_PORT;

	}

	@interface Server {

		String host() default "localhost";

		int port() default GemfireUtils.DEFAULT_CACHE_SERVER_PORT;

	}
}
