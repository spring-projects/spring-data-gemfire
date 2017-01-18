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

import org.apache.geode.cache.client.Pool;
import org.apache.geode.cache.client.PoolFactory;
import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.GemfireUtils;

/**
 * The {@link EnablePool} annotation configures a Spring {@link org.springframework.context.annotation.Configuration}
 * annotated class with a "named" GemFire client {@link Pool} bean in the application context.
 *
 * This annotation is used in conjunction with the {@link ClientCacheApplication} annotation to add an additional
 * {@link Pool} to a GemFire cache client application configured with Spring (Data GemFire).
 *
 * To add more than 1 {@link Pool} to your application, this annotation can be nested in the {@link EnablePools}
 * annotation.
 *
 * @author John Blum
 * @see org.apache.geode.cache.client.Pool
 * @see org.apache.geode.cache.client.PoolFactory
 * @see org.springframework.data.gemfire.config.annotation.AddPoolConfiguration
 * @see org.springframework.data.gemfire.config.annotation.EnablePools
 * @see org.springframework.data.gemfire.config.annotation.PoolConfigurer
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(AddPoolConfiguration.class)
@SuppressWarnings("unused")
public @interface EnablePool {

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
	 * Configures the load conditioning interval for this pool.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_LOAD_CONDITIONING_INTERVAL
	 */
	int loadConditioningInterval() default PoolFactory.DEFAULT_LOAD_CONDITIONING_INTERVAL;

	/**
	 * Configures the GemFire {@link org.apache.geode.distributed.Locator Locators} to which
	 * this cache client will connect.
	 */
	Locator[] locators() default {};

	/**
	 * A {@link String} containing a comma-delimited list of hosts and ports defining the connection endpoints
	 * of GemFire Locators in the cluster.
	 *
	 * The {@link String} must be formatted as: 'host1[port], host2[port], ..., hostN[port]'.
	 */
	String locatorsString() default "";

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
	 * Specifies the name of the GemFire client {@link Pool}.
	 */
	String name();

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
	 * Configures the number of times to retry a request after timeout/exception.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_RETRY_ATTEMPTS
	 */
	int retryAttempts() default PoolFactory.DEFAULT_RETRY_ATTEMPTS;

	/**
	 * Configures the group that all servers this pool connects to must belong to.
	 *
	 * @see org.apache.geode.cache.client.PoolFactory#DEFAULT_SERVER_GROUP
	 */
	String serverGroup() default PoolFactory.DEFAULT_SERVER_GROUP;

	/**
	 * Configures the GemFire {@link org.apache.geode.cache.server.CacheServer CacheServers} to which
	 * this cache client will connect.
	 */
	Server[] servers() default {};

	/**
	 * A {@link String} containing a comma-delimited list of hosts and ports defining the connection endpoints
	 * of GemFire Servers in the cluster.
	 *
	 * The {@link String} must be formatted as: 'host1[port], host2[port], ..., hostN[port]'.
	 */
	String serversString() default "";

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

	@interface Locator {

		String host() default "localhost";

		int port() default GemfireUtils.DEFAULT_LOCATOR_PORT;

	}

	@interface Server {

		String host() default "localhost";

		int port() default GemfireUtils.DEFAULT_CACHE_SERVER_PORT;

	}
}
