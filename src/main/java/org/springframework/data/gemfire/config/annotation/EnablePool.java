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
 * @see org.springframework.data.gemfire.config.annotation.AddPoolsConfiguration
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
	 * Defaults to {@link PoolFactory#DEFAULT_FREE_CONNECTION_TIMEOUT}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.free-connection-timeout} property
	 * or the {@literal spring.data.gemfire.pool.free-connection-timeout} property
	 * in {@literal application.properties}.
	 */
	int freeConnectionTimeout() default PoolFactory.DEFAULT_FREE_CONNECTION_TIMEOUT;

	/**
	 * Configures the amount of time a connection can be idle before expiring the connection.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_IDLE_TIMEOUT}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.idle-timeout} property
	 * or the {@literal spring.data.gemfire.pool.idle-timeout} property
	 * in {@literal application.properties}.
	 */
	long idleTimeout() default PoolFactory.DEFAULT_IDLE_TIMEOUT;

	/**
	 * Configures the load conditioning interval for this pool.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_LOAD_CONDITIONING_INTERVAL}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.load-conditioning-interval} property
	 * or the {@literal spring.data.gemfire.pool.load-conditioning-interval} property
	 * in {@literal application.properties}.
	 */
	int loadConditioningInterval() default PoolFactory.DEFAULT_LOAD_CONDITIONING_INTERVAL;

	/**
	 * Configures the GemFire {@link org.apache.geode.distributed.Locator Locators} to which
	 * this cache client will connect.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.locators} property
	 * or the {@literal spring.data.gemfire.pool.locators} property
	 * in {@literal application.properties}.
	 */
	Locator[] locators() default {};

	/**
	 * A {@link String} containing a comma-delimited list of hosts and ports defining the connection endpoints
	 * of GemFire Locators in the cluster.
	 *
	 * The {@link String} must be formatted as: 'host1[port], host2[port], ..., hostN[port]'.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.locators} property
	 * or the {@literal spring.data.gemfire.pool.locators} property
	 * in {@literal application.properties}.
	 */
	String locatorsString() default "";

	/**
	 * Configures the max number of client to server connections that the pool will create.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_MAX_CONNECTIONS}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.max-connections} property
	 * or the {@literal spring.data.gemfire.pool.max-connections} property
	 * in {@literal application.properties}.
	 */
	int maxConnections() default PoolFactory.DEFAULT_MAX_CONNECTIONS;

	/**
	 * Configures the minimum number of connections to keep available at all times.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_MIN_CONNECTIONS}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.min-connections} property
	 * or the {@literal spring.data.gemfire.pool.min-connections} property
	 * in {@literal application.properties}.
	 */
	int minConnections() default PoolFactory.DEFAULT_MIN_CONNECTIONS;

	/**
	 * If set to true then the created pool can be used by multiple users.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_MULTIUSER_AUTHENTICATION}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.multi-user-authentication} property
	 * or the {@literal spring.data.gemfire.pool.multi-user-authentication} property
	 * in {@literal application.properties}.
	 */
	boolean multiUserAuthentication() default PoolFactory.DEFAULT_MULTIUSER_AUTHENTICATION;

	/**
	 * Specifies the {@link String name} of the client {@link Pool} in Pivotal GemFire/Apache Geode, which is also
	 * used as the Spring bean name in the container as well as the name
	 * (e.g. {@literal spring.data.gemfire.pool.<poolName>.max-connections} used in the resolution of {@link Pool}
	 * properties from {@literal application.properties} that are specific to this {@link Pool}.
	 */
	String name();

	/**
	 * Configures how often to ping servers to verify that they are still alive.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_PING_INTERVAL}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.ping-interval} property
	 * or the {@literal spring.data.gemfire.pool.ping-interval} property
	 * in {@literal application.properties}.
	 */
	long pingInterval() default PoolFactory.DEFAULT_PING_INTERVAL;

	/**
	 * By default {@code prSingleHopEnabled} is {@literal true} in which case the client is aware of the location
	 * of partitions on servers hosting Regions with {@link org.apache.geode.cache.DataPolicy#PARTITION}.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_PR_SINGLE_HOP_ENABLED}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.pr-single-hop-enabled} property
	 * or the {@literal spring.data.gemfire.pool.pr-single-hop-enabled} property
	 * in {@literal application.properties}.
	 */
	boolean prSingleHopEnabled() default PoolFactory.DEFAULT_PR_SINGLE_HOP_ENABLED;

	/**
	 * Configures the number of milliseconds to wait for a response from a server before timing out the operation
	 * and trying another server (if any are available).
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_READ_TIMEOUT}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.read-timeout} property
	 * or the {@literal spring.data.gemfire.pool.read-timeout} property
	 * in {@literal application.properties}.
	 */
	int readTimeout() default PoolFactory.DEFAULT_READ_TIMEOUT;

	/**
	 * Configures the number of times to retry a request after timeout/exception.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_RETRY_ATTEMPTS}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.retry-attempts} property
	 * or the {@literal spring.data.gemfire.pool.retry-attempts} property
	 * in {@literal application.properties}.
	 */
	int retryAttempts() default PoolFactory.DEFAULT_RETRY_ATTEMPTS;

	/**
	 * Configures the group that all servers in which this pool connects to must belong to.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_SERVER_GROUP}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.server-group} property
	 * or the {@literal spring.data.gemfire.pool.server-group} property
	 * in {@literal application.properties}.
	 */
	String serverGroup() default PoolFactory.DEFAULT_SERVER_GROUP;

	/**
	 * Configures the GemFire {@link org.apache.geode.cache.server.CacheServer CacheServers} to which
	 * this cache client will connect.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.servers} property
	 * or the {@literal spring.data.gemfire.pool.servers} property
	 * in {@literal application.properties}.
	 */
	Server[] servers() default {};

	/**
	 * A {@link String} containing a comma-delimited list of hosts and ports defining the connection endpoints
	 * of GemFire Servers in the cluster.
	 *
	 * The {@link String} must be formatted as: 'host1[port], host2[port], ..., hostN[port]'.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.servers} property
	 * or the {@literal spring.data.gemfire.pool.servers} property
	 * in {@literal application.properties}.
	 */
	String serversString() default "";

	/**
	 * Configures the socket buffer size for each connection made in this pool.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_SOCKET_BUFFER_SIZE}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.socket-buffer-size} property
	 * or the {@literal spring.data.gemfire.pool.socket-buffer-size} property
	 * in {@literal application.properties}.
	 */
	int socketBufferSize() default PoolFactory.DEFAULT_SOCKET_BUFFER_SIZE;

	/**
	 * Configures how often to send client statistics to the server.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_STATISTIC_INTERVAL}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.statistic-interval} property
	 * or the {@literal spring.data.gemfire.pool.statistic-interval} property
	 * in {@literal application.properties}.
	 */
	int statisticInterval() default PoolFactory.DEFAULT_STATISTIC_INTERVAL;

	/**
	 * Configures the interval in milliseconds to wait before sending acknowledgements to the cache server
	 * for events received from the server subscriptions.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_SUBSCRIPTION_ACK_INTERVAL}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.subscription-ack-interval} property
	 * or the {@literal spring.data.gemfire.pool.subscription-ack-interval} property
	 * in {@literal application.properties}.
	 */
	int subscriptionAckInterval() default PoolFactory.DEFAULT_SUBSCRIPTION_ACK_INTERVAL;

	/**
	 * If set to true then the created pool will have server-to-client subscriptions enabled.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_SUBSCRIPTION_ENABLED}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.subscription-enabled} property
	 * or the {@literal spring.data.gemfire.pool.subscription-enabled} property
	 * in {@literal application.properties}.
	 */
	boolean subscriptionEnabled() default PoolFactory.DEFAULT_SUBSCRIPTION_ENABLED;

	/**
	 * Configures the messageTrackingTimeout attribute which is the time-to-live period, in milliseconds,
	 * for subscription events the client has received from the server.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.subscription-message-tracking-timeout} property
	 * or the {@literal spring.data.gemfire.pool.subscription-message-tracking-timeout} property
	 * in {@literal application.properties}.
	 */
	int subscriptionMessageTrackingTimeout() default PoolFactory.DEFAULT_SUBSCRIPTION_MESSAGE_TRACKING_TIMEOUT;

	/**
	 * Configures the redundancy level for this pools server-to-client subscriptions.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_SUBSCRIPTION_REDUNDANCY}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.subscription-redundancy} property
	 * or the {@literal spring.data.gemfire.pool.subscription-redundancy} property
	 * in {@literal application.properties}.
	 */
	int subscriptionRedundancy() default PoolFactory.DEFAULT_SUBSCRIPTION_REDUNDANCY;

	/**
	 * Configures the thread local connections policy for this pool.
	 *
	 * Defaults to {@link PoolFactory#DEFAULT_THREAD_LOCAL_CONNECTIONS}.
	 *
	 * Use either the {@literal spring.data.gemfire.pool.<poolName>.thread-local-connections} property
	 * or the {@literal spring.data.gemfire.pool.thread-local-connections} property
	 * in {@literal application.properties}.
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
