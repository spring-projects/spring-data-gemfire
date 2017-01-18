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

import org.apache.geode.cache.server.CacheServer;
import org.apache.geode.cache.server.ClientSubscriptionConfig;
import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.server.SubscriptionEvictionPolicy;

/**
 * The {@link EnableCacheServer} annotation configures a Spring {@link org.springframework.context.annotation.Configuration}
 * annotated class with a GemFire {@link CacheServer} bean in the Spring application context.
 *
 * This annotation is used in conjunction with the {@link CacheServerApplication}, or {@link PeerCacheApplication}
 * annotations to add an additional {@link CacheServer CacheServers} to a GemFire peer cache application
 * configured with Spring (Data GemFire).
 *
 * To add more than 1 {@link CacheServer} to your application, this annotation can be nested in
 * the {@link EnableCacheServers} annotation.

 * @author John Blum
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.springframework.data.gemfire.config.annotation.AddCacheServerConfiguration
 * @see org.springframework.data.gemfire.config.annotation.CacheServerConfigurer
 * @see org.springframework.data.gemfire.config.annotation.EnableCacheServers
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(AddCacheServerConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableCacheServer {

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
	 * Configures the time (in seconds ) after which a message in the client queue will expire.
	 *
	 * @see org.apache.geode.cache.server.CacheServer#DEFAULT_MESSAGE_TIME_TO_LIVE
	 */
	int messageTimeToLive() default CacheServer.DEFAULT_MESSAGE_TIME_TO_LIVE;

	/**
	 * Configures the name of the Spring bean defined in the Spring application context.
	 *
	 * Defaults to empty.
	 */
	String name() default "";

	/**
	 * Configures the port on which this cache server listens for clients.
	 *
	 * @see org.apache.geode.cache.server.CacheServer#DEFAULT_PORT
	 */
	int port() default CacheServer.DEFAULT_PORT;

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

}
