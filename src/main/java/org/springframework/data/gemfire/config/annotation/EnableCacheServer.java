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
	 * Defaults to {@literal true).
	 *
	 * Use either the {@literal spring.data.gemfire.cache.server.<beanName>.auto-startup} property
	 * or the {@literal spring.data.gemfire.cache.server.auto-startup} property
	 * in {@literal application.properties}.
	 */
	boolean autoStartup() default true;

	/**
	 * Configures the ip address or host name that this cache server will listen on.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_BIND_ADDRESS}.
	 *
	 * Use either the {@literal spring.data.gemfire.cache.server.<beanName>.bind-address} property
	 * or the {@literal spring.data.gemfire.cache.server.bind-address} property
	 * in {@literal application.properties}.
	 */
	String bindAddress() default CacheServer.DEFAULT_BIND_ADDRESS;

	/**
	 * Configures the ip address or host name that server locators will tell clients that this cache server
	 * is listening on.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_HOSTNAME_FOR_CLIENTS}.
	 *
	 * Use either the {@literal spring.data.gemfire.cache.server.<beanName>.hostname-for-clients} property
	 * or the {@literal spring.data.gemfire.cache.server.hostname-for-clients} property
	 * in {@literal application.properties}.
	 */
	String hostnameForClients() default CacheServer.DEFAULT_HOSTNAME_FOR_CLIENTS;

	/**
	 * Configures the frequency in milliseconds to poll the load probe on this cache server.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_LOAD_POLL_INTERVAL}.
	 *
	 * Use either the {@literal spring.data.gemfire.cache.server.<beanName>.load-poll-interval} property
	 * or the {@literal spring.data.gemfire.cache.server.load-poll-interval} property
	 * in {@literal application.properties}.
	 */
	long loadPollInterval() default CacheServer.DEFAULT_LOAD_POLL_INTERVAL;

	/**
	 * Configures the maximum allowed client connections.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_MAX_CONNECTIONS}.
	 *
	 * Use either the {@literal spring.data.gemfire.cache.server.<beanName>.max-connections} property
	 * or the {@literal spring.data.gemfire.cache.server.max-connections} property
	 * in {@literal application.properties}.
	 */
	int maxConnections() default CacheServer.DEFAULT_MAX_CONNECTIONS;

	/**
	 * Configures he maximum number of messages that can be enqueued in a client-queue.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_MAXIMUM_MESSAGE_COUNT}.
	 *
	 * Use either the {@literal spring.data.gemfire.cache.server.<beanName>.max-message-count} property
	 * or the {@literal spring.data.gemfire.cache.server.max-message-count} property
	 * in {@literal application.properties}.
	 */
	int maxMessageCount() default CacheServer.DEFAULT_MAXIMUM_MESSAGE_COUNT;

	/**
	 * Configures the maximum number of threads allowed in this cache server to service client requests.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_MAX_THREADS}.
	 *
	 * Use either the {@literal spring.data.gemfire.cache.server.<beanName>.max-threads} property
	 * or the {@literal spring.data.gemfire.cache.server.max-threads} property
	 * in {@literal application.properties}.
	 */
	int maxThreads() default CacheServer.DEFAULT_MAX_THREADS;

	/**
	 * Configures the maximum amount of time between client pings.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS}.
	 *
	 * Use either the {@literal spring.data.gemfire.cache.server.<beanName>.max-time-between-pings} property
	 * or the {@literal spring.data.gemfire.cache.server.max-time-between-pings} property
	 * in {@literal application.properties}.
	 */
	int maxTimeBetweenPings() default CacheServer.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS;

	/**
	 * Configures the time (in seconds ) after which a message in the client queue will expire.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_MESSAGE_TIME_TO_LIVE}.
	 *
	 * Use either the {@literal spring.data.gemfire.cache.server.<beanName>.message-time-to-live} property
	 * or the {@literal spring.data.gemfire.cache.server.message-time-to-live} property
	 * in {@literal application.properties}.
	 */
	int messageTimeToLive() default CacheServer.DEFAULT_MESSAGE_TIME_TO_LIVE;

	/**
	 * Configures the {@link String name} of the Spring bean defined in the Spring application context.
	 *
	 * Defaults to empty.
	 *
	 * This attribute is also used to resolve named {@link CacheServer} properties
	 * from {@literal application.properties} specific to the configuration of this {@link CacheServer} definition,
	 * therefore, this attribute must be specified when external configuration (e.g. {@literal application.properties})
	 * is used.
	 */
	String name() default "";

	/**
	 * Configures the port on which this cache server listens for clients.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_PORT}.
	 *
	 * Use either the {@literal spring.data.gemfire.cache.server.<beanName>.port} property
	 * or the {@literal spring.data.gemfire.cache.server.port} property
	 * in {@literal application.properties}.
	 */
	int port() default CacheServer.DEFAULT_PORT;

	/**
	 * Configures the configured buffer size of the socket connection for this CacheServer.
	 *
	 * Defaults to {@link CacheServer#DEFAULT_SOCKET_BUFFER_SIZE}.
	 *
	 * Use either the {@literal spring.data.gemfire.cache.server.<beanName>.socket-buffer-size} property
	 * or the {@literal spring.data.gemfire.cache.server.socket-buffer-size} property
	 * in {@literal application.properties}.
	 */
	int socketBufferSize() default CacheServer.DEFAULT_SOCKET_BUFFER_SIZE;

	/**
	 * Configures the capacity of the client queue.
	 *
	 * Defaults to {@link ClientSubscriptionConfig#DEFAULT_CAPACITY}.
	 *
	 * Use either the {@literal spring.data.gemfire.cache.server.<beanName>.subscription-capacity} property
	 * or the {@literal spring.data.gemfire.cache.server.subscription-capacity} property
	 * in {@literal application.properties}.
	 */
	int subscriptionCapacity() default ClientSubscriptionConfig.DEFAULT_CAPACITY;

	/**
	 * Configures the disk store name for overflow.
	 *
	 * Use either the {@literal spring.data.gemfire.cache.server.<beanName>.subscription-disk-store-name} property
	 * or the {@literal spring.data.gemfire.cache.server.subscription-disk-store-name} property
	 * in {@literal application.properties}.
	 */
	String subscriptionDiskStoreName() default "";

	/**
	 * Configures the eviction policy that is executed when capacity of the client queue is reached.
	 *
	 * Defaults to {@link SubscriptionEvictionPolicy#NONE}.
	 *
	 * Use either the {@literal spring.data.gemfire.cache.server.<beanName>.subscription-eviction-policy} property
	 * or the {@literal spring.data.gemfire.cache.server.subscription-eviction-policy} property
	 * in {@literal application.properties}.
	 */
	SubscriptionEvictionPolicy subscriptionEvictionPolicy() default SubscriptionEvictionPolicy.NONE;

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
