/*
 * Copyright 2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.config.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.wan.GatewayReceiver;

import org.springframework.context.annotation.Import;

/**
 * The {@link EnableGatewayReceiver} annotation creates a {@link GatewayReceiver} within an Apache Geode
 * or Pivotal GemFire peer {@link Cache}.
 *
 * @author Udo Kohlmeyer
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.wan.GatewayReceiver
 * @see org.apache.geode.cache.wan.GatewayTransportFilter
 * @see org.springframework.context.annotation.Import
 * @since 2.2.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(GatewayReceiverConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableGatewayReceiver {

	/**
	 * The IP address or hostname that the {@link org.apache.geode.cache.wan.GatewayReceiver} communication socket will be bound to.
	 * An empty String will cause the underlying socket to bind to 0.0.0.0<br>
	 * This property can also be configured using the <b><i>spring.data.gemfire.gateway.receiver.bind-address</i></b> property <br>
	 * Default value is {@value GatewayReceiverConfiguration#DEFAULT_BIND_ADDRESS}
	 */
	String bindAddress() default GatewayReceiverConfiguration.DEFAULT_BIND_ADDRESS;

	/**
	 * The hostname or IP Address that the {@link org.apache.geode.cache.wan.GatewaySender} will use to connect and communicate with the
	 * {@link org.apache.geode.cache.wan.GatewayReceiver}. An empty String will cause the system to expose the hostname
	 * to the {@link org.apache.geode.cache.wan.GatewaySender}. Generally this property is set when there are multiple
	 * network interfaces or when they are designated as "internal" or "public" network interfaces. In a cloud environment
	 * the notion of external and internal network interfaces exist and generally the "externally"/outwardly facing network
	 * interface is used.<br>
	 * This property can also be configured using the <b><i>spring.data.gemfire.gateway.receiver.hostname-for-senders</i></b> property<br>
	 * Default value is {@value GatewayReceiverConfiguration#DEFAULT_HOSTNAME_FOR_SENDERS}
	 */
	String hostnameForSenders() default GatewayReceiverConfiguration.DEFAULT_HOSTNAME_FOR_SENDERS;

	/**
	 * A boolean to allow the GatewayReceiver to be created without being started after creation.
	 * If the manualStart is set to <b><i>true</i></b> then the system will create the GatewayReceiver but not start it.
	 * It then becomes the responsibility of the operator to start the GatewayReceiver at a later stage.
	 * If set to <b><i>false</i></b> the GatewayReceiver will start automatically after creation.<br>
	 * This property can also be configured using the <b><i>spring.data.gemfire.gateway.receiver.manual-start</i></b> property<br>
	 * Default is {@value GatewayReceiverConfiguration#DEFAULT_MANUAL_START}
	 */
	boolean manualStart() default GatewayReceiverConfiguration.DEFAULT_MANUAL_START;

	/**
	 * An integer value in milliseconds representing the maximum time which a {@link org.apache.geode.cache.wan.GatewayReceiver}
	 * will wait to receive a ping back from a {@link org.apache.geode.cache.wan.GatewaySender} before the {@link org.apache.geode.cache.wan.GatewayReceiver}
	 * believes the sender to be not available.<br>
	 * This property can also be configured using the <b><i>spring.data.gemfire.gateway.receiver.maximum-time-between-pings</i></b> property<br>
	 * Default value is {@value GatewayReceiverConfiguration#DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS}
	 */
	int maximumTimeBetweenPings() default GatewayReceiverConfiguration.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS;

	/**
	 * The starting port that GatewayReceiver will use when selecting a port to run on. This range of port numbers
	 * is bounded by a <b><i>startPort</i></b> and <b><i>endPort</i></b>.<br>
	 * This property can also be configured using the <b><i>spring.data.gemfire.gateway.receiver.start-port</i></b> property<br>
	 * Default is {@value GatewayReceiverConfiguration#DEFAULT_START_PORT}
	 */
	int startPort() default GatewayReceiverConfiguration.DEFAULT_START_PORT;

	/**
	 * The end port that GatewayReceiver will use when selecting a port to run on. This range of port numbers
	 * is bounded by a <b><i>startPort</i></b> and <b><i>endPort</i></b>.<br>
	 * This property can also be configured using the <b><i>spring.data.gemfire.gateway.receiver.end-port</i></b> property<br>
	 * Default is {@value GatewayReceiverConfiguration#DEFAULT_END_PORT}
	 */
	int endPort() default GatewayReceiverConfiguration.DEFAULT_END_PORT;

	/**
	 * The socket buffer size for the {@link org.apache.geode.cache.wan.GatewayReceiver}. This setting is in bytes.<br>
	 * This property can also be configured using the <b><i>spring.data.gemfire.gateway.receiver.socket-buffer-size</i></b> property<br>
	 * Default value is {@value GatewayReceiverConfiguration#DEFAULT_SOCKET_BUFFER_SIZE}
	 */
	int socketBufferSize() default GatewayReceiverConfiguration.DEFAULT_SOCKET_BUFFER_SIZE;

	/**
	 * An in-order list of {@link org.apache.geode.cache.wan.GatewayTransportFilter} to be applied to
	 * {@link org.apache.geode.cache.wan.GatewayReceiver}<br>
	 * This property can also be configured using the <b><i>spring.data.gemfire.gateway.receiver.transport-filters</i></b> property<br>
	 * Default value is an empty String array
	 */
	String[] transportFilters() default {};

}
