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

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * The {@link EnableMcast} annotation marks a Spring {@link Configuration @Configuration} annotated {@link Class}
 * to configure and enable Pivotal GemFire/Apache Geode's multi-cast networking features.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see org.springframework.context.annotation.Import
 * @see org.springframework.data.gemfire.config.annotation.McastConfiguration
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(McastConfiguration.class)
@UsesGemFireProperties
@SuppressWarnings("unused")
public @interface EnableMcast {

	/**
	 * Address used to discover other members of the distributed system. Only used if {@literal mcast-port} is non-zero.
	 * This attribute must be consistent across the distributed system. Select different multicast addresses
	 * and different ports for different distributed systems. Do not just use different addresses.
	 * Some operating systems may not keep communication separate between systems that use unique addresses
	 * but the same port number.
	 *
	 * This default multicast address was assigned by IANA. Consult the IANA chart when selecting another multicast
	 * address to use with GemFire.
	 *
	 * This setting controls only peer-to-peer communication and does not apply to client/server or multi-site
	 * communication. If multicast is enabled, distributed regions use it for most communication. Partitioned Regions
	 * only use multicast for a few purposes and mainly use either TCP or UDP unicast.
	 *
	 * Defaults to {@literal 239.192.81.1} for IPv4 and {@literal FF38::1234} for IPv6.
	 *
	 * @see <a href="http://www.iana.org/assignments/multicast-addresses">IANA chart</a>
	 */
	String address() default McastConfiguration.DEFAULT_MCAST_ADDRESS;

	/**
	 * Tuning property for flow-of-control protocol for unicast and multicast no-ack UDP messaging. Compound property
	 * is made up of three settings separated by commas: {@literal byteAllowance}, {@literal rechargeThreshold},
	 * and {@literal rechargeBlockMs}.
	 *
	 * Valid values range from these minimums: 10000,0.1,500 to these maximums: no_maximum ,0.5,60000.
	 *
	 * This setting controls only peer-to-peer communication, generally between distributed Regions.
	 *
	 * Defaults to {@literal 1048576,0.25,5000}.
	 */
	String flowControl() default McastConfiguration.DEFAULT_MCAST_FLOW_CONTROL;

	/**
	 * Port used, along with the mcast-address, for multicast communication with other members
	 * of the distributed system.
	 *
	 * If zero, multicast is disabled. If you have values specified for the {@literal locators} property,
	 * the {@literal mcast-port} property defaults to {@literal 0}.
	 *
	 * Valid values are in the range 0..65535.
	 *
	 * Select different multicast addresses and ports for different distributed systems. Do not just use
	 * different addresses. Some operating systems may not keep communication separate between systems
	 * that use unique addresses but the same port number.
	 *
	 * This setting controls only peer-to-peer communication and does not apply to client/server
	 * or multi-site communication.
	 *
	 * Defaults to {@literal 10334}.
	 */
	int port() default McastConfiguration.DEFAULT_MCAST_PORT;

	/**
	 * Size of the socket buffer used for incoming multicast transmissions. You should set this high
	 * if there will be high volumes of messages.
	 *
	 * Valid values are in the range 2048.. OS_maximum.
	 *
	 * The default setting is higher than the default OS maximum buffer size on Unix, which should be increased
	 * to at least 1 megabyte to provide high-volume messaging on Unix systems.
	 *
	 * This setting controls only peer-to-peer communication and does not apply to client/server
	 * or multi-site communication.
	 *
	 * Defaults to {@literal 1048576} bytes.
	 */
	int receiveBufferSize() default McastConfiguration.DEFAULT_MCAST_RECEIVE_BUFFER_SIZE;

	/**
	 * The size of the socket buffer used for outgoing multicast transmissions.
	 *
	 * Valid values are in the range 2048.. OS_maximum.
	 *
	 * This setting controls only peer-to-peer communication and does not apply to client/server
	 * or multi-site communication.
	 *
	 * Defaults to {@literal 65535}.
	 */
	int sendBufferSize() default McastConfiguration.DEFAULT_MCAST_SEND_BUFFER_SIZE;

	/**
	 * How far multicast messaging goes in your network. Lower settings may improve system performance.
	 * A setting of 0 constrains multicast messaging to the machine.
	 *
	 * Defaults to {@literal 32}.
	 */
	int timeToLive() default 32;

}
