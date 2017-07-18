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

import org.springframework.context.annotation.Import;

/**
 * The {@link EnableGemFireProperties} annotation marks a Spring {@link org.springframework.context.annotation.Configuration @Configuration}
 * annotated class to configure GemFire/Geode System properties at runtime during [Spring Boot] application startup.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.GemFirePropertiesConfiguration
 * @see <a href="http://gemfire.docs.pivotal.io/docs-gemfire/reference/topics/gemfire_properties.html">GemFire System Properties</a>
 * @see <a href="http://geode.docs.pivotal.io/docs/reference/topics/gemfire_properties.html">Geode System Properties</a>
 * @since 1.9.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(GemFirePropertiesConfiguration.class)
@UsesGemFireProperties
@SuppressWarnings("unused")
public @interface EnableGemFireProperties {

	/**
	 * Number of seconds the distributed system will wait after the {@literal ack-wait-threshold} for a message
	 * to be acknowledged before it issues an alert at severe level. A value of zero disables this feature.
	 *
	 * Defaults to {@literal 0} seconds.
	 */
	int ackSevereAlertThreshold() default GemFirePropertiesConfiguration.DEFAULT_ACK_SEVERE_ALERT_THRESHOLD;

	/**
	 * Number of seconds a distributed message can wait for acknowledgment before it sends an alert to signal
	 * that something might be wrong with the system member that is unresponsive.
	 *
	 * The waiter continues to wait. The alerts are logged in the system member’s log as warnings.
	 * Valid values are in the range 0…2147483647
	 *
	 * Defaults to {@literal 15} seconds.
	 */
	int ackWaitThreshold() default GemFirePropertiesConfiguration.DEFAULT_ACK_WAIT_THRESHOLD;

	/**
	 * The number of milliseconds a process that is publishing to this process should attempt to distribute
	 * a cache operation before switching over to asynchronous messaging for this process. The switch to
	 * asynchronous messaging lasts until this process catches up, departs, or some specified limit is reached,
	 * such as async-queue-timeout or async-max-queue-size.
	 *
	 * This setting controls only peer-to-peer communication and does not apply to client/server
	 * or multi-site communication.
	 *
	 * Defaults to {@literal 0} milliseconds.
	 */
	long asyncDistributionTimeout() default GemFirePropertiesConfiguration.DEFAULT_ASYNC_DISTRIBUTION_TIMEOUT;

	/**
	 * Affects non-conflated asynchronous queues for members that publish to this member. This is the maximum size
	 * the queue can reach (in megabytes) before the publisher asks this member to leave the distributed system.
	 *
	 * Valid values are in the range 0..1024.
	 *
	 * This setting controls only peer-to-peer communication and does not apply to client/server
	 * or multi-site communication.
	 *
	 * Defaults to {@literal 8} MB.
	 */
	int asyncMaxQueueSize() default GemFirePropertiesConfiguration.DEFAULT_ASYNC_MAX_QUEUE_SIZE;

	/**
	 * Affects asynchronous queues for members that publish to this member. This is the maximum milliseconds
	 * the publisher should wait with no distribution to this member before it asks this member to leave
	 * the distributed system. Used for handling slow receivers.
	 *
	 * This setting controls only peer-to-peer communication and does not apply to client/server
	 * or multi-site communication.
	 *
	 * Defaults to {@literal 60000} milliseconds.
	 */
	long asyncQueueTimeout() default GemFirePropertiesConfiguration.DEFAULT_ASYNC_QUEUE_TIMEOUT;

	/**
	 * Relevant only for multi-homed hosts - machines with multiple network interface cards (NICs).
	 * Specifies the adapter card the cache binds to for peer-to-peer (P2P) communication. Also specifies
	 * the default location for GemFire Servers to listen on, which is used unless overridden by
	 * the {@literal server-bind-address}. An empty string causes the member to listen on the default card
	 * for the machine. This is a machine-wide attribute used for system member and client/server communication.
	 * It has no effect on Locator location, unless the Locator is embedded in a member process.
	 *
	 * Specify the IP address, not the hostname, because each network card may not have a unique hostname.
	 * An empty string (the default) causes the member to listen on the default card for the machine.
	 *
	 * Defaults to unset.
	 */
	String bindAddress() default "";

	/**
	 * Declarative initialization file for the member’s cache.
	 *
	 * Defaults to unset.
	 */
	String cacheXmlFile() default "";

	/**
	 * This property specifies the directory in which the cluster configuration related disk-store and artifacts
	 * are stored. This property is only applicable to dedicated Locators that have {@literal enable-cluster-configuration}
	 * set to {@literal true}.
	 *
	 * Defaults to unset.
	 */
	String clusterConfigurationDirectory() default "";

	/**
	 * Used only by clients in a client/server installation. This is a client-side property
	 * that is passed to the server. Affects subscription queue conflation in this client’s
	 * servers. Specifies whether to conflate (true setting), not conflate (false), or to
	 * use the server’s conflation setting (server).
	 *
	 * Defaults to {@literal server}.
	 */
	String conflateEvents() default GemFirePropertiesConfiguration.DEFAULT_CONFLATE_EVENTS;

	/**
	 * Specifies whether sockets are shared by the system member’s threads. If true, threads share,
	 * and a minimum number of sockets are used to connect to the distributed system. If false,
	 * every application thread has its own sockets for distribution purposes. You can override
	 * this setting for individual threads inside your application. Where possible, it is better to
	 * set {@literal conserve-sockets} to {@literal true} and enable the use of specific extra sockets
	 * in the application code if needed. WAN deployments increase the messaging demands on a GemFire system.
	 * To avoid hangs related to WAN messaging, always set {@literal conserve-sockets} to {@literal false}
	 * for GemFire members that participate in a WAN deployment.
	 *
	 * Defaults to {@literal true}.
	 */
	boolean conserveSockets() default GemFirePropertiesConfiguration.DEFAULT_CONSERVE_SOCKETS;

	/**
	 * Specifies whether to distribute the deltas for entry updates, instead of the full values,
	 * between clients and servers, and between peers.
	 *
	 * Default to {@literal true}.
	 */
	boolean deltaPropagation() default GemFirePropertiesConfiguration.DEFAULT_DELTA_PROPAGATION;

	/**
	 * Working directory used when deploying JAR application files to distributed system members.
	 * This directory can be local and unique to the member or a shared resource.
	 *
	 * Defaults to current working directory of this GemFire JVM process.
	 */
	String deployWorkingDirectory() default GemFirePropertiesConfiguration.DEFAULT_DEPLOY_WORKING_DIRECTORY;

	/**
	 * Boolean indicating whether to disable the use of TCP/IP sockets for inter-cache point-to-point messaging.
	 * If disabled, the cache uses datagram (UDP) sockets.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean disableTcp() default GemFirePropertiesConfiguration.DEFAULT_DISABLE_TCP;

	/**
	 * Identifier used to distinguish messages from different distributed systems.
	 *
	 * Set this to different values for different systems in a multi-site (WAN) configuration. This is required
	 * for Portable Data eXchange (PDX) data serialization. This setting must be the same for every member
	 * in the same distributed system and unique to the distributed system within the WAN installation.
	 *
	 * -1 means no setting. Valid values are integers in the range -1…255.
	 *
	 * Defaults to {@literal -1}.
	 */
	int distributedSystemId() default GemFirePropertiesConfiguration.DEFAULT_DISTRIBUTED_SYSTEM_ID;

	/**
	 * Boolean instructing the system to detect and handle splits in the distributed system, typically caused by
	 * a partitioning of the network (split brain) where the distributed system is running. We recommend setting
	 * this property to {@literal true}. You must set this property to the same value across all your
	 * distributed system members. In addition, you must set this property to {@literal true} if you are using
	 * persistent Regions and configure your Regions to use {@literal DISTRIBUTED_ACK} or {@literal GLOBAL} scope
	 * to avoid potential data conflicts.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean enableNetworkPartitionDetection() default GemFirePropertiesConfiguration.DEFAULT_ENABLE_NETWORK_PARTITION_DETECTION;

	/**
	 * Whether partitioned regions will put redundant copies of the same data in different members
	 * running on the same physical machine. By default, GemFire tries to put redundant copies on different machines,
	 * but it will put them on the same machine if no other machines are available. Setting this property
	 * to {@literal true} prevents this and requires different machines for redundant copies.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean enforceUniqueHost() default GemFirePropertiesConfiguration.DEFAULT_ENFORCE_UNIQUE_HOST;

	/**
	 * Defines the list of groups that this member belongs to. Use commas to separate group names.
	 * Note that anything defined by the {@literal roles} gemfire property will also be considered
	 * a group.
	 *
	 * Default to unset.
	 */
	String[] groups() default {};

	/**
	 * Setting this property to {@literal true} causes loading of cluster configuration from the
	 * {@literal cluster_config} directory in the Locator. This property is only applicable to dedicated Locators
	 * that have {@literal enable-cluster-configuration} set to {@literal true}.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean loadClusterConfigurationFromDirectory() default GemFirePropertiesConfiguration.DEFAULT_LOAD_CLUSTER_CONFIGURATION_FROM_DIRECTORY;

	/**
	 * The number of seconds that a member should wait for a Locator to start if a Locator is not available
	 * when attempting to join the distributed system. Use this setting when you are starting Locators
	 * and peers all at once. This timeout allows peers to wait for the Locators to finish starting up
	 * before attempting to join the distributed system.
	 *
	 * Defaults to {@literal 0} seconds.
	 */
	long locatorWaitTimeout() default GemFirePropertiesConfiguration.DEFAULT_LOCATOR_WAIT_TIME;

	/**
	 * When true, locks heap and off-heap memory into RAM to prevent the operating system from paging the memory
	 * out to disk.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean lockMemory() default GemFirePropertiesConfiguration.DEFAULT_LOCK_MEMORY;

	/**
	 * Maximum number of milliseconds to wait for the distributed system to reconnect on each reconnect attempt.
	 *
	 * {@link PeerCacheApplication#enableAutoReconnect()} or {@link CacheServerApplication#enableAutoReconnect()}
	 * must be set to {@literal true} for this property to have any effect.
	 *
	 * Defaults to {@literal 60000} milliseconds.
	 */
	long maxWaitTimeReconnect() default GemFirePropertiesConfiguration.DEFAULT_MAX_WAIT_TIME_RECONNECT;

	/**
	 * GemFire uses the {@literal member-timeout} server configuration, specified in milliseconds, to detect
	 * the abnormal termination of members. The configuration setting is used in two ways:
	 *
	 * 1) First, it is used during the UDP heartbeat detection process. When a member detects that
	 * a heartbeat datagram is missing from the member that it is monitoring after the time interval
	 * of 2 * the value of member-timeout, the detecting member attempts to form a TCP/IP stream-socket connection
	 * with the monitored member as described in the next case.
	 *
	 * 2) The property is then used again during the TCP/IP stream-socket connection. If the suspected process
	 * does not respond to the are you alive datagram within the time period specified in member-timeout,
	 * the membership coordinator sends out a new membership view that notes the member’s failure.
	 *
	 * Valid values are in the range 1000..600000.
	 *
	 * Defaults to {@literal 5000} milliseconds.
	 */
	long memberTimeout() default GemFirePropertiesConfiguration.DEFAULT_MEMBER_TIMEOUT;

	/**
	 * The range of ports available for unicast UDP messaging and for TCP failure detection. This is specified
	 * as two integers separated by a hyphen. Different members can use different ranges.
	 *
	 * GemFire randomly chooses at least two unique integers from this range for the member, one for
	 * UDP unicast messaging and the other for TCP failure detection messaging. If {@literal tcp-port}
	 * is configured to {@literal 0}, it will also randomly select a port from this range for TCP sockets
	 * used for peer-to-peer communication only.
	 *
	 * Therefore, the specified range must include at least three available port numbers (UDP, FD_SOCK,
	 * and TCP DirectChannel).
	 *
	 * The system uniquely identifies the member using the combined host IP address and UDP port number.
	 *
	 * You may want to restrict the range of ports that GemFire uses so the product can run in an environment
	 * where routers only allow traffic on certain ports.
	 *
	 * Defaults to {@literal 1024-65535}.
	 */
	String membershipPortRange() default GemFirePropertiesConfiguration.DEFAULT_MEMBERSHIP_PORT_RANGE;

	/**
	 * Defines this member’s redundancy zone. Used to separate member’s into different groups for satisfying
	 * Partitioned Region redundancy. If this property is set, GemFire will not put redundant copies of data
	 * in members with the same redundancy zone setting.
	 *
	 * Defaults to unset.
	 *
	 * @see <a href="http://geode.docs.pivotal.io/docs/developing/partitioned_regions/configuring_ha_for_pr.html">Configure High Availability for a Partitioned Region</a>
	 */
	String redundancyZone() default "";

	/**
	 * Used to configure the Locators that a cluster will use in order to connect to a remote site in a multi-site
	 * (WAN) configuration.
	 *
	 * To use Locators in a WAN configuration, you must specify a unique distributed system ID
	 * ({@literal distributed-system-id}) for the local cluster and remote Locator(s) for the remote clusters
	 * to which you will connect.
	 *
	 * For each remote Locator, provide a host name and/or address (separated by ‘@’, if you use both),
	 * followed by a port number in brackets.
	 *
	 * Examples:
	 *
	 * <pre>
	 *     <code>
	 * remote-locators=address1[port1],address2[port2]
	 *
	 * remote-locators=hostName1@address1[port1],hostName2@address2[port2]
	 *
	 * remote-locators=hostName1[port1],hostName2[port2]
	 *     </code>
	 * </pre>
	 *
	 * Defaults to unset.
	 */
	String remoteLocators() default "";

	/**
	 * When this property is set to {@literal true}, the primary server drops unresponsive clients
	 * from all secondaries and itself. Clients are deemed unresponsive when their messaging queues
	 * become full on the server. While a client’s queue is full, puts that would add to the queue
	 * block on the server.
	 *
	 * Defaults to {@literal false}.
	 */
	boolean removeUnresponsiveClient() default GemFirePropertiesConfiguration.DEFAULT_REMOVE_UNRESPONSIVE_CLIENT;

	/**
	 * Receive buffer sizes in bytes of the TCP/IP connections used for data transmission. To minimize the buffer size
	 * allocation needed for distributing large, serializable messages, the messages are sent in chunks. This setting
	 * determines the size of the chunks. Larger buffers can handle large messages more quickly, but take up
	 * more memory.
	 *
	 * Defaults to {@literal 32768} bytes.
	 */
	int socketBufferSize() default GemFirePropertiesConfiguration.DEFAULT_SOCKET_BUFFER_SIZE;

	/**
	 * Time, in milliseconds, a thread can have exclusive access to a socket it is not actively using.
	 * A value of zero causes socket leases to never expire. This property is ignored
	 * if {@literal conserve-sockets} is {@literal true}.
	 *
	 * Valid values are in the range 0..600000.
	 *
	 * Defaults to {@literal 60000} milliseconds.
	 */
	long socketLeaseTime() default GemFirePropertiesConfiguration.DEFAULT_SOCKET_LEASE_TIME;

	/**
	 * The TCP port to listen on for cache communications. If set to zero, the operating system selects
	 * an available port. Each process on a machine must have its own TCP port. Note that some operating systems
	 * restrict the range of ports usable by non-privileged users, and using restricted port numbers can cause
	 * runtime errors in GemFire startup.
	 *
	 * Valid values are in the range 0..65535.
	 *
	 * Defaults to {@literal 0}.
	 */
	int tcpPort() default GemFirePropertiesConfiguration.DEFAULT_TCP_PORT;

	/**
	 * The number of tombstones that can accumulate before the GemFire member triggers garbage collection
	 * for tombstones.
	 *
	 * Defaults to {@literal 100000} tombstones.
	 *
	 * @see <a href="http://geode.docs.pivotal.io/docs/developing/distributed_regions/how_region_versioning_works.html#topic_321B05044B6641FCAEFABBF5066BD399">How Destroy and Clear Operations Are Resolved</a>
	 */
	int tombstoneGcThreshold() default GemFirePropertiesConfiguration.DEFAULT_TOMBSTONE_THRESHOLD;

	/**
	 * Maximum fragment size, in bytes, for transmission over UDP unicast or multicast sockets. Smaller messages
	 * are combined, if possible, for transmission up to the fragment size setting.
	 *
	 * Valid values are in the range 1000..60000.
	 *
	 * Defaults to {@literal 60000} bytes.
	 */
	int udpFragmentSize() default GemFirePropertiesConfiguration.DEFAULT_UDP_FRAGMENT_SIZE;

	/**
	 * The size of the socket buffer used for incoming UDP point-to-point transmissions. If {@literal disable-tcp}
	 * is {@literal false}, a reduced buffer size of 65535 is used by default.
	 *
	 * The default setting of 1048576 is higher than the default OS maximum buffer size on Unix, which should be
	 * increased to at least 1 megabyte to provide high-volume messaging on Unix systems.
	 *
	 * Valid values are in the range 2048.. OS_maximum.
	 *
	 * Defaults to {@literal 1048576} bytes.
	 */
	int udpReceiveBufferSize() default GemFirePropertiesConfiguration.DEFAULT_UDP_RECEIVE_BUFFER_SIZE;

	/**
	 * The size of the socket buffer used for outgoing UDP point-to-point transmissions.
	 *
	 * Valid values are in the range 2048..OS_maximum.
	 *
	 * Defaults to {@literal 65535} bytes.
	 */
	int udpSendBufferSize() default GemFirePropertiesConfiguration.DEFAULT_UDP_SEND_BUFFER_SIZE;

	/**
	 * A comma separated list of Java packages that contain classes implementing the Spring Shell
	 * CommandMarker interface.
	 *
	 * Matching classes will be loaded when the VM starts and will be available in the GFSH command-line utility.
	 *
	 * Defaults to unset.
	 */
	String userCommandPackages() default "";

}
