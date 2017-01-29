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

import java.util.Map;
import java.util.Properties;

import org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport;
import org.springframework.data.gemfire.util.PropertiesBuilder;

/**
 * The {@link GemFirePropertiesConfiguration} class is a Spring {@link org.springframework.context.annotation.ImportBeanDefinitionRegistrar}
 * capable of configuring additional GemFire Properties on a (Spring Boot) application class at runtime
 * during startup.
 *
 * @author John Blum
 * @see org.springframework.data.gemfire.config.annotation.EnableGemFireProperties
 * @see org.springframework.data.gemfire.config.annotation.support.EmbeddedServiceConfigurationSupport
 * @see org.springframework.data.gemfire.util.PropertiesBuilder
 * @since 1.9.0
 */
public class GemFirePropertiesConfiguration extends EmbeddedServiceConfigurationSupport {

	public static final boolean DEFAULT_CONSERVE_SOCKETS = true;
	public static final boolean DEFAULT_DELTA_PROPAGATION = true;
	public static final boolean DEFAULT_DISABLE_TCP = false;
	public static final boolean DEFAULT_ENABLE_NETWORK_PARTITION_DETECTION = false;
	public static final boolean DEFAULT_ENFORCE_UNIQUE_HOST = false;
	public static final boolean DEFAULT_LOAD_CLUSTER_CONFIGURATION_FROM_DIRECTORY = false;
	public static final boolean DEFAULT_LOCK_MEMORY = false;
	public static final boolean DEFAULT_REMOVE_UNRESPONSIVE_CLIENT = false;

	public static final int DEFAULT_ACK_SEVERE_ALERT_THRESHOLD = 0;
	public static final int DEFAULT_ACK_WAIT_THRESHOLD = 15;
	public static final int DEFAULT_ASYNC_MAX_QUEUE_SIZE = 8;
	public static final int DEFAULT_DISTRIBUTED_SYSTEM_ID = -1;
	public static final int DEFAULT_SOCKET_BUFFER_SIZE = 32768;
	public static final int DEFAULT_TCP_PORT = 0;
	public static final int DEFAULT_TOMBSTONE_THRESHOLD = 100000;
	public static final int DEFAULT_UDP_FRAGMENT_SIZE = 60000;
	public static final int DEFAULT_UDP_RECEIVE_BUFFER_SIZE = 1048576;
	public static final int DEFAULT_UDP_SEND_BUFFER_SIZE = 65535;

	public static final long DEFAULT_ASYNC_DISTRIBUTION_TIMEOUT = 0L;
	public static final long DEFAULT_ASYNC_QUEUE_TIMEOUT = 60000L;
	public static final long DEFAULT_LOCATOR_WAIT_TIME = 0L;
	public static final long DEFAULT_MAX_WAIT_TIME_RECONNECT = 60000L;
	public static final long DEFAULT_MEMBER_TIMEOUT = 5000L;
	public static final long DEFAULT_SOCKET_LEASE_TIME = 60000L;

	public static final String DEFAULT_CONFLATE_EVENTS = "server";
	public static final String DEFAULT_DEPLOY_WORKING_DIRECTORY = ".";
	public static final String DEFAULT_MEMBERSHIP_PORT_RANGE = "1024-65535";

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Class getAnnotationType() {
		return EnableGemFireProperties.class;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	protected Properties toGemFireProperties(Map<String, Object> annotationAttributes) {
		PropertiesBuilder gemfireProperties = new PropertiesBuilder();

		gemfireProperties.setPropertyIfNotDefault("ack-severe-alert-threshold",
			annotationAttributes.get("ackSevereAlertThreshold"), DEFAULT_ACK_SEVERE_ALERT_THRESHOLD);

		gemfireProperties.setPropertyIfNotDefault("ack-wait-threshold",
			annotationAttributes.get("ackWaitThreshold"), DEFAULT_ACK_WAIT_THRESHOLD);

		gemfireProperties.setPropertyIfNotDefault("async-distribution-timeout",
			annotationAttributes.get("asyncDistributionTimeout"), DEFAULT_ASYNC_DISTRIBUTION_TIMEOUT);

		gemfireProperties.setPropertyIfNotDefault("async-max-queue-size",
			annotationAttributes.get("asyncMaxQueueSize"), DEFAULT_ASYNC_MAX_QUEUE_SIZE);

		gemfireProperties.setPropertyIfNotDefault("async-queue-timeout",
			annotationAttributes.get("asyncQueueTimeout"), DEFAULT_ASYNC_QUEUE_TIMEOUT);

		gemfireProperties.setProperty("bind-address", annotationAttributes.get("bindAddress"));

		gemfireProperties.setProperty("cache-xml-file", annotationAttributes.get("cacheXmlFile"));

		gemfireProperties.setProperty("cluster-configuration-dir",
			annotationAttributes.get("clusterConfigurationDirectory"));

		gemfireProperties.setPropertyIfNotDefault("conflate-events",
			annotationAttributes.get("conflateEvents"), DEFAULT_CONFLATE_EVENTS);

		gemfireProperties.setPropertyIfNotDefault("conserve-sockets",
			annotationAttributes.get("conserveSockets"), DEFAULT_CONSERVE_SOCKETS);

		gemfireProperties.setPropertyIfNotDefault("delta-propagation",
			annotationAttributes.get("deltaPropagation"), DEFAULT_DELTA_PROPAGATION);

		gemfireProperties.setPropertyIfNotDefault("deploy-working-dir",
			annotationAttributes.get("deployWorkingDirectory"), DEFAULT_DEPLOY_WORKING_DIRECTORY);

		gemfireProperties.setPropertyIfNotDefault("disable-tcp",
			annotationAttributes.get("disableTcp"), DEFAULT_DISABLE_TCP);

		gemfireProperties.setPropertyIfNotDefault("distributed-system-id",
			annotationAttributes.get("distributedSystemId"), DEFAULT_DISTRIBUTED_SYSTEM_ID);

		gemfireProperties.setPropertyIfNotDefault("enable-network-partition-detection",
			annotationAttributes.get("enableNetworkPartitionDetection"), DEFAULT_ENABLE_NETWORK_PARTITION_DETECTION);

		gemfireProperties.setPropertyIfNotDefault("enforce-unique-host",
			annotationAttributes.get("enforceUniqueHost"), DEFAULT_ENFORCE_UNIQUE_HOST);

		gemfireProperties.setProperty("groups", (String[]) annotationAttributes.get("groups"));

		gemfireProperties.setPropertyIfNotDefault("load-cluster-configuration-from-dir",
			annotationAttributes.get("loadClusterConfigurationFromDirectory"),
				DEFAULT_LOAD_CLUSTER_CONFIGURATION_FROM_DIRECTORY);

		gemfireProperties.setPropertyIfNotDefault("locator-wait-time",
			annotationAttributes.get("locatorWaitTimeout"), DEFAULT_LOCATOR_WAIT_TIME);

		gemfireProperties.setPropertyIfNotDefault("lock-memory",
			annotationAttributes.get("lockMemory"), DEFAULT_LOCK_MEMORY);

		gemfireProperties.setPropertyIfNotDefault("max-wait-time-reconnect",
			annotationAttributes.get("maxWaitTimeReconnect"), DEFAULT_MAX_WAIT_TIME_RECONNECT);

		gemfireProperties.setPropertyIfNotDefault("member-timeout",
			annotationAttributes.get("memberTimeout"), DEFAULT_MEMBER_TIMEOUT);

		gemfireProperties.setPropertyIfNotDefault("membership-port-range",
			annotationAttributes.get("membershipPortRange"), DEFAULT_MEMBERSHIP_PORT_RANGE);

		gemfireProperties.setProperty("redundancy-zone", annotationAttributes.get("redundancyZone"));

		gemfireProperties.setProperty("remote-locators", annotationAttributes.get("remoteLocators"));

		gemfireProperties.setPropertyIfNotDefault("remove-unresponsive-client",
			annotationAttributes.get("removeUnresponsiveClient"), DEFAULT_REMOVE_UNRESPONSIVE_CLIENT);

		gemfireProperties.setPropertyIfNotDefault("socket-buffer-size",
			annotationAttributes.get("socketBufferSize"), DEFAULT_SOCKET_BUFFER_SIZE);

		gemfireProperties.setPropertyIfNotDefault("socket-lease-time",
			annotationAttributes.get("socketLeaseTime"), DEFAULT_SOCKET_LEASE_TIME);

		gemfireProperties.setPropertyIfNotDefault("tcp-port", annotationAttributes.get("tcpPort"), DEFAULT_TCP_PORT);

		gemfireProperties.setPropertyIfNotDefault("tombstone-gc-threshold",
			annotationAttributes.get("tombstoneGcThreshold"), DEFAULT_TOMBSTONE_THRESHOLD);

		gemfireProperties.setPropertyIfNotDefault("udp-fragment-size",
			annotationAttributes.get("udpFragmentSize"), DEFAULT_UDP_FRAGMENT_SIZE);

		gemfireProperties.setPropertyIfNotDefault("udp-recv-buffer-size",
			annotationAttributes.get("udpReceiveBufferSize"), DEFAULT_UDP_RECEIVE_BUFFER_SIZE);

		gemfireProperties.setPropertyIfNotDefault("udp-send-buffer-size",
			annotationAttributes.get("udpSendBufferSize"), DEFAULT_UDP_SEND_BUFFER_SIZE);

		gemfireProperties.setProperty("user-command-packages", annotationAttributes.get("userCommandPackages"));

		return gemfireProperties.build();
	}
}
