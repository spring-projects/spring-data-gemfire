/*
 * Copyright 2010-2019 the original author or authors.
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

import java.lang.annotation.Annotation;
import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.apache.geode.cache.DiskStore;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.server.CacheServer;
import org.apache.geode.cache.wan.GatewayEventFilter;
import org.apache.geode.cache.wan.GatewayEventSubstitutionFilter;
import org.apache.geode.cache.wan.GatewayReceiver;
import org.apache.geode.cache.wan.GatewaySender;
import org.apache.geode.cache.wan.GatewayTransportFilter;

import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.config.support.GatewaySenderBeanFactoryPostProcessor;
import org.springframework.data.gemfire.wan.OrderPolicyType;

/**
 * This {@link Annotation} is responsible for the configuration of multiple {@link GatewaySender GatewaySenders}.
 *
 * All properties set with this {@link Annotation} will be used as the default values for all
 * {@link GatewaySender GatewaySenders} configured within the <b><i>gatewaySenders</i></b> property.
 *
 * @author Udo Kohlmeyer
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see java.lang.annotation.Documented
 * @see java.lang.annotation.Inherited
 * @see java.lang.annotation.Retention
 * @see java.lang.annotation.Target
 * @see org.apache.geode.cache.wan.GatewayEventFilter
 * @see org.apache.geode.cache.wan.GatewayEventSubstitutionFilter
 * @see org.apache.geode.cache.wan.GatewayReceiver
 * @see org.apache.geode.cache.wan.GatewaySender
 * @see org.apache.geode.cache.wan.GatewaySender.OrderPolicy
 * @see org.apache.geode.cache.wan.GatewayTransportFilter
 * @see org.springframework.data.gemfire.config.annotation.EnableGatewaySender
 * @since 2.2.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import({ GatewaySenderBeanFactoryPostProcessor.class, GatewaySendersConfiguration.class })
@SuppressWarnings("unused")
public @interface EnableGatewaySenders {

	/**
	 * Configures the time, in milliseconds, that an object can be in the queue to be replicated before the
	 * configured {@link GatewaySender GatewwaySenders} log an alert.
	 *
	 * Defaults to {@link GatewaySenderConfiguration#DEFAULT_ALERT_THRESHOLD}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.alert-threshold} property
	 * in {@literal application.properties}.
	 */
	int alertThreshold() default GatewaySenderConfiguration.DEFAULT_ALERT_THRESHOLD;

	/**
	 * A {@literal boolean} flag to indicate if the configured {@link GatewaySender GatewaySenders} should use conflate
	 * entries in each batch. This means, that a batch will never contain duplicate entries, as the batch will always
	 * only contain the latest value for a key.
	 *
	 * Defaults to {@value GatewaySenderConfiguration#DEFAULT_BATCH_CONFLATION_ENABLED}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.batch-conflation-enabled} property
	 * in {@literal application.properties}.
	 */
	boolean batchConflationEnabled() default GatewaySenderConfiguration.DEFAULT_BATCH_CONFLATION_ENABLED;

	/**
	 * Configures the maximum batch size that all configured {@link GatewaySender GatewaySenders} sends to
	 * the remote site.
	 *
	 * This property works in conjunction with the {@link EnableGatewaySenders#batchTimeInterval()} setting.
	 * A {@link GatewaySender} will send when either the {@literal batch-size} or {@literal batch-time-interval}
	 * is reached.
	 *
	 * Defaults to {@value GatewaySenderConfiguration#DEFAULT_BATCH_SIZE}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.batch-size} property
	 * in {@literal application.properties}.
	 */
	int batchSize() default GatewaySenderConfiguration.DEFAULT_BATCH_SIZE;

	/**
	 * Configures the maximum batch time interval in milliseconds that all configured {@link GatewaySender GatewaySenders}
	 * wait before attempting to send a batch of queued objects to the remote {@link GatewayReceiver GatewayReceivers}.
	 *
	 * This property works in conjunction with the {@link EnableGatewaySenders#batchSize()} setting.
	 * A {@link GatewaySender} will send when either the {@literal batch-size} or {@literal batch-time-interval}
	 * is reached.
	 *
	 * Defaults to {@value GatewaySenderConfiguration#DEFAULT_BATCH_TIME_INTERVAL}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.batch-time-interval} property
	 * in {@literal application.properties}.
	 */
	int batchTimeInterval() default GatewaySenderConfiguration.DEFAULT_BATCH_TIME_INTERVAL;

	/**
	 * Configures the {@link DiskStore} used by all configured {@link }GatewaySender GatewaySenders} when persisting
	 * the {@link GatewaySender GatewaySenders} queue data.
	 *
	 * This setting should be set when the {@link EnableGatewaySender#persistent()} property is set to {@literal true}.
	 *
	 * Defaults to {@value GatewaySenderConfiguration#DEFAULT_DISK_STORE_REFERENCE}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.diskstore-reference} property
	 * in {@literal application.properties}.
	 */
	String diskStoreReference() default GatewaySenderConfiguration.DEFAULT_DISK_STORE_REFERENCE;

	/**
	 * A {@literal boolean} flag indicating whether all configured {@link GatewaySender GatewaySenders} should use
	 * synchronous {@link DiskStore} writes.
	 *
	 * Defaults to {@value GatewaySenderConfiguration#DEFAULT_DISK_SYNCHRONOUS}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.disk-synchronous} property
	 * in {@literal application.properties}.
	 */
	boolean diskSynchronous() default GatewaySenderConfiguration.DEFAULT_DISK_SYNCHRONOUS;

	/**
	 * Configures the number of dispatcher threads that all configured {@link GatewaySender GatewaySenders} will try
	 * to use to dispatch the queued events.
	 *
	 * Defaults to {@value GatewaySenderConfiguration#DEFAULT_DISPATCHER_THREADS}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.dispatcher-threads} property
	 * in {@literal application.properties}.
	 */
	int dispatcherThreads() default GatewaySenderConfiguration.DEFAULT_DISPATCHER_THREADS;

	/**
	 * Configures the list of {@link GatewayEventFilter GatewayEventFilters} to be applied to all configured
	 * {@link GatewaySender GatewaySenders}.
	 *
	 * {@link GatewayEventFilter GatewayEventFilters} are used to filter out objects from the sending queue before
	 * dispatching them to remote {@link GatewayReceiver GatewayReceivers}.
	 *
	 * Defaults to empty list.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.event-filters} property
	 * in {@literal application.properties}.
	 */
	String[] eventFilters() default {};

	/**
	 * Configures the {@link GatewayEventSubstitutionFilter} used by all configured {@link GatewaySender GatewaySenders}.
	 *
	 * The {@link GatewayEventSubstitutionFilter} is used to replace values on objects before they are enqueued
	 * for remote replication.
	 *
	 * Defaults to {@link GatewaySenderConfiguration#DEFAULT_EVENT_SUBSTITUTION_FILTER}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.event-substitution-filter} property
	 * in {@literal application.properties}.
	 */
	String eventSubstitutionFilter() default GatewaySenderConfiguration.DEFAULT_EVENT_SUBSTITUTION_FILTER;

	/**
	 * The list of {@link GatewaySender GatewaySenders} to be configured.
	 *
	 * If no {@link GatewaySender GatewaySenders) are configured, a default {@link GatewaySender} will be created
	 * using the properties provided.
	 */
	EnableGatewaySender[] gatewaySenders() default {};

	/**
	 * A {@literal boolean} flag indicating whether all configured {@link GatewaySender GatewaySenders} should be
	 * started automatically.
	 *
	 * <p>Defaults to {@value @EnableGatewaySenderConfiguration.DEFAULT_MANUAL_START}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.manual-start} property
	 * in {@literal application.properties}.
	 */
	boolean manualStart() default GatewaySenderConfiguration.DEFAULT_MANUAL_START;

	/**
	 * Configures the maximum size in megabytes that all configured {@link GatewaySender GatewaySenders} queue may take
	 * in heap memory before overflowing to disk.
	 *
	 * Defaults to {@value GatewaySenderConfiguration#DEFAULT_MAXIMUM_QUEUE_MEMORY}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.maximum-queue-memory} property
	 * in {@literal application.properties}.	 */
	int maximumQueueMemory() default GatewaySenderConfiguration.DEFAULT_MAXIMUM_QUEUE_MEMORY;

	/**
	 * Configures the ordering policy that all configured {@link GatewaySender GatewaySenders} will use when queueing
	 * entries to be replicated to a remote {@link GatewayReceiver}.
	 *
	 * <p>There are three different ordering policies:
	 *
	 * <ul>
	 * <li>{@link OrderPolicyType#KEY} - Order of events preserved on a per key basis</li>
	 * <li>{@link OrderPolicyType#THREAD} - Order of events preserved by the thread that added the event</li>
	 * <li>{@link OrderPolicyType#PARTITION} - Order of events is preserved in order that they arrived in partitioned Region</li>
	 * </ul>
	 *
	 * Defaults to {@link OrderPolicyType#KEY}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.order-policy} property
	 * in {@literal application.properties}.
	 */
	OrderPolicyType orderPolicy() default OrderPolicyType.KEY;

	/**
	 * A {@literal }boolean} flag indicating whether all configured {@link GatewaySender GatewaySenders} should use
	 * parallel replication.
	 *
	 * Parallel replication means that each {@link CacheServer} that defines a {@link GatewaySender} will send data
	 * to a remote {@link GatewayReceiver}.
	 *
	 * Defaults to {@value GatewaySenderConfiguration#DEFAULT_PARALLEL}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.parallel} property
	 * in {@literal application.properties}.
	 */
	boolean parallel() default GatewaySenderConfiguration.DEFAULT_PARALLEL;

	/**
	 * A {@literal boolean} flag indicating whether all configured {@link GatewaySender GatewaySenders} should use
	 * persistence.
	 *
	 * This setting should be used in conjunction with the {@literal disk-store-reference} property.
	 *
	 * Defaults to {@value GatewaySenderConfiguration#DEFAULT_PERSISTENT}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.persistent} property
	 * in {@literal application.properties}.
	 */
	boolean persistent() default GatewaySenderConfiguration.DEFAULT_PERSISTENT;

	/**
	 * Configures the list of {@link Region} names that will be configured for all {@link GatewaySender GatewaySenders}.
	 *
	 * An empty list denotes that ALL {@link Region Regions} are to be replicated to the remote {@link GatewayReceiver}.
	 *
	 * Defaults to empty list.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.region-names} property
	 * in {@literal application.properties}.
	 */
	String[] regions() default {};

	/**
	 * Configures the id of the remote distributed system (cluster) that all configured
	 * {@link GatewaySender GatewaySenders} will send their data to.
	 *
	 * Defaults to {@value @EnableGatewaySenderConfiguration.DEFAULT_REMOTE_DISTRIBUTED_SYSTEM_ID}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.remote-distributed-system-id} property
	 * in {@literal application.properties}.
	 */
	int remoteDistributedSystemId() default GatewaySenderConfiguration.DEFAULT_REMOTE_DISTRIBUTED_SYSTEM_ID;

	/**
	 * Configures the socket buffer size in bytes for all configured {@link GatewaySender GatewaySenders}.
	 *
	 * Defaults to {@value GatewaySenderConfiguration#DEFAULT_SOCKET_BUFFER_SIZE}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.socket-buffer-size} property
	 * in {@literal application.properties}.
	 */
	int socketBufferSize() default GatewaySenderConfiguration.DEFAULT_SOCKET_BUFFER_SIZE;

	/**
	 * Configures the amount of time in milliseconds that all configured {@link GatewaySender GatewaySenders} will wait
	 * to receive an acknowledgment from a remote site.
	 *
	 * By default this is set to {@literal 0}, which means there is no timeout. The minimum allowed timeout
	 * is {@literal 30000 (milliseconds)}.
	 *
	 * Defaults to {@value GatewaySenderConfiguration#DEFAULT_SOCKET_READ_TIMEOUT}.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.socket-read-timeout} property
	 * in {@literal application.properties}.
	 */
	int socketReadTimeout() default GatewaySenderConfiguration.DEFAULT_SOCKET_READ_TIMEOUT;

	/**
	 * Configures an in-order list of {@link GatewayTransportFilter} objects to be applied to all configured
	 * {@link GatewaySender GatewaySenders}.
	 *
	 * Defaults to empty list.
	 *
	 * Alternatively use the {@literal spring.data.gemfire.gateway.sender.transport-filters} property
	 * in {@literal application.properties}.
	 */
	String[] transportFilters() default {};

}
