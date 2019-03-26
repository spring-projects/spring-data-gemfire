package org.springframework.data.gemfire.config.annotation;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.config.support.GatewaySenderBeanFactoryPostProcessor;
import org.springframework.data.gemfire.wan.OrderPolicyType;

/**
 * This annotation is responsible for the configuration of a single {@link org.apache.geode.cache.wan.GatewaySender}.
 * All properties configured on this annotation will be be overrides from the defaults set on the {@link EnableGatewaySenders}.
 *
 * @author Udo Kohlmeyer
 * @see EnableGatewaySenders
 * @see org.apache.geode.cache.wan.GatewaySender
 * @see org.apache.geode.cache.wan.GatewayReceiver
 * @see org.apache.geode.cache.wan.GatewayEventFilter
 * @see org.apache.geode.cache.wan.GatewayTransportFilter
 * @see org.apache.geode.cache.wan.GatewaySender.OrderPolicy
 * @see org.apache.geode.cache.wan.GatewayEventSubstitutionFilter
 * @since 2.2.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import({ GatewaySenderBeanFactoryPostProcessor.class, GatewaySenderConfiguration.class })
@SuppressWarnings("unused")
public @interface EnableGatewaySender {

	/**
	 * This property configures the time, in milliseconds, that an object can be in the queue to be replicated before the
	 * {@link org.apache.geode.cache.wan.GatewaySender} logs an alert.
	 * <p> This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.alert-threshold}
	 * <p>Default value is {@link GatewaySenderConfiguration#DEFAULT_ALERT_THRESHOLD}
	 */
	int alertThreshold() default GatewaySenderConfiguration.DEFAULT_ALERT_THRESHOLD;

	/**
	 * A boolean flag to indicate if the configured GatewaySender(s) should use conflate entries in each batch. This means,
	 * that a batch will never contain duplicate entries, as the batch will always only contain the latest value for a key.
	 * <p> This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.batch-conflation-enabled}
	 * <p>Default value is {@value GatewaySenderConfiguration#DEFAULT_BATCH_CONFLATION_ENABLED}
	 */
	boolean batchConflationEnabled() default GatewaySenderConfiguration.DEFAULT_BATCH_CONFLATION_ENABLED;

	/**
	 * This property configures the maximum batch size that the {@link org.apache.geode.cache.wan.GatewaySender} send to the
	 * remote site. This property works in conjunction with the {@link EnableGatewaySenders#batchTimeInterval()} setting.
	 * The {@link org.apache.geode.cache.wan.GatewaySender} will send when either the {@literal batch-size} or {@literal batch-time-interval}
	 * is met.
	 * <p> This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.batch-size}
	 * <p>Default value is {@value GatewaySenderConfiguration#DEFAULT_BATCH_SIZE}
	 */
	int batchSize() default GatewaySenderConfiguration.DEFAULT_BATCH_SIZE;

	/**
	 * This property configures the maximum batch time interval, in milliseconds, that the {@link org.apache.geode.cache.wan.GatewaySender} wait before
	 * attempting to send a batch of queued object to the remote {@link org.apache.geode.cache.wan.GatewayReceiver}.
	 * <p>This property works in conjunction with the {@link EnableGatewaySenders#batchSize()} setting.
	 * The {@link org.apache.geode.cache.wan.GatewaySender} will send when either the {@literal batch-size} or {@literal batch-time-interval}
	 * is met.
	 * <p> This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.batch-time-interval}
	 * <p>Default value is {@value GatewaySenderConfiguration#DEFAULT_BATCH_TIME_INTERVAL}
	 */
	int batchTimeInterval() default GatewaySenderConfiguration.DEFAULT_BATCH_TIME_INTERVAL;

	/**
	 * This property configures what {@link org.apache.geode.cache.DiskStore} the GatewaySender(s) are to use when persisting
	 * the GatewaySender(s) queues. This setting should be set when the {@literal persistent} property is set to {@literal true}.
	 * <p> This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.diskstore-reference}
	 * <p>Default value is {@value GatewaySenderConfiguration#DEFAULT_DISK_STORE_REFERENCE}
	 */
	String diskStoreReference() default GatewaySenderConfiguration.DEFAULT_DISK_STORE_REFERENCE;

	/**
	 * A boolean flag to indicate if the configured GatewaySender(s) should use synchronous diskstore writes.
	 * <p> This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.disk-synchronous}
	 * <p>Default value is {@value GatewaySenderConfiguration#DEFAULT_DISK_SYNCHRONOUS}
	 */
	boolean diskSynchronous() default GatewaySenderConfiguration.DEFAULT_DISK_SYNCHRONOUS;

	/**
	 * This property configures the number of dispatcher threads that the {@link org.apache.geode.cache.wan.GatewaySender}
	 * will try to use to dispatch the queuing object.
	 * <p>This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.dispatcher-threads}
	 * <p>Default value is {@value GatewaySenderConfiguration#DEFAULT_DISPATCHER_THREADS}
	 */
	int dispatcherThreads() default GatewaySenderConfiguration.DEFAULT_DISPATCHER_THREADS;

	/**
	 * A list of {@link org.apache.geode.cache.wan.GatewayEventFilter} to be applied to the {@link org.apache.geode.cache.wan.GatewaySender}.
	 * {@link org.apache.geode.cache.wan.GatewayEventFilter} are used to filter out objects from the sending queue before dispatching
	 * them to the remote {@link org.apache.geode.cache.wan.GatewayReceiver}.
	 * <p>This property  can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.event-filters} property.
	 * <p>Default value is and empty list
	 */
	String[] eventFilters() default {};

	/**
	 * This property configures the {@link org.apache.geode.cache.wan.GatewayEventSubstitutionFilter} to be used by the GatewaySender(s).
	 * The {@link org.apache.geode.cache.wan.GatewayEventSubstitutionFilter} is used to replace values on objects before they
	 * are enqueue for remote replication.
	 * <p> This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.event-substitution-filter}
	 * <p>Default value is {@link GatewaySenderConfiguration#DEFAULT_EVENT_SUBSTITUTION_FILTER}
	 */
	String eventSubstitutionFilter() default GatewaySenderConfiguration.DEFAULT_EVENT_SUBSTITUTION_FILTER;

	/**
	 * A boolean to indicate if a configured {@link org.apache.geode.cache.wan.GatewaySender} should be started automatically
	 * <p>This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.manual-start}
	 * <p>Default is {@value @EnableGatewaySenderConfiguration.DEFAULT_MANUAL_START}
	 */
	boolean manualStart() default GatewaySenderConfiguration.DEFAULT_MANUAL_START;

	/**
	 * This property configures the maximum size, in MB, that the {@link org.apache.geode.cache.wan.GatewaySender} that the queue
	 * may take on heap memory, before overflowing to disk.
	 * <p>This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.maximum-queue-memory}
	 * <p>Default value is {@value GatewaySenderConfiguration#DEFAULT_MAXIMUM_QUEUE_MEMORY}
	 */
	int maximumQueueMemory() default GatewaySenderConfiguration.DEFAULT_MAXIMUM_QUEUE_MEMORY;

	/**
	 * Specifies the {@link String name} of the {@link org.apache.geode.cache.wan.GatewaySender}.
	 * This name is also used as the name of the bean registered in the Spring container as well as the name used in the resolution
	 * {@link org.apache.geode.cache.wan.GatewaySender} properties from {@literal application.properties}
	 * (e.g. {@literal spring.data.gemfire.gateway.sender.<name>.manual-start}), that are specific to this {@link org.apache.geode.cache.wan.GatewaySender}
	 */
	String name();

	/**
	 * This property sets the ordering policy that the GatewaySender(s) will use when queueing entries to be replicated to
	 * a remote {@link org.apache.geode.cache.wan.GatewayReceiver}.
	 * <p>There are three different ordering policies:
	 * <ul>
	 * <li>{@link OrderPolicyType#KEY} - Order of events preserved on a per key basis</li>
	 * <li>{@link OrderPolicyType#THREAD} - Order of events preserved by the thread that added the event</li>
	 * <li>{@link OrderPolicyType#PARTITION} - Order of events is preserved in order that they arrived in partitioned Region</li>
	 * </ul>
	 * <p>This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.order-policy}</p>
	 * <p>Default value is an empty list
	 */
	OrderPolicyType orderPolicy() default OrderPolicyType.KEY;

	/**
	 * A boolean flag to indicate if the configured GatewaySender(s) should use parallel GatewaySender replication.
	 * Parallel replication means that each {@link org.apache.geode.cache.server.CacheServer} that defines a {@link org.apache.geode.cache.wan.GatewaySender}
	 * will send data to a remote {@link org.apache.geode.cache.wan.GatewayReceiver}.
	 * <p> This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.parallel}
	 * <p>Default value is {@value GatewaySenderConfiguration#DEFAULT_PARALLEL}
	 */
	boolean parallel() default GatewaySenderConfiguration.DEFAULT_PARALLEL;

	/**
	 * A boolean flag to indicate if the configured GatewaySender(s) should use persistence. This setting should be used
	 * in conjunction with the {@literal disk-store-reference} property.
	 * <p> This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.persistent}
	 * <p>Default value is {@value GatewaySenderConfiguration#DEFAULT_PERSISTENT}
	 */
	boolean persistent() default GatewaySenderConfiguration.DEFAULT_PERSISTENT;

	/**
	 * A list of {@link org.apache.geode.cache.Region} names that are to be configured with {@link org.apache.geode.cache.wan.GatewaySender} replication.
	 * An empty list will denote that ALL regions are to be replicated to the remote {@link org.apache.geode.cache.wan.GatewayReceiver}.
	 * <p>This property  can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.region-names} property.
	 * <p>Default value is an empty list
	 */
	String[] regions() default {};

	/**
	 * The id of the distributed system (cluster) that the GatewaySender(s) would send their data to.
	 * <p>This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.remote-distributed-system-id}
	 * <p>Default is {@value @EnableGatewaySenderConfiguration.DEFAULT_REMOTE_DISTRIBUTED_SYSTEM_ID}
	 */
	int remoteDistributedSystemId() default GatewaySenderConfiguration.DEFAULT_REMOTE_DISTRIBUTED_SYSTEM_ID;

	/**
	 * The socket buffer size for the {@link org.apache.geode.cache.wan.GatewaySender}. This setting is in bytes.
	 * <p>This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.socket-buffer-size} property.
	 * <p>Default value is {@value GatewaySenderConfiguration#DEFAULT_SOCKET_BUFFER_SIZE}
	 */
	int socketBufferSize() default GatewaySenderConfiguration.DEFAULT_SOCKET_BUFFER_SIZE;

	/**
	 * Amount of time in milliseconds that the gateway sender will wait to receive an acknowledgment from a remote site.
	 * By default this is set to 0, which means there is no timeout. The minimum allowed timeout is 30000 (milliseconds).
	 * <p>This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.socket-read-timeout} property.
	 * <p>Default value is {@value GatewaySenderConfiguration#DEFAULT_SOCKET_READ_TIMEOUT}
	 */
	int socketReadTimeout() default GatewaySenderConfiguration.DEFAULT_SOCKET_READ_TIMEOUT;

	/**
	 * An in-order list of {@link org.apache.geode.cache.wan.GatewayTransportFilter} to be applied to
	 * {@link org.apache.geode.cache.wan.GatewaySender}
	 * <p>This property can also be configured using the {@literal spring.data.gemfire.gateway.sender.<name>.transport-filters}s property
	 * <p>Default value is an empty list
	 */
	String[] transportFilters() default {};
}
