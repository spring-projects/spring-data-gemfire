package org.springframework.data.gemfire.wan.annotation;

import org.springframework.context.annotation.Import;

import java.lang.annotation.*;

/**
 * The EnableGatewayReceiver annotation creates a {@link org.apache.geode.cache.wan.GatewayReceiver}
 * within GemFire/Geode {@link org.apache.geode.cache.Cache}
 *
 * @author Udo Kohlmeyer
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.context.annotation.Import
 * @see org.apache.geode.cache.wan.GatewayTransportFilter
 * @see org.apache.geode.cache.wan.GatewayReceiver
 * @see org.apache.geode.cache.Cache
 * @since 2.2.0
 */
@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
@Inherited
@Documented
@Import(EnableGatewayReceiverConfiguration.class)
@SuppressWarnings("unused")
public @interface EnableGatewayReceiver {

	/**
	 * A boolean to allow the GatewayReceiver to be created without being started after creation.
	 * If the manualStart is set to {@literal true} then the system will create the GatewayReceiver but not start it.
	 * It then becomes the responsibility of the operator to start the GatewayReceiver at a later stage.
	 * If set to {@literal false} the GatewayReceiver will start automatically after creation.
	 * Default is {@value EnableGatewayReceiverConfiguration#DEFAULT_MANUAL_START}
	 */
	boolean manualStart() default EnableGatewayReceiverConfiguration.DEFAULT_MANUAL_START;

	/**
	 * The starting port number that GatewayReceiver will use when selecting a port to run on. This range of port numbers
	 * is bounded by the {startPort} and {endPort}.
	 * Default is {@value EnableGatewayReceiverConfiguration#DEFAULT_START_PORT}
	 */
	int startPort() default EnableGatewayReceiverConfiguration.DEFAULT_START_PORT;

	/**
	 * The end port number that GatewayReceiver will use when selecting a port to run on. This range of port numbers
	 * is bounded by the {startPort} and {endPort}.
	 * Default is {@value EnableGatewayReceiverConfiguration#DEFAULT_END_PORT}
	 */
	int endPort() default EnableGatewayReceiverConfiguration.DEFAULT_END_PORT;

	/**
	 * An integer value in milliseconds representing the maximum time which a {@link org.apache.geode.cache.wan.GatewayReceiver}
	 * will wait to receive a ping back from a {@link org.apache.geode.cache.wan.GatewaySender} before the {@link org.apache.geode.cache.wan.GatewayReceiver}
	 * believes the sender to be not available.
	 * Default value is {@value EnableGatewayReceiverConfiguration#DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS}
	 */
	int maximumTimeBetweenPings() default EnableGatewayReceiverConfiguration.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS;

	/**
	 * The socket buffer size for the {@link org.apache.geode.cache.wan.GatewayReceiver}. This setting is in bytes
	 * Default value is {@value EnableGatewayReceiverConfiguration#DEFAULT_SOCKET_BUFFER_SIZE}
	 */
	int socketBufferSize() default EnableGatewayReceiverConfiguration.DEFAULT_SOCKET_BUFFER_SIZE;

	/**
	 * An in-order list of {@link org.apache.geode.cache.wan.GatewayTransportFilter} to be applied to
	 * {@link org.apache.geode.cache.wan.GatewayReceiver}
	 * Default value is {@value {}}
	 */
	String[] transportFilters() default {};

	/**
	 * The IP address or hostname that the {@link org.apache.geode.cache.wan.GatewayReceiver} communication socket will be bound to.
	 * An empty String will cause the underlying socket to bind to 0.0.0.0
	 * Default value is {@value EnableGatewayReceiverConfiguration#DEFAULT_BIND_ADDRESS}
	 */
	String bindAddress() default EnableGatewayReceiverConfiguration.DEFAULT_BIND_ADDRESS;

	/**
	 * The hostname or IP Address that the {@link org.apache.geode.cache.wan.GatewaySender} will use to connect and communicate with the
	 * {@link org.apache.geode.cache.wan.GatewayReceiver}. An empty String will cause the system to expose the hostname
	 * to the {@link org.apache.geode.cache.wan.GatewaySender}. Generally this property is set when there are multiple
	 * network interfaces or when they are designated as "internal" or "public" network interfaces. In a cloud environment
	 * the notion of external and internal network interfaces exist and generally the "externally"/outwardly facing network
	 * interface is used.
	 * Default value is {@value EnableGatewayReceiverConfiguration#DEFAULT_HOSTNAME_FOR_SENDERS}
	 */
	String hostnameForSenders() default EnableGatewayReceiverConfiguration.DEFAULT_HOSTNAME_FOR_SENDERS;
}
