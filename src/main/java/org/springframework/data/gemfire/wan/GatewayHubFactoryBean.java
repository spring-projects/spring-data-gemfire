/*
 * Copyright 2010-2013 the original author or authors.
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
 */
package org.springframework.data.gemfire.wan;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

import org.springframework.data.gemfire.wan.GatewayProxy.GatewayQueue;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.util.Gateway;
import com.gemstone.gemfire.cache.util.GatewayEventListener;
import com.gemstone.gemfire.cache.util.GatewayHub;
import com.gemstone.gemfire.cache.util.GatewayQueueAttributes;
import com.gemstone.gemfire.management.internal.cli.util.spring.StringUtils;

/**
 * FactoryBean for creating a GemFire {@link GatewayHub} (deprecated in Gemfire 7).
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.wan.AbstractWANComponentFactoryBean
 * @see com.gemstone.gemfire.cache.Cache
 * @see com.gemstone.gemfire.cache.util.Gateway
 * @see com.gemstone.gemfire.cache.util.GatewayHub
 * @see com.gemstone.gemfire.cache.util.GatewayEventListener
 * @see com.gemstone.gemfire.cache.util.GatewayQueueAttributes
 */
@SuppressWarnings({"deprecation", "unused" })
public class GatewayHubFactoryBean extends AbstractWANComponentFactoryBean<GatewayHub> {

	private Boolean manualStart;

	private GatewayHub gatewayHub;

	//private Integer maxConnections;
	private Integer maximumTimeBetweenPings;
	private Integer port;
	private Integer socketBufferSize;

	private List<GatewayProxy> gateways;

	private StartupPolicyType startupPolicy;

	private String bindAddress;

	/**
	 * Constructs an instance of the GatewayHubFactoryBean class used to create GemFire WAN GatewayHubs initialized
	 * with the specified GemFire Cache.
	 *
	 * @param cache a reference to the Gemfire Cache.
	 * @see com.gemstone.gemfire.cache.Cache
	 */
	public GatewayHubFactoryBean(final Cache cache) {
		super(cache);
	}

	@Override
	public GatewayHub getObject() throws Exception {
		return gatewayHub;
	}

	@Override
	public Class<?> getObjectType() {
		return (gatewayHub != null ? gatewayHub.getClass() : GatewayHub.class);
	}

	@Override
	protected void doInit() {
		gatewayHub = cache.addGatewayHub(getName(), getPort());

		if (log.isDebugEnabled()) {
			log.debug(String.format("Adding GemFire GatewayHub (%1$s)", getName()));
		}

		Assert.notNull(cache.getGatewayHub(getName()));

		gatewayHub.setBindAddress(getBindAddress());
		gatewayHub.setManualStart(isManualStart(GatewayHub.DEFAULT_MANUAL_START));
		//gatewayHub.setMaxConnections(getMaxConnections());
		gatewayHub.setMaximumTimeBetweenPings(getMaximumTimeBetweenPings());
		gatewayHub.setSocketBufferSize(getSocketBufferSize());
		gatewayHub.setStartupPolicy(getStartupPolicy().getName());

		configureGateways();
		autoStart();
	}

	private void configureGateways() {
		for (GatewayProxy gatewayProxy : getGateways()) {
			Gateway gateway = gatewayHub.addGateway(gatewayProxy.getId(), gatewayProxy.getConcurrencyLevel());

			for (GatewayProxy.GatewayEndpoint endpoint : gatewayProxy.getEndpoints()) {
				gateway.addEndpoint(endpoint.getId(), endpoint.getHost(), endpoint.getPort());
			}

			for (GatewayEventListener listener : gatewayProxy.getListeners()) {
				gateway.addListener(listener);
			}

			gateway.setOrderPolicy(gatewayProxy.getOrderPolicy());
			gateway.setSocketBufferSize(gatewayProxy.getSocketBufferSize());
			gateway.setSocketReadTimeout(gatewayProxy.getSocketReadTimeout());

			if (gatewayProxy.getQueue() != null) {
				GatewayQueue queue = gatewayProxy.getQueue();
				GatewayQueueAttributes queueAttributes = gateway.getQueueAttributes();

				queueAttributes.setAlertThreshold(queue.getAlertThreshold());
				queueAttributes.setBatchConflation(queue.getEnableBatchConflation());
				queueAttributes.setBatchSize(queue.getBatchSize());
				queueAttributes.setBatchTimeInterval(queue.getBatchTimeInterval());
				queueAttributes.setMaximumQueueMemory(queue.getMaximumQueueMemory());
				queueAttributes.setEnablePersistence(queue.getPersistent());

				if (queue.getDiskStoreRef() != null) {
					queueAttributes.setDiskStoreName(queue.getDiskStoreRef());
				}
			}
		}
	}

	private void autoStart() {
		if (!gatewayHub.getManualStart()) {
			try {
				gatewayHub.start();
			}
			catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
	}

	public void setBindAddress(String bindAddress) {
		this.bindAddress = bindAddress;
	}

	/* (non-Javadoc) */
	String getBindAddress() {
		return (StringUtils.hasText(bindAddress) ? bindAddress : GatewayHub.DEFAULT_BIND_ADDRESS);
	}

	public void setGateways(List<GatewayProxy> gateways) {
		this.gateways = gateways;
	}

	/* (non-Javadoc) */
	List<GatewayProxy> getGateways() {
		return (gateways != null ? gateways : Collections.<GatewayProxy>emptyList());
	}

	public void setManualStart(Boolean manualStart) {
		this.manualStart = manualStart;
	}

	/* (non-Javadoc) */
	boolean isManualStart(final boolean defaultManualStart) {
		return (manualStart != null ? manualStart :  defaultManualStart);
	}

	/*
	public void setMaxConnections(Integer maxConnections) {
		this.maxConnections = maxConnections;
	}

	// (non-Javadoc)
	Integer getMaxConnections() {
		return (maxConnections != null ? maxConnections : GatewayHub.DEFAULT_MAX_CONNECTIONS);
	}
	*/

	public void setMaximumTimeBetweenPings(Integer maximumTimeBetweenPings) {
		this.maximumTimeBetweenPings = maximumTimeBetweenPings;
	}

	// (non-Javadoc)
	Integer getMaximumTimeBetweenPings() {
		return (maximumTimeBetweenPings != null ? maximumTimeBetweenPings
			: GatewayHub.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS);
	}

	public void setPort(Integer port) {
		this.port = port;
	}

	/* (non-Javadoc) */
	Integer getPort() {
		return (port != null ? port : GatewayHub.DEFAULT_PORT);
	}

	public void setSocketBufferSize(Integer socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	/* (non-Javadoc) */
	Integer getSocketBufferSize() {
		return (socketBufferSize != null ? socketBufferSize : GatewayHub.DEFAULT_SOCKET_BUFFER_SIZE);
	}

	public void setStartupPolicy(StartupPolicyType startupPolicy) {
		this.startupPolicy = startupPolicy;
	}

	/* (non-Javadoc) */
	StartupPolicyType getStartupPolicy() {
		return (startupPolicy != null ? startupPolicy : StartupPolicyType.DEFAULT);
	}

}
