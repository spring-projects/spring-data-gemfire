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
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.springframework.data.gemfire.wan.GatewayProxy.GatewayQueue;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.util.Gateway;
import com.gemstone.gemfire.cache.util.Gateway.OrderPolicy;
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
@SuppressWarnings({ "deprecation", "unused" })
public class GatewayHubFactoryBean extends AbstractWANComponentFactoryBean<GatewayHub> {

	private static List<String> validOrderPolicyValues = Arrays.asList("key, partition, thread");

	private static List<String> validStartupPolicyValues = Arrays.asList(GatewayHub.STARTUP_POLICY_NONE,
		GatewayHub.STARTUP_POLICY_PRIMARY, GatewayHub.STARTUP_POLICY_SECONDARY);

	private Boolean manualStart;

	private GatewayHub gatewayHub;

	private Integer maximumTimeBetweenPings;
	private Integer port;
	private Integer socketBufferSize;

	private List<GatewayProxy> gateways;

	private String bindAddress;
	private String startupPolicy;

	/**
	 * @param cache a reference to the GemFire Cache.
	 */
	public GatewayHubFactoryBean(Cache cache) {
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
		gatewayHub.setMaximumTimeBetweenPings(getMaximumTimeBetweenPings());
		gatewayHub.setSocketBufferSize(getSocketBufferSize());

		String localStartupPolicy = getStartupPolicy().trim().toLowerCase();

		Assert.isTrue(validStartupPolicyValues.contains(localStartupPolicy), String.format(
			"The specified startup-policy '%1$s' is not valid!", localStartupPolicy));

		gatewayHub.setStartupPolicy(localStartupPolicy);

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

			if (StringUtils.hasText(gatewayProxy.getOrderPolicy())) {
				OrderPolicy orderPolicy = getOrderPolicyEnum(gatewayProxy.getOrderPolicy());
				Assert.notNull(orderPolicy, String.format("The specified order-policy '%1$s' is not valid!",
					gatewayProxy.getOrderPolicy()));
				gateway.setOrderPolicy(orderPolicy);
			}

			gateway.setSocketBufferSize(gatewayProxy.getSocketBufferSize());

			if (gatewayProxy.getQueue() != null) {
				GatewayQueue queue = gatewayProxy.getQueue();
				GatewayQueueAttributes queueAttributes = gateway.getQueueAttributes();

				queueAttributes.setAlertThreshold(queue.getAlertThreshold());
				queueAttributes.setBatchConflation(queue.getEnableBatchConflation());
				queueAttributes.setBatchSize(queue.getBatchSize());
				queueAttributes.setBatchTimeInterval(queue.getBatchTimeInterval());
				queueAttributes.setEnablePersistence(queue.getPersistent());
				queueAttributes.setMaximumQueueMemory(queue.getMaximumQueueMemory());

				if (queue.getDiskStoreRef() != null) {
					queueAttributes.setDiskStoreName(queue.getDiskStoreRef());
				}
			}
		}
	}

	private Gateway.OrderPolicy getOrderPolicyEnum(final String value) {
		try {
			return OrderPolicy.valueOf(value.trim().toUpperCase());
		}
		catch (IllegalArgumentException ignore) {
			return null;
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

	String getBindAddress() {
		return (StringUtils.hasText(bindAddress) ? bindAddress : GatewayHub.DEFAULT_BIND_ADDRESS);
	}

	public void setGateways(List<GatewayProxy> gateways) {
		this.gateways = gateways;
	}

	List<GatewayProxy> getGateways() {
		return (gateways != null ? gateways : Collections.<GatewayProxy>emptyList());
	}

	public void setManualStart(Boolean manualStart) {
		this.manualStart = manualStart;
	}

	boolean isManualStart(final boolean defaultManualStart) {
		return (manualStart != null ? manualStart : defaultManualStart);
	}

	public void setMaximumTimeBetweenPings(Integer maximumTimeBetweenPings) {
		this.maximumTimeBetweenPings = maximumTimeBetweenPings;
	}

	Integer getMaximumTimeBetweenPings() {
		return (maximumTimeBetweenPings != null ? maximumTimeBetweenPings
			: GatewayHub.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS);
	}

	public void setPort(Integer port) {
		this.port = port;
	}

	Integer getPort() {
		return (port != null ? port : GatewayHub.DEFAULT_PORT);
	}

	public void setSocketBufferSize(Integer socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	Integer getSocketBufferSize() {
		return (socketBufferSize != null ? socketBufferSize : GatewayHub.DEFAULT_SOCKET_BUFFER_SIZE);
	}

	public void setStartupPolicy(String startupPolicy) {
		this.startupPolicy = startupPolicy;
	}

	String getStartupPolicy() {
		return (StringUtils.hasText(startupPolicy) ? startupPolicy : GatewayHub.DEFAULT_STARTUP_POLICY);
	}

}
