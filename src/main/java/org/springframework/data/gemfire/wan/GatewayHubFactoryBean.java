/*
 * Copyright 2010-2012 the original author or authors.
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
import java.util.List;

import org.springframework.data.gemfire.wan.GatewayProxy.GatewayQueue;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.util.Gateway;
import com.gemstone.gemfire.cache.util.Gateway.OrderPolicy;
import com.gemstone.gemfire.cache.util.GatewayEventListener;
import com.gemstone.gemfire.cache.util.GatewayHub;
import com.gemstone.gemfire.cache.util.GatewayQueueAttributes;

/**
 * FactoryBean for creating a GemFire {@link GatewayHub} (deprecated in Gemfire
 * 7)
 * @author David Turanski
 * 
 */
public class GatewayHubFactoryBean extends AbstractWANComponentFactoryBean<GatewayHub> {
	private static List<String> validStartupPolicyValues = Arrays.asList("none", "primary", "secondary");

	private static List<String> validOrderPolicyValues = Arrays.asList("KEY,PARTITION,THREAD");

	private GatewayHub gatewayHub;

	private Integer port;

	private String bindAddress;

	private Integer maximumTimeBetweenPings;

	private Integer socketBufferSize;

	private String startupPolicy;

	private Boolean manualStart;

	private List<GatewayProxy> gateways;

	/**
	 * @param cache the Gemfire cache
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
		return GatewayHub.class;
	}

	@Override
	protected void doInit() {
		gatewayHub = cache.addGatewayHub(name, port == null ? GatewayHub.DEFAULT_PORT : port);

		if (log.isDebugEnabled()) {
			log.debug("added gateway hub " + name);
		}

		Assert.notNull(cache.getGatewayHub(name));
		
		if (bindAddress != null) {
			gatewayHub.setBindAddress(bindAddress);
		}
		if (manualStart != null) {
			gatewayHub.setManualStart(manualStart);
		}
		if (socketBufferSize != null) {
			gatewayHub.setSocketBufferSize(socketBufferSize);
		}
		if (startupPolicy != null) {
			Assert.isTrue(validStartupPolicyValues.contains(startupPolicy), "The value of startup policy:'"
					+ startupPolicy + "' is invalid");
			gatewayHub.setStartupPolicy(startupPolicy);
		}
		if (maximumTimeBetweenPings != null) {
			gatewayHub.setMaximumTimeBetweenPings(maximumTimeBetweenPings);
		}
		for (GatewayProxy gateway : gateways) {
			Gateway gw = gatewayHub.addGateway(
					gateway.getId(),
					gateway.getConcurrencyLevel() == null ? Gateway.DEFAULT_CONCURRENCY_LEVEL : gateway
							.getConcurrencyLevel());
			if (!CollectionUtils.isEmpty(gateway.getEndpoints())) {
				for (GatewayProxy.GatewayEndpoint endpoint : gateway.getEndpoints()) {
					gw.addEndpoint(endpoint.getId(), endpoint.getHost(), endpoint.getPort());
				}
			}
			if (!CollectionUtils.isEmpty(gateway.getListeners())) {
				for (GatewayEventListener listener : gateway.getListeners()) {
					gw.addListener(listener);
				}
			}
			if (gateway.getOrderPolicy() != null) {
				Assert.isTrue(validOrderPolicyValues.contains(gateway.getOrderPolicy()), "The value of order policy:'"
						+ gateway.getOrderPolicy() + "' is invalid");
				gw.setOrderPolicy(OrderPolicy.valueOf(gateway.getOrderPolicy()));
			}
			if (gateway.getSocketBufferSize() != null) {
				gw.setSocketBufferSize(gateway.getSocketBufferSize());
			}

			if (gateway.getQueue() != null) {
				GatewayQueue queue = gateway.getQueue();
				GatewayQueueAttributes queueAttributes = gw.getQueueAttributes();
				if (queue.getAlertThreshold() != null) {
					queueAttributes.setAlertThreshold(queue.getAlertThreshold());
				}
				if (queue.getEnableBatchConflation() != null) {
					queueAttributes.setBatchConflation(queue.getEnableBatchConflation());
				}
				if (queue.getBatchSize() != null) {
					queueAttributes.setBatchSize(queue.getBatchSize());
				}
				if (queue.getBatchTimeInterval() != null) {
					queueAttributes.setBatchTimeInterval(queue.getBatchTimeInterval());
				}

				if (queue.getDiskStoreRef() != null) {
					boolean persistent = (queue.getPersistent() == null) ? Boolean.TRUE : queue.getPersistent();
					Assert.isTrue(persistent, "specifying a disk store requires persistent property to be true");
					queueAttributes.setDiskStoreName(queue.getDiskStoreRef());
				}

				if (queue.getPersistent() != null) {
					queueAttributes.setEnablePersistence(queue.getPersistent());
				}

				if (queue.getMaximumQueueMemory() != null) {
					queueAttributes.setMaximumQueueMemory(queue.getMaximumQueueMemory());
				}
			}
		}
		if (gatewayHub.getManualStart() == false) {
			try {
				gatewayHub.start();
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
	}

	public void setPort(Integer port) {
		this.port = port;
	}

	public void setBindAddress(String bindAddress) {
		this.bindAddress = bindAddress;
	}

	public void setMaximumTimeBetweenPings(Integer maximumTimeBetweenPings) {
		this.maximumTimeBetweenPings = maximumTimeBetweenPings;
	}

	public void setSocketBufferSize(Integer socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	public void setStartupPolicy(String startupPolicy) {
		this.startupPolicy = startupPolicy;
	}

	public void setManualStart(Boolean manualStart) {
		this.manualStart = manualStart;
	}

	public void setGateways(List<GatewayProxy> gateways) {
		this.gateways = gateways;
	}
}
