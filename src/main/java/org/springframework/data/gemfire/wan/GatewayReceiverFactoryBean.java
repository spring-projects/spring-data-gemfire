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

import java.util.List;

import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.wan.GatewayReceiver;
import com.gemstone.gemfire.cache.wan.GatewayReceiverFactory;
import com.gemstone.gemfire.cache.wan.GatewayTransportFilter;

/**
 * FactoryBean for creating a GemFire {@link GatewayReceiver}.
 * 
 * @author David Turanski
 * 
 */
public class GatewayReceiverFactoryBean extends AbstractWANComponentFactoryBean<GatewayReceiver> {
	private GatewayReceiver gatewayReceiver;

	private List<GatewayTransportFilter> transportFilters;

	private Integer startPort;

	private Integer endPort;

	private Integer maximumTimeBetweenPings;

	private Integer socketBufferSize;

	private String bindAddress;

	/**
	 * 
	 * @param cache
	 */
	public GatewayReceiverFactoryBean(Cache cache) {
		super(cache);
		// Bean name not required.
		this.name = "gateway-receiver";
	}

	@Override
	public GatewayReceiver getObject() throws Exception {
		return gatewayReceiver;
	}

	@Override
	public Class<?> getObjectType() {
		return GatewayReceiver.class;
	}

	@Override
	protected void doInit() {
		GatewayReceiverFactory gatewayReceiverFactory = cache.createGatewayReceiverFactory();
		if (!CollectionUtils.isEmpty(transportFilters)) {
			for (GatewayTransportFilter transportFilter : transportFilters) {
				gatewayReceiverFactory.addGatewayTransportFilter(transportFilter);
			}
		}
		int minPort = (startPort == null) ? GatewayReceiver.DEFAULT_START_PORT : startPort;
		int maxPort = (endPort == null) ? GatewayReceiver.DEFAULT_END_PORT : endPort;
		Assert.isTrue(minPort <= maxPort, "startPort must be less then or equal to " + maxPort);
		gatewayReceiverFactory.setStartPort(minPort);
		gatewayReceiverFactory.setEndPort(maxPort);
		if (socketBufferSize != null) {
			gatewayReceiverFactory.setSocketBufferSize(socketBufferSize);
		}
		if (maximumTimeBetweenPings != null) {
			gatewayReceiverFactory.setMaximumTimeBetweenPings(maximumTimeBetweenPings);
		}
		if (bindAddress != null) {
			gatewayReceiverFactory.setBindAddress(bindAddress);
		}

		gatewayReceiver = gatewayReceiverFactory.create();
	}

	public void setGatewayReceiver(GatewayReceiver gatewayReceiver) {
		this.gatewayReceiver = gatewayReceiver;
	}

	public void setTransportFilters(List<GatewayTransportFilter> transportFilters) {
		this.transportFilters = transportFilters;
	}

	public void setStartPort(Integer startPort) {
		this.startPort = startPort;
	}

	public void setEndPort(Integer endPort) {
		this.endPort = endPort;
	}

	public void setMaximumTimeBetweenPings(Integer maximumTimeBetweenPings) {
		this.maximumTimeBetweenPings = maximumTimeBetweenPings;
	}

	public void setSocketBufferSize(Integer socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	public void setBindAddress(String bindAddress) {
		this.bindAddress = bindAddress;
	}

}
