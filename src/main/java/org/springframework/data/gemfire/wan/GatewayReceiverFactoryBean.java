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
import java.util.List;

import org.springframework.context.SmartLifecycle;
import org.springframework.util.Assert;
import org.springframework.util.CollectionUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.wan.GatewayReceiver;
import com.gemstone.gemfire.cache.wan.GatewayReceiverFactory;
import com.gemstone.gemfire.cache.wan.GatewayTransportFilter;
import com.gemstone.gemfire.management.internal.cli.util.spring.StringUtils;

/**
 * Spring FactoryBean for creating a GemFire {@link GatewayReceiver}.
 * 
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.context.SmartLifecycle
 * @see org.springframework.data.gemfire.wan.AbstractWANComponentFactoryBean
 * @see com.gemstone.gemfire.cache.Cache
 * @see com.gemstone.gemfire.cache.wan.GatewayReceiver
 * @see com.gemstone.gemfire.cache.wan.GatewayReceiverFactory
 * @since 1.2.2
 */
@SuppressWarnings("unused")
public class GatewayReceiverFactoryBean extends AbstractWANComponentFactoryBean<GatewayReceiver>
		implements SmartLifecycle {

	private boolean manualStart = false;

	private volatile GatewayReceiver gatewayReceiver;

	private Integer endPort;
	private Integer maximumTimeBetweenPings;
	private Integer socketBufferSize;
	private Integer startPort;

	private List<GatewayTransportFilter> transportFilters;

	private String bindAddress;
	private String hostnameForSenders;

	/**
	 * Constructs an instance of the GatewayReceiverFactoryBean class for configuring an initializing
	 * a GemFire Gateway Receiver.
	 *
	 * @param cache a reference to the GemFire Cache used to setup the Gateway Receiver.
	 */
	public GatewayReceiverFactoryBean(Cache cache) {
		super(cache);
	}

	@Override
	public GatewayReceiver getObject() throws Exception {
		return gatewayReceiver;
	}

	@Override
	public Class<?> getObjectType() {
		return (gatewayReceiver != null ? gatewayReceiver.getClass() : GatewayReceiver.class);
	}

	@Override
	protected void doInit() throws Exception {
		GatewayReceiverFactory gatewayReceiverFactory = cache.createGatewayReceiverFactory();

		if (!CollectionUtils.isEmpty(transportFilters)) {
			for (GatewayTransportFilter transportFilter : transportFilters) {
				gatewayReceiverFactory.addGatewayTransportFilter(transportFilter);
			}
		}

		if (StringUtils.hasText(bindAddress)) {
			gatewayReceiverFactory.setBindAddress(bindAddress);
		}

		if (StringUtils.hasText(hostnameForSenders)) {
			gatewayReceiverFactory.setHostnameForSenders(hostnameForSenders);
		}

		if (maximumTimeBetweenPings != null) {
			gatewayReceiverFactory.setMaximumTimeBetweenPings(maximumTimeBetweenPings);
		}

		int localStartPort = (startPort != null ? startPort : GatewayReceiver.DEFAULT_START_PORT);
		int localEndPort = (endPort != null ? endPort : GatewayReceiver.DEFAULT_END_PORT);

		Assert.isTrue(localStartPort <= localEndPort, String.format("'startPort' must be less than or equal to %1$d.",
			localEndPort));

		gatewayReceiverFactory.setStartPort(localStartPort);
		gatewayReceiverFactory.setEndPort(localEndPort);
		gatewayReceiverFactory.setManualStart(true);

		if (socketBufferSize != null) {
			gatewayReceiverFactory.setSocketBufferSize(socketBufferSize);
		}

		gatewayReceiver = gatewayReceiverFactory.create();
	}

	public void setGatewayReceiver(final GatewayReceiver gatewayReceiver) {
		this.gatewayReceiver = gatewayReceiver;
	}

	public void setBindAddress(String bindAddress) {
		this.bindAddress = bindAddress;
	}

	public void setHostnameForSenders(String hostnameForSenders) {
		this.hostnameForSenders = hostnameForSenders;
	}

	public void setStartPort(Integer startPort) {
		this.startPort = startPort;
	}

	public void setEndPort(Integer endPort) {
		this.endPort = endPort;
	}

	public void setManualStart(Boolean manualStart) {
		this.manualStart = Boolean.TRUE.equals(manualStart);
	}

	public void setMaximumTimeBetweenPings(Integer maximumTimeBetweenPings) {
		this.maximumTimeBetweenPings = maximumTimeBetweenPings;
	}

	public void setSocketBufferSize(Integer socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
	}

	public void setTransportFilters(List<GatewayTransportFilter> transportFilters) {
		this.transportFilters = transportFilters;
	}

	@Override
	public boolean isAutoStartup() {
		return !manualStart;
	}

	@Override
	public int getPhase() {
		return Integer.MAX_VALUE;
	}

	@Override
	public boolean isRunning() {
		return gatewayReceiver.isRunning();
	}

	@Override
	public void start() {
		Assert.state(gatewayReceiver != null, "The GatewayReceiver was not properly configured and initialized!");

		if (!isRunning()) {
			try {
				gatewayReceiver.start();
			}
			catch (IOException e) {
				throw new RuntimeException("Failed to start Gateway Receiver due to I/O error.", e);
			}
		}
	}

	@Override
	public void stop() {
		gatewayReceiver.stop();
	}

	@Override
	public void stop(final Runnable callback) {
		stop();
		callback.run();
	}

}
