/*
 * Copyright 2010-2018 the original author or authors.
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

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.wan.GatewayReceiver;
import org.apache.geode.cache.wan.GatewayReceiverFactory;
import org.apache.geode.cache.wan.GatewayTransportFilter;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} for creating a GemFire {@link GatewayReceiver}.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.context.SmartLifecycle
 * @see org.springframework.data.gemfire.wan.AbstractWANComponentFactoryBean
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.wan.GatewayReceiver
 * @see org.apache.geode.cache.wan.GatewayReceiverFactory
 * @since 1.2.2
 */
@SuppressWarnings("unused")
public class GatewayReceiverFactoryBean extends AbstractWANComponentFactoryBean<GatewayReceiver> {

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
	 * Constructs an instance of the {@link GatewayReceiverFactoryBean} class initialized with a reference to
	 * the GemFire {@link Cache} used to configure and initialize a GemFire {@link GatewayReceiver}.
	 *
	 * @param cache reference to the GemFire {@link Cache} used to create the {@link GatewayReceiver}.
	 * @see org.apache.geode.cache.Cache
	 */
	public GatewayReceiverFactoryBean(Cache cache) {
		super(cache);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public GatewayReceiver getObject() throws Exception {
		return gatewayReceiver;
	}

	/**
	 * @inheritDoc
	 */
	@Override
	public Class<?> getObjectType() {
		return (gatewayReceiver != null ? gatewayReceiver.getClass() : GatewayReceiver.class);
	}

	/**
	 * @inheritDoc
	 */
	@Override
	protected void doInit() throws Exception {
		GatewayReceiverFactory gatewayReceiverFactory = cache.createGatewayReceiverFactory();

		if (StringUtils.hasText(bindAddress)) {
			gatewayReceiverFactory.setBindAddress(bindAddress);
		}

		if (StringUtils.hasText(hostnameForSenders)) {
			gatewayReceiverFactory.setHostnameForSenders(hostnameForSenders);
		}

		int localStartPort = defaultPort(startPort, GatewayReceiver.DEFAULT_START_PORT);
		int localEndPort = defaultPort(endPort, GatewayReceiver.DEFAULT_END_PORT);

		Assert.isTrue(localStartPort <= localEndPort,
			String.format("'startPort' must be less than or equal to %d.", localEndPort));

		gatewayReceiverFactory.setStartPort(localStartPort);
		gatewayReceiverFactory.setEndPort(localEndPort);
		gatewayReceiverFactory.setManualStart(manualStart);

		if (maximumTimeBetweenPings != null) {
			gatewayReceiverFactory.setMaximumTimeBetweenPings(maximumTimeBetweenPings);
		}

		if (socketBufferSize != null) {
			gatewayReceiverFactory.setSocketBufferSize(socketBufferSize);
		}

		for (GatewayTransportFilter transportFilter : CollectionUtils.nullSafeList(transportFilters)) {
			gatewayReceiverFactory.addGatewayTransportFilter(transportFilter);
		}

		gatewayReceiver = gatewayReceiverFactory.create();
	}

	protected int defaultPort(Integer port, int defaultPort) {
		return (port != null ? port : defaultPort);
	}

	public void setGatewayReceiver(GatewayReceiver gatewayReceiver) {
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
}
