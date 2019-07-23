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
package org.springframework.data.gemfire.wan;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.wan.GatewayReceiver;
import org.apache.geode.cache.wan.GatewayReceiverFactory;
import org.apache.geode.cache.wan.GatewayTransportFilter;

import org.springframework.beans.factory.FactoryBean;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Spring {@link FactoryBean} for creating a Pivotal GemFire {@link GatewayReceiver}.
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
	 * the Pivotal GemFire {@link Cache} used to configure and initialize a Pivotal GemFire {@link GatewayReceiver}.
	 *
	 * @param cache reference to the Pivotal GemFire {@link Cache} used to create the {@link GatewayReceiver}.
	 * @see org.apache.geode.cache.Cache
	 */
	public GatewayReceiverFactoryBean(Cache cache) {
		super(cache);
	}

	@Override
	protected void doInit() {

		GatewayReceiverFactory gatewayReceiverFactory = this.cache.createGatewayReceiverFactory();

		Optional.ofNullable(this.bindAddress)
			.filter(StringUtils::hasText)
			.ifPresent(gatewayReceiverFactory::setBindAddress);

		Optional.ofNullable(this.hostnameForSenders)
			.filter(StringUtils::hasText)
			.ifPresent(gatewayReceiverFactory::setHostnameForSenders);

		int localStartPort = defaultPort(startPort, GatewayReceiver.DEFAULT_START_PORT);
		int localEndPort = defaultPort(endPort, GatewayReceiver.DEFAULT_END_PORT);

		Assert.isTrue(localStartPort <= localEndPort,
			String.format("[startPort] must be less than or equal to [%d]", localEndPort));

		gatewayReceiverFactory.setStartPort(localStartPort);
		gatewayReceiverFactory.setEndPort(localEndPort);
		gatewayReceiverFactory.setManualStart(manualStart);

		if (maximumTimeBetweenPings != null) {
			gatewayReceiverFactory.setMaximumTimeBetweenPings(maximumTimeBetweenPings);
		}

		if (socketBufferSize != null) {
			gatewayReceiverFactory.setSocketBufferSize(socketBufferSize);
		}

		CollectionUtils.nullSafeList(this.transportFilters).forEach(gatewayReceiverFactory::addGatewayTransportFilter);

		gatewayReceiver = gatewayReceiverFactory.create();
	}

	@Override
	public GatewayReceiver getObject() throws Exception {
		return this.gatewayReceiver;
	}

	@Override
	public Class<?> getObjectType() {

		return this.gatewayReceiver != null
			? this.gatewayReceiver.getClass()
			: GatewayReceiver.class;
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

	public Collection<? extends GatewayTransportFilter> getTransportFilters() {
		return Collections.unmodifiableList(this.transportFilters);
	}
}
