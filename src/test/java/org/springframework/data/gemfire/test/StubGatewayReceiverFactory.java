/*
 * Copyright 2002-2013 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.test;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import com.gemstone.gemfire.cache.wan.GatewayReceiver;
import com.gemstone.gemfire.cache.wan.GatewayReceiverFactory;
import com.gemstone.gemfire.cache.wan.GatewayTransportFilter;
import com.gemstone.gemfire.management.internal.cli.util.spring.StringUtils;

/**
 * @author David Turanski
 * @author John Blum
 *
 */
@SuppressWarnings("unused")
public class StubGatewayReceiverFactory implements GatewayReceiverFactory {

	private int endPort;
	private int maximumTimeBetweenPings;
	private int socketBufferSize;
	private int startPort;

	private List<GatewayTransportFilter> gatewayTransportFilters = new ArrayList<GatewayTransportFilter>();

	private String bindAddress;
	private String hostnameForClients;
	private String hostnameForSenders;

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewayReceiverFactory#setStartPort(int)
	 */
	@Override
	public GatewayReceiverFactory setStartPort(int startPort) {
		this.startPort = startPort;
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewayReceiverFactory#setEndPort(int)
	 */
	@Override
	public GatewayReceiverFactory setEndPort(int endPort) {
		this.endPort = endPort;
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewayReceiverFactory#setSocketBufferSize(int)
	 */
	@Override
	public GatewayReceiverFactory setSocketBufferSize(int socketBufferSize) {
		this.socketBufferSize = socketBufferSize;
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewayReceiverFactory#setBindAddress(java.lang.String)
	 */
	@Override
	public GatewayReceiverFactory setBindAddress(String address) {
		this.bindAddress = address;
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewayReceiverFactory#addGatewayTransportFilter(com.gemstone.gemfire.cache.wan.GatewayTransportFilter)
	 */
	@Override
	public GatewayReceiverFactory addGatewayTransportFilter(GatewayTransportFilter filter) {
		this.gatewayTransportFilters.add(filter);
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewayReceiverFactory#removeGatewayTransportFilter(com.gemstone.gemfire.cache.wan.GatewayTransportFilter)
	 */
	@Override
	public GatewayReceiverFactory removeGatewayTransportFilter(GatewayTransportFilter filter) {
		this.gatewayTransportFilters.remove(filter);
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewayReceiverFactory#setMaximumTimeBetweenPings(int)
	 */
	@Override
	public GatewayReceiverFactory setMaximumTimeBetweenPings(int time) {
		this.maximumTimeBetweenPings = time;
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewayReceiverFactory#setHostnameForClients(String)
	 * @see com.gemstone.gemfire.cache.wan.GatewayReceiver#getHost
	 */
	//@Override
	public GatewayReceiverFactory setHostnameForClients(final String name) {
		this.hostnameForClients = name;
		return this;
	}

	/* (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewayReceiverFactory#setHostnameForSenders(String)
	 */
	@Override
	public GatewayReceiverFactory setHostnameForSenders(final String hostnameForSenders) {
		this.hostnameForSenders = hostnameForSenders;
		return this;
	}

	/* (non-Javadoc)
		 * @see com.gemstone.gemfire.cache.wan.GatewayReceiverFactory#create()
		 */
	@Override
	public GatewayReceiver create() {
		GatewayReceiver gatewayReceiver = mock(GatewayReceiver.class);

		when(gatewayReceiver.getBindAddress()).thenReturn(this.bindAddress);
		when(gatewayReceiver.getEndPort()).thenReturn(this.endPort);
		when(gatewayReceiver.getGatewayTransportFilters()).thenReturn(this.gatewayTransportFilters);
		when(gatewayReceiver.getHost()).thenReturn(StringUtils.hasText(this.hostnameForSenders)
			? this.hostnameForSenders : this.hostnameForClients);
		when(gatewayReceiver.getMaximumTimeBetweenPings()).thenReturn(this.maximumTimeBetweenPings);
		when(gatewayReceiver.getSocketBufferSize()).thenReturn(this.socketBufferSize);
		when(gatewayReceiver.getStartPort()).thenReturn(this.startPort);

		return gatewayReceiver;
	}

}
