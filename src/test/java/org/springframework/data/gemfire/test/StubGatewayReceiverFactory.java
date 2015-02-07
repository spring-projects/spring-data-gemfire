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

import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import com.gemstone.gemfire.cache.wan.GatewayReceiver;
import com.gemstone.gemfire.cache.wan.GatewayReceiverFactory;
import com.gemstone.gemfire.cache.wan.GatewayTransportFilter;
import com.gemstone.gemfire.management.internal.cli.util.spring.StringUtils;

/**
 * @author David Turanski
 * @author John Blum
 * @see com.gemstone.gemfire.cache.wan.GatewayReceiverFactory
 */
@SuppressWarnings("unused")
public class StubGatewayReceiverFactory implements GatewayReceiverFactory {

	private volatile boolean running;

	private boolean manualStart;

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

	/*
	 * (non-Javadoc)
	 * @see com.gemstone.gemfire.cache.wan.GatewayReceiverFactory#setManualStart(boolean)
	 */
	@Override
	public GatewayReceiverFactory setManualStart(final boolean manualStart) {
		this.manualStart = manualStart;
		return this;
	}

	private int generatePort() {
		final int portIncrement = new Random(System.currentTimeMillis()).nextInt(this.endPort - this.startPort);
		return Math.min(this.startPort + portIncrement, this.endPort);
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
		when(gatewayReceiver.isManualStart()).thenReturn(this.manualStart);
		when(gatewayReceiver.getMaximumTimeBetweenPings()).thenReturn(this.maximumTimeBetweenPings);
		when(gatewayReceiver.getPort()).thenReturn(generatePort());
		when(gatewayReceiver.getSocketBufferSize()).thenReturn(this.socketBufferSize);
		when(gatewayReceiver.getStartPort()).thenReturn(this.startPort);

		when(gatewayReceiver.isRunning()).thenAnswer(new Answer<Boolean>() {
			@Override
			public Boolean answer(InvocationOnMock invocation) throws Throwable {
				return StubGatewayReceiverFactory.this.running;
			}
		});

		try {
			doAnswer(new Answer<Void>() {
				public Void answer(InvocationOnMock invocation) {
					StubGatewayReceiverFactory.this.running = true;
					return null;
				}
			}).when(gatewayReceiver).start();
		}
		catch (IOException e) {
			throw new RuntimeException(e);
		}

		doAnswer(new Answer<Void>() {
			public Void answer(final InvocationOnMock invocation) throws Throwable {
				StubGatewayReceiverFactory.this.running = false;
				return null;
			}
		}).when(gatewayReceiver).stop();

		return gatewayReceiver;
	}

}
