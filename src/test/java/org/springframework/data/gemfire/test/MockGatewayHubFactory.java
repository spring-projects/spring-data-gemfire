/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.test;

import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import com.gemstone.gemfire.cache.util.Gateway;
import com.gemstone.gemfire.cache.util.GatewayHub;

/**
 * The MockGatewayHubFactory class is a factory for creating mock GemFire GatewayHubs.
 *
 * @author John Blum
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.test.AbstractMockerySupport
 * @see org.springframework.data.gemfire.wan.GatewayHubFactoryBean
 * @see com.gemstone.gemfire.cache.util.Gateway
 * @see com.gemstone.gemfire.cache.util.GatewayHub
 * @since 1.5.3
 */
@SuppressWarnings({ "deprecation", "unused" })
public class MockGatewayHubFactory extends AbstractMockerySupport {

	private static final AtomicLong ID_SEQUENCE = new AtomicLong(System.currentTimeMillis());

	private Boolean manualStart = GatewayHub.DEFAULT_MANUAL_START;

	private Integer maximumConnections = GatewayHub.DEFAULT_MAX_CONNECTIONS;
	private Integer maximumTimeBetweenPings = GatewayHub.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS;
	private Integer socketBufferSize = GatewayHub.DEFAULT_SOCKET_BUFFER_SIZE;

	private List<Gateway> gateways = new ArrayList<Gateway>();

	private String bindAddress = GatewayHub.DEFAULT_BIND_ADDRESS;
	private String startupPolicy = GatewayHub.DEFAULT_STARTUP_POLICY;

	public GatewayHub mockGatewayHub(final String id, final int port) {
		final GatewayHub mockGatewayHub = mock(GatewayHub.class, String.format("%1$s.MockGatewayHub", getMockId()));

		when(mockGatewayHub.getGateways()).thenReturn(gateways);
		when(mockGatewayHub.getGatewayIds()).thenReturn(getGatewayIds(gateways));
		when(mockGatewayHub.getId()).thenReturn(id);
		when(mockGatewayHub.getPort()).thenReturn(port);

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				bindAddress = invocation.getArgumentAt(0, String.class);
				return null;
			}
		}).when(mockGatewayHub).setBindAddress(anyString());

		when(mockGatewayHub.getBindAddress()).thenAnswer(new Answer<String>() {
			@Override public String answer(final InvocationOnMock invocation) throws Throwable {
				return bindAddress;
			}
		});

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				manualStart = invocation.getArgumentAt(0, Boolean.class);
				return null;
			}
		}).when(mockGatewayHub).setManualStart(anyBoolean());

		when(mockGatewayHub.getManualStart()).thenAnswer(new Answer<Boolean>() {
			@Override public Boolean answer(final InvocationOnMock invocation) throws Throwable {
				return manualStart;
			}
		});

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				maximumConnections = invocation.getArgumentAt(0, Integer.class);
				return null;
			}
		}).when(mockGatewayHub).setMaxConnections(anyInt());

		when(mockGatewayHub.getMaxConnections()).thenAnswer(new Answer<Integer>() {
			@Override public Integer answer(final InvocationOnMock invocation) throws Throwable {
				return maximumConnections;
			}
		});

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				maximumTimeBetweenPings = invocation.getArgumentAt(0, Integer.class);
				return null;
			}
		}).when(mockGatewayHub).setMaximumTimeBetweenPings(anyInt());

		when(mockGatewayHub.getMaximumTimeBetweenPings()).thenAnswer(new Answer<Integer>() {
			@Override public Integer answer(final InvocationOnMock invocation) throws Throwable {
				return maximumTimeBetweenPings;
			}
		});

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				socketBufferSize = invocation.getArgumentAt(0, Integer.class);
				return null;
			}
		}).when(mockGatewayHub).setSocketBufferSize(anyInt());

		when(mockGatewayHub.getSocketBufferSize()).thenAnswer(new Answer<Integer>() {
			@Override public Integer answer(final InvocationOnMock invocation) throws Throwable {
				return socketBufferSize;
			}
		});

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				startupPolicy = invocation.getArgumentAt(0, String.class);
				return null;
			}
		}).when(mockGatewayHub).setStartupPolicy(anyString());

		when(mockGatewayHub.getStartupPolicy()).thenAnswer(new Answer<String>() {
			@Override public String answer(final InvocationOnMock invocation) throws Throwable {
				return startupPolicy;
			}
		});

		when(mockGatewayHub.addGateway(anyString())).thenAnswer(new Answer<Gateway>() {
			@Override public Gateway answer(final InvocationOnMock invocation) throws Throwable {
				String id = invocation.getArgumentAt(0, String.class);
				return mockGatewayHub.addGateway(id, Gateway.DEFAULT_CONCURRENCY_LEVEL);
			}
		});

		when(mockGatewayHub.addGateway(anyString(), anyInt())).thenAnswer(new Answer<Gateway>() {
			@Override public Gateway answer(final InvocationOnMock invocation) throws Throwable {
				String id = invocation.getArgumentAt(0, String.class);
				Integer concurrencyLevel = invocation.getArgumentAt(1, Integer.class);
				Gateway mockGateway = new MockGatewayFactory().mockGateway(id, concurrencyLevel);
				when(mockGateway.getGatewayHubId()).thenReturn(id);
				gateways.add(mockGateway);
				return mockGateway;
			}
		});

		return mockGatewayHub;
	}

	private List<String> getGatewayIds(final List<Gateway> gateways) {
		List<String> gatewayIds = new ArrayList<String>(gateways.size());

		for (Gateway gateway : gateways) {
			gatewayIds.add(gateway.getId());
		}

		return gatewayIds;
	}

}
