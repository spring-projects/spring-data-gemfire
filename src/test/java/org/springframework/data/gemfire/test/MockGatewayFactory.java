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

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.apache.geode.cache.util.Gateway;
import org.apache.geode.cache.util.GatewayEventListener;
import org.apache.geode.cache.util.GatewayQueueAttributes;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.data.gemfire.test.support.AbstractUnitAndIntegrationTestsWithMockSupport;

/**
 * The MockGatewayFactory class is a factory for creating mock GemFire Gateways, GatewayQueueAttributes
 * and Gateway.Endpoints.
 *
 * @author John Blum
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.wan.GatewayHubFactoryBean
 * @see org.apache.geode.cache.util.Gateway
 * @see org.apache.geode.cache.util.GatewayHub
 * @since 1.5.3
 */
@SuppressWarnings({ "deprecation", "unused" })
public class MockGatewayFactory extends AbstractUnitAndIntegrationTestsWithMockSupport {

	private Boolean batchConflation = GatewayQueueAttributes.DEFAULT_BATCH_CONFLATION;
	private Boolean persistence = GatewayQueueAttributes.DEFAULT_ENABLE_PERSISTENCE;

	private Gateway.OrderPolicy orderPolicy;

	private GatewayQueueAttributes queueAttributes;

	private Integer alertThreshold = GatewayQueueAttributes.DEFAULT_ALERT_THRESHOLD;
	private Integer batchSize = GatewayQueueAttributes.DEFAULT_BATCH_SIZE;
	private Integer batchTimeInterval = GatewayQueueAttributes.DEFAULT_BATCH_TIME_INTERVAL;
	private Integer maxQueueMemory = GatewayQueueAttributes.DEFAULT_MAXIMUM_QUEUE_MEMORY;
	private Integer socketBufferSize = Gateway.DEFAULT_SOCKET_BUFFER_SIZE;
	private Integer socketReadTimeout = Gateway.DEFAULT_SOCKET_READ_TIMEOUT;

	private List<Gateway.Endpoint> endpoints;
	private List<GatewayEventListener> listeners;

	private String diskStoreName;

	public MockGatewayFactory() {
		endpoints = new ArrayList<Gateway.Endpoint>();
		listeners = new ArrayList<GatewayEventListener>();
		queueAttributes = mockGatewayQueueAttributes();
	}

	public Gateway mockGateway(final String id, final int concurrencyLevel) {
		Gateway mockGateway = mock(Gateway.class, String.format("%1$s.MockGateway", getMockId()));

		when(mockGateway.getConcurrencyLevel()).thenReturn(concurrencyLevel);
		when(mockGateway.getEndpoints()).thenReturn(endpoints);
		when(mockGateway.getId()).thenReturn(id);
		when(mockGateway.getListeners()).thenReturn(listeners);
		when(mockGateway.getQueueSize()).thenThrow(new UnsupportedOperationException(NOT_IMPLEMENTED));

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				orderPolicy = invocation.getArgumentAt(0, Gateway.OrderPolicy.class);
				return null;
			}
		}).when(mockGateway).setOrderPolicy(any(Gateway.OrderPolicy.class));

		when(mockGateway.getOrderPolicy()).thenAnswer(new Answer<Gateway.OrderPolicy>() {
			@Override public Gateway.OrderPolicy answer(final InvocationOnMock invocation) throws Throwable {
				return orderPolicy;
			}
		});

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				queueAttributes = invocation.getArgumentAt(0, GatewayQueueAttributes.class);
				return null;
			}
		}).when(mockGateway).setQueueAttributes(any(GatewayQueueAttributes.class));

		when(mockGateway.getQueueAttributes()).thenAnswer(new Answer<GatewayQueueAttributes>() {
			@Override public GatewayQueueAttributes answer(final InvocationOnMock invocation) throws Throwable {
				return queueAttributes;
			}
		});

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				socketBufferSize = invocation.getArgumentAt(0, Integer.class);
				return null;
			}
		}).when(mockGateway).setSocketBufferSize(anyInt());

		when(mockGateway.getSocketBufferSize()).thenAnswer(new Answer<Integer>() {
			@Override public Integer answer(final InvocationOnMock invocation) throws Throwable {
				return socketBufferSize;
			}
		});

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				socketReadTimeout = invocation.getArgumentAt(0, Integer.class);
				return null;
			}
		}).when(mockGateway).setSocketReadTimeout(anyInt());

		when(mockGateway.getSocketReadTimeout()).thenAnswer(new Answer<Integer>() {
			@Override public Integer answer(final InvocationOnMock invocation) throws Throwable {
				return socketReadTimeout;
			}
		});

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				String id = invocation.getArgumentAt(0, String.class);
				String host = invocation.getArgumentAt(1, String.class);
				int port = invocation.getArgumentAt(2, Integer.class);
				endpoints.add(mockGatewayEndpoint(id, host, port));
				return null;
			}
		}).when(mockGateway).addEndpoint(anyString(), anyString(), anyInt());

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				listeners.add(invocation.getArgumentAt(0, GatewayEventListener.class));
				return null;
			}
		}).when(mockGateway).addListener(any(GatewayEventListener.class));

		return mockGateway;
	}

	public Gateway.Endpoint mockGatewayEndpoint(final String id, final String host, final int port) {
		Gateway.Endpoint mockEndpoint = mock(Gateway.Endpoint.class,
			String.format("%1$s.MockGatewayEndpoint", getMockId()));

		when(mockEndpoint.getId()).thenReturn(id);
		when(mockEndpoint.getHost()).thenReturn(host);
		when(mockEndpoint.getPort()).thenReturn(port);

		return mockEndpoint;
	}

	public GatewayQueueAttributes mockGatewayQueueAttributes() {
		GatewayQueueAttributes mockQueueAttributes = mock(GatewayQueueAttributes.class,
			String.format("%1$s.MockGatewayQueueAttributes", getMockId()));

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				alertThreshold = invocation.getArgumentAt(0, Integer.class);
				return null;
			}
		}).when(mockQueueAttributes).setAlertThreshold(anyInt());

		when(mockQueueAttributes.getAlertThreshold()).thenAnswer(new Answer<Integer>() {
			@Override public Integer answer(final InvocationOnMock invocation) throws Throwable {
				return alertThreshold;
			}
		});

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				batchConflation = invocation.getArgumentAt(0, Boolean.class);
				return null;
			}
		}).when(mockQueueAttributes).setBatchConflation(anyBoolean());

		when(mockQueueAttributes.getBatchConflation()).thenAnswer(new Answer<Boolean>() {
			@Override public Boolean answer(final InvocationOnMock invocation) throws Throwable {
				return batchConflation;
			}
		});

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				batchSize = invocation.getArgumentAt(0, Integer.class);
				return null;
			}
		}).when(mockQueueAttributes).setBatchSize(anyInt());

		when(mockQueueAttributes.getBatchSize()).thenAnswer(new Answer<Integer>() {
			@Override public Integer answer(final InvocationOnMock invocation) throws Throwable {
				return batchSize;
			}
		});

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				batchTimeInterval = invocation.getArgumentAt(0, Integer.class);
				return null;
			}
		}).when(mockQueueAttributes).setBatchTimeInterval(anyInt());

		when(mockQueueAttributes.getBatchTimeInterval()).thenAnswer(new Answer<Integer>() {
			@Override public Integer answer(final InvocationOnMock invocation) throws Throwable {
				return batchTimeInterval;
			}
		});

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				diskStoreName = invocation.getArgumentAt(0, String.class);
				return null;
			}
		}).when(mockQueueAttributes).setDiskStoreName(anyString());

		when(mockQueueAttributes.getDiskStoreName()).thenAnswer(new Answer<String>() {
			@Override public String answer(final InvocationOnMock invocation) throws Throwable {
				return diskStoreName;
			}
		});

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				persistence = invocation.getArgumentAt(0, Boolean.class);
				return null;
			}
		}).when(mockQueueAttributes).setEnablePersistence(anyBoolean());

		when(mockQueueAttributes.getEnablePersistence()).thenAnswer(new Answer<Boolean>() {
			@Override public Boolean answer(final InvocationOnMock invocation) throws Throwable {
				return persistence;
			}
		});

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				maxQueueMemory = invocation.getArgumentAt(0, Integer.class);
				return null;
			}
		}).when(mockQueueAttributes).setMaximumQueueMemory(anyInt());

		when(mockQueueAttributes.getMaximumQueueMemory()).thenAnswer(new Answer<Integer>() {
			@Override public Integer answer(final InvocationOnMock invocation) throws Throwable {
				return maxQueueMemory;
			}
		});

		return mockQueueAttributes;
	}

}
