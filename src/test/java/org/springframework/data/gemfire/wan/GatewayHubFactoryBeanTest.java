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

package org.springframework.data.gemfire.wan;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.util.Gateway;
import com.gemstone.gemfire.cache.util.GatewayEventListener;
import com.gemstone.gemfire.cache.util.GatewayHub;
import com.gemstone.gemfire.cache.util.GatewayQueueAttributes;

/**
 * The GatewayHubFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the GatewayHubFactoryBean.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.wan.GatewayHubFactoryBean
 * @since 1.7.0
 */
@SuppressWarnings("deprecation")
public class GatewayHubFactoryBeanTest {

	private Cache mockCache;

	private GatewayHubFactoryBean factoryBean;

	@Before
	public void setup() {
		mockCache = mock(Cache.class, "GemFire Cache");
		factoryBean = new GatewayHubFactoryBean(mockCache);
	}

	@Test
	public void testGetObjectAndObjectType() throws Exception {
		assertNull(factoryBean.getObject());
		assertEquals(GatewayHub.class, factoryBean.getObjectType());
	}

	@Test
	public void testSetAndGetBindAddress() {
		assertEquals(GatewayHub.DEFAULT_BIND_ADDRESS, factoryBean.getBindAddress());
		factoryBean.setBindAddress("10.127.255.1");
		assertEquals("10.127.255.1", factoryBean.getBindAddress());
		factoryBean.setBindAddress(null);
		assertEquals(GatewayHub.DEFAULT_BIND_ADDRESS, factoryBean.getBindAddress());
	}

	@Test
	public void testGetGateways() {
		List<GatewayProxy> gateways = factoryBean.getGateways();

		assertNotNull(gateways);
		assertTrue(gateways.isEmpty());
	}

	@Test
	public void testSetAndIsManualStart() {
		assertEquals(GatewayHub.DEFAULT_MANUAL_START, factoryBean.isManualStart(GatewayHub.DEFAULT_MANUAL_START));
		factoryBean.setManualStart(true);
		assertTrue(factoryBean.isManualStart(GatewayHub.DEFAULT_MANUAL_START));
		factoryBean.setManualStart(false);
		assertFalse(factoryBean.isManualStart(true));
		factoryBean.setManualStart(null);
		assertEquals(GatewayHub.DEFAULT_MANUAL_START, factoryBean.isManualStart(GatewayHub.DEFAULT_MANUAL_START));
	}

	@Test
	public void testSetAndGetMaxConnections() {
		assertEquals(GatewayHub.DEFAULT_MAX_CONNECTIONS, factoryBean.getMaxConnections().intValue());
		factoryBean.setMaxConnections(8192);
		assertEquals(8192, factoryBean.getMaxConnections().intValue());
		factoryBean.setMaxConnections(null);
		assertEquals(GatewayHub.DEFAULT_MAX_CONNECTIONS, factoryBean.getMaxConnections().intValue());
	}

	@Test
	public void testSetAndGetMaximumTimeBetweenPings() {
		assertEquals(GatewayHub.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS, factoryBean.getMaximumTimeBetweenPings().intValue());
		factoryBean.setMaximumTimeBetweenPings(15000);
		assertEquals(15000, factoryBean.getMaximumTimeBetweenPings().intValue());
		factoryBean.setMaximumTimeBetweenPings(null);
		assertEquals(GatewayHub.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS, factoryBean.getMaximumTimeBetweenPings().intValue());
	}

	@Test
	public void testSetAndGetPort() {
		assertEquals(GatewayHub.DEFAULT_PORT, factoryBean.getPort().intValue());
		factoryBean.setPort(15221);
		assertEquals(15221, factoryBean.getPort().intValue());
		factoryBean.setPort(null);
		assertEquals(GatewayHub.DEFAULT_PORT, factoryBean.getPort().intValue());
	}

	@Test
	public void testSetAndGetSocketBufferSize() {
		assertEquals(GatewayHub.DEFAULT_SOCKET_BUFFER_SIZE, factoryBean.getSocketBufferSize().intValue());
		factoryBean.setSocketBufferSize(16384);
		assertEquals(16384, factoryBean.getSocketBufferSize().intValue());
		factoryBean.setSocketBufferSize(null);
		assertEquals(GatewayHub.DEFAULT_SOCKET_BUFFER_SIZE, factoryBean.getSocketBufferSize().intValue());
	}

	@Test
	public void testSetAndGetStartUpPolicy() {
		assertEquals(StartupPolicyType.DEFAULT, factoryBean.getStartupPolicy());
		factoryBean.setStartupPolicy(StartupPolicyType.PRIMARY);
		assertEquals(StartupPolicyType.PRIMARY, factoryBean.getStartupPolicy());
		factoryBean.setStartupPolicy(null);
		assertEquals(StartupPolicyType.DEFAULT, factoryBean.getStartupPolicy());
		factoryBean.setStartupPolicy(StartupPolicyType.SECONDARY);
		assertEquals(StartupPolicyType.SECONDARY, factoryBean.getStartupPolicy());
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAfterPropertiesSetWitNullCache() throws Exception {
		try {
			new GatewayHubFactoryBean(null).afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Cache must not be null.", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAfterPropertiesSetWithNullName() throws Exception {
		try {
			factoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Name must not be null.", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testDoInit() throws Exception {
		String gatewayHubName = "testDoInit";

		GatewayProxy.GatewayEndpoint gatewayEndpointOne = new GatewayProxy.GatewayEndpoint();

		gatewayEndpointOne.setHost("localhost");
		gatewayEndpointOne.setId("123");
		gatewayEndpointOne.setPort(2121);

		GatewayProxy.GatewayEndpoint gatewayEndpointTwo = new GatewayProxy.GatewayEndpoint();

		gatewayEndpointOne.setHost("localhost");
		gatewayEndpointOne.setId("456");
		gatewayEndpointOne.setPort(4242);

		GatewayEventListener mockGatewayListener = mock(GatewayEventListener.class,
			"testDoInit.MockGatewayEventListener");

		GatewayProxy.GatewayQueue gatewayQueue = new GatewayProxy.GatewayQueue();

		gatewayQueue.setAlertThreshold(20);
		gatewayQueue.setBatchSize(100);
		gatewayQueue.setBatchTimeInterval(60000);
		gatewayQueue.setDiskStoreRef("diskX");
		gatewayQueue.setEnableBatchConflation(true);
		gatewayQueue.setMaximumQueueMemory(1024);
		gatewayQueue.setPersistent(true);

		GatewayProxy gatewayProxy = new GatewayProxy();

		gatewayProxy.setId("gatewayProxyId");
		gatewayProxy.setConcurrencyLevel(4);
		gatewayProxy.setEndpoints(Arrays.asList(gatewayEndpointOne, gatewayEndpointTwo));
		gatewayProxy.setListeners(Arrays.asList(mockGatewayListener));
		gatewayProxy.setOrderPolicy(Gateway.OrderPolicy.THREAD);
		gatewayProxy.setQueue(gatewayQueue);
		gatewayProxy.setSocketBufferSize(16384);
		gatewayProxy.setSocketReadTimeout(300);

		GatewayHub mockGatewayHub = mock(GatewayHub.class, "testDoInit.MockGatewayHub");

		Gateway mockGateway = mock(Gateway.class, "testDoInit.MockGateway");

		GatewayQueueAttributes mockGatewayQueueAttributes = mock(GatewayQueueAttributes.class,
			"testDoInit.MockGatewayQueueAttributes");

		when(mockCache.addGatewayHub(eq(gatewayHubName), eq(8484))).thenReturn(mockGatewayHub);
		when(mockCache.getGatewayHub(eq(gatewayHubName))).thenReturn(mockGatewayHub);
		when(mockGatewayHub.addGateway(eq(gatewayProxy.getId()), eq(gatewayProxy.getConcurrencyLevel().intValue())))
			.thenReturn(mockGateway);
		when(mockGatewayHub.getManualStart()).thenReturn(false);
		when(mockGateway.getQueueAttributes()).thenReturn(mockGatewayQueueAttributes);

		factoryBean.setBindAddress("10.124.210.42");
		factoryBean.setGateways(Arrays.asList(gatewayProxy));
		factoryBean.setManualStart(false);
		factoryBean.setMaxConnections(50);
		factoryBean.setMaximumTimeBetweenPings(20480);
		factoryBean.setName(gatewayHubName);
		factoryBean.setPort(8484);
		factoryBean.setSocketBufferSize(4096);
		factoryBean.setStartupPolicy(StartupPolicyType.PRIMARY);
		factoryBean.afterPropertiesSet();

		verify(mockGatewayHub, times(1)).setBindAddress(eq("10.124.210.42"));
		verify(mockGatewayHub, times(1)).setManualStart(eq(false));
		verify(mockGatewayHub, times(1)).setMaxConnections(eq(50));
		verify(mockGatewayHub, times(1)).setMaximumTimeBetweenPings(eq(20480));
		verify(mockGatewayHub, times(1)).setSocketBufferSize(eq(4096));
		verify(mockGatewayHub, times(1)).setStartupPolicy(eq(StartupPolicyType.PRIMARY.getName()));
		verify(mockGatewayHub, times(1)).addGateway(eq(gatewayProxy.getId()), eq(gatewayProxy.getConcurrencyLevel()));
		verify(mockGatewayHub, times(1)).start();
		verify(mockGateway, times(1)).addEndpoint(eq(gatewayEndpointOne.getId()), eq(gatewayEndpointOne.getHost()),
			eq(gatewayEndpointOne.getPort()));
		verify(mockGateway, times(1)).addEndpoint(eq(gatewayEndpointTwo.getId()), eq(gatewayEndpointTwo.getHost()),
			eq(gatewayEndpointTwo.getPort()));
		verify(mockGateway, times(1)).addListener(same(mockGatewayListener));
		verify(mockGateway, times(1)).setOrderPolicy(eq(gatewayProxy.getOrderPolicy()));
		verify(mockGateway, times(1)).setSocketBufferSize(eq(gatewayProxy.getSocketBufferSize()));
		verify(mockGateway, times(1)).setSocketReadTimeout(eq(gatewayProxy.getSocketReadTimeout()));
		verify(mockGateway, times(1)).getQueueAttributes();
		verify(mockGatewayQueueAttributes, times(1)).setAlertThreshold(eq(gatewayQueue.getAlertThreshold()));
		verify(mockGatewayQueueAttributes, times(1)).setBatchConflation(eq(gatewayQueue.getEnableBatchConflation()));
		verify(mockGatewayQueueAttributes, times(1)).setBatchSize(eq(gatewayQueue.getBatchSize()));
		verify(mockGatewayQueueAttributes, times(1)).setBatchTimeInterval(eq(gatewayQueue.getBatchTimeInterval()));
		verify(mockGatewayQueueAttributes, times(1)).setDiskStoreName(eq(gatewayQueue.getDiskStoreRef()));
		verify(mockGatewayQueueAttributes, times(1)).setMaximumQueueMemory(eq(gatewayQueue.getMaximumQueueMemory()));
		verify(mockGatewayQueueAttributes, times(1)).setEnablePersistence(eq(gatewayQueue.getPersistent()));
	}

	@Test
	public void testGatewayQueueWithOverflowNoPersistence() throws Exception {
		String gatewayHubName = "testGatewayQueueWithOverflowNoPersistence";

		GatewayProxy.GatewayQueue gatewayQueue = new GatewayProxy.GatewayQueue();

		gatewayQueue.setAlertThreshold(100);
		gatewayQueue.setBatchSize(250);
		gatewayQueue.setBatchTimeInterval(120000);
		gatewayQueue.setDiskStoreRef("diskZ");
		gatewayQueue.setEnableBatchConflation(true);
		gatewayQueue.setMaximumQueueMemory(2048);
		gatewayQueue.setPersistent(false);

		GatewayProxy gatewayProxy = new GatewayProxy();

		gatewayProxy.setId("gatewayProxyId");
		gatewayProxy.setConcurrencyLevel(2);
		gatewayProxy.setEndpoints(null);
		gatewayProxy.setListeners(null);
		gatewayProxy.setOrderPolicy(Gateway.OrderPolicy.THREAD);
		gatewayProxy.setQueue(gatewayQueue);
		gatewayProxy.setSocketBufferSize(4096);
		gatewayProxy.setSocketReadTimeout(60);

		GatewayHub mockGatewayHub = mock(GatewayHub.class, "testGatewayQueueWithOverflowNoPersistence.MockGatewayHub");

		Gateway mockGateway = mock(Gateway.class, "testGatewayQueueWithOverflowNoPersistence.MockGateway");

		GatewayQueueAttributes mockGatewayQueueAttributes = mock(GatewayQueueAttributes.class,
			"testGatewayQueueWithOverflowNoPersistence.MockGatewayQueueAttributes");

		when(mockCache.addGatewayHub(eq(gatewayHubName), eq(10224))).thenReturn(mockGatewayHub);
		when(mockCache.getGatewayHub(eq(gatewayHubName))).thenReturn(mockGatewayHub);
		when(mockGatewayHub.addGateway(eq(gatewayProxy.getId()), eq(gatewayProxy.getConcurrencyLevel())))
			.thenReturn(mockGateway);
		when(mockGatewayHub.getManualStart()).thenReturn(GatewayHub.DEFAULT_MANUAL_START);
		when(mockGateway.getQueueAttributes()).thenReturn(mockGatewayQueueAttributes);

		factoryBean.setGateways(Arrays.asList(gatewayProxy));
		factoryBean.setName(gatewayHubName);
		factoryBean.setPort(10224);
		factoryBean.afterPropertiesSet();

		verify(mockGatewayHub, times(1)).setBindAddress(eq(GatewayHub.DEFAULT_BIND_ADDRESS));
		verify(mockGatewayHub, times(1)).setManualStart(eq(GatewayHub.DEFAULT_MANUAL_START));
		verify(mockGatewayHub, times(1)).setMaxConnections(GatewayHub.DEFAULT_MAX_CONNECTIONS);
		verify(mockGatewayHub, times(1)).setMaximumTimeBetweenPings(eq(GatewayHub.DEFAULT_MAXIMUM_TIME_BETWEEN_PINGS));
		verify(mockGatewayHub, times(1)).setSocketBufferSize(eq(GatewayHub.DEFAULT_SOCKET_BUFFER_SIZE));
		verify(mockGatewayHub, times(1)).setStartupPolicy(eq(GatewayHub.DEFAULT_STARTUP_POLICY));
		verify(mockGatewayHub, times(1)).start();
		verify(mockGatewayHub, times(1)).addGateway(eq(gatewayProxy.getId()), eq(gatewayProxy.getConcurrencyLevel()));
		verify(mockGateway, times(1)).setOrderPolicy(eq(gatewayProxy.getOrderPolicy()));
		verify(mockGateway, times(1)).setSocketBufferSize(eq(gatewayProxy.getSocketBufferSize()));
		verify(mockGateway, times(1)).setSocketReadTimeout(eq(gatewayProxy.getSocketReadTimeout()));
		verify(mockGatewayQueueAttributes, times(1)).setAlertThreshold(gatewayQueue.getAlertThreshold());
		verify(mockGatewayQueueAttributes, times(1)).setBatchConflation(gatewayQueue.getEnableBatchConflation());
		verify(mockGatewayQueueAttributes, times(1)).setBatchSize(gatewayQueue.getBatchSize());
		verify(mockGatewayQueueAttributes, times(1)).setBatchTimeInterval(gatewayQueue.getBatchTimeInterval());
		verify(mockGatewayQueueAttributes, times(1)).setDiskStoreName(gatewayQueue.getDiskStoreRef());
		verify(mockGatewayQueueAttributes, times(1)).setMaximumQueueMemory(gatewayQueue.getMaximumQueueMemory());
		verify(mockGatewayQueueAttributes, times(1)).setEnablePersistence(gatewayQueue.getPersistent());
	}

}
