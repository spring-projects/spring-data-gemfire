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

package org.springframework.data.gemfire.config.xml;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.List;

import javax.annotation.Resource;

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.util.Gateway;
import com.gemstone.gemfire.cache.util.GatewayEvent;
import com.gemstone.gemfire.cache.util.GatewayEventListener;
import com.gemstone.gemfire.cache.util.GatewayHub;
import com.gemstone.gemfire.cache.util.GatewayQueueAttributes;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The GatewayHubNamespaceTest class is a test suite of test cases testing the contract and functionality
 * of the GatewayHub SDG XML namespace (XSD) configuration meta-data.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer
 * @see org.springframework.data.gemfire.wan.GatewayHubFactoryBean
 * @see org.springframework.data.gemfire.wan.GatewayProxy
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @see com.gemstone.gemfire.cache.util.Gateway
 * @see com.gemstone.gemfire.cache.util.GatewayHub
 * @since 1.5.3
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(initializers = GemfireTestApplicationContextInitializer.class)
@SuppressWarnings({"deprecation", "unused" })
public class GatewayHubNamespaceTest {

	@Resource(name = "Example")
	private Region<?, ?> example;

	@Autowired
	private GatewayHub gatewayHub;

	protected Gateway getGatewayById(final List<Gateway> gateways, final String id) {
		for (Gateway gateway : gateways) {
			if (gateway.getId().equals(id)) {
				return gateway;
			}
		}

		return null;
	}

	@Test
	public void testRegionConfiguration() {
		assertNotNull("The '/Example' Region was not properly initialized!", example);
		assertEquals("Example", example.getName());
		assertEquals("/Example", example.getFullPath());
		assertNotNull(example.getAttributes());
		assertEquals(DataPolicy.REPLICATE, example.getAttributes().getDataPolicy());
		assertTrue(example.getAttributes().getEnableGateway());
		assertEquals("testGatewayHub", example.getAttributes().getGatewayHubId());
	}

	@Test
	public void testGatewayConfiguration() {
		assertNotNull("The 'TestGatewayHub' GatewayHub was not properly initialized!", gatewayHub);
		assertEquals("localhost", gatewayHub.getBindAddress());
		assertEquals("TestGatewayHub", gatewayHub.getId());
		assertTrue(gatewayHub.getManualStart());
		assertEquals(125, gatewayHub.getMaxConnections());
		assertEquals(5000, gatewayHub.getMaximumTimeBetweenPings());
		assertEquals(45123, gatewayHub.getPort());
		assertEquals(16384, gatewayHub.getSocketBufferSize());
		assertEquals("primary", gatewayHub.getStartupPolicy());

		List<Gateway> gateways = gatewayHub.getGateways();

		assertNotNull(gateways);
		assertFalse(gateways.isEmpty());
		assertEquals(3, gateways.size());

		Gateway gatewayOne = getGatewayById(gateways, "gateway1");

		assertNotNull(gatewayOne);
		assertEquals("gateway1", gatewayOne.getId());
		assertEquals(8, gatewayOne.getConcurrencyLevel());
		assertEquals(Gateway.OrderPolicy.THREAD, gatewayOne.getOrderPolicy());
		assertEquals(65536, gatewayOne.getSocketBufferSize());
		assertEquals(75000, gatewayOne.getSocketReadTimeout());
		assertTrue(gatewayOne.getEndpoints() == null || gatewayOne.getEndpoints().isEmpty());

		List<GatewayEventListener> gatewayEventListeners = gatewayOne.getListeners();

		assertNotNull(gatewayEventListeners);
		assertFalse(gatewayEventListeners.isEmpty());
		assertEquals(2, gatewayEventListeners.size());
		assertTrue(gatewayEventListeners.get(0) instanceof TestGatewayListener);
		assertEquals("ListenerOne", gatewayEventListeners.get(0).toString());
		assertTrue(gatewayEventListeners.get(1) instanceof TestGatewayListener);
		assertEquals("ListenerTwo", gatewayEventListeners.get(1).toString());

		GatewayQueueAttributes gatewayQueueAttributes = gatewayOne.getQueueAttributes();

		assertNotNull(gatewayQueueAttributes);
		assertEquals(99, gatewayQueueAttributes.getAlertThreshold());
		assertTrue(gatewayQueueAttributes.getBatchConflation());
		assertEquals(3, gatewayQueueAttributes.getBatchSize());
		assertEquals(10, gatewayQueueAttributes.getBatchTimeInterval());
		assertEquals("TestGatewayQueueDiskStore", gatewayQueueAttributes.getDiskStoreName());
		assertFalse(gatewayQueueAttributes.getEnablePersistence());
		assertEquals(5, gatewayQueueAttributes.getMaximumQueueMemory());

		Gateway gatewayTwo = getGatewayById(gateways, "gateway2");

		assertNotNull(gatewayTwo);
		assertEquals("gateway2", gatewayTwo.getId());
		assertEquals(Gateway.DEFAULT_CONCURRENCY_LEVEL, gatewayTwo.getConcurrencyLevel());
		assertNull(gatewayTwo.getOrderPolicy());
		assertEquals(Gateway.DEFAULT_SOCKET_BUFFER_SIZE, gatewayTwo.getSocketBufferSize());
		assertEquals(Gateway.DEFAULT_SOCKET_READ_TIMEOUT, gatewayTwo.getSocketReadTimeout());
		assertTrue(gatewayTwo.getListeners() == null || gatewayTwo.getListeners().isEmpty());

		List gatewayEndpoints = gatewayTwo.getEndpoints();

		assertNotNull(gatewayEndpoints);
		assertFalse(gatewayEndpoints.isEmpty());
		assertEquals(2, gatewayEndpoints.size());

		Gateway.Endpoint gatewayEndpointOne = (Gateway.Endpoint) gatewayEndpoints.get(0);

		assertEquals("endpoint1", gatewayEndpointOne.getId());
		assertEquals("localhost", gatewayEndpointOne.getHost());
		assertEquals(1234, gatewayEndpointOne.getPort());

		Gateway.Endpoint gatewayEndpointTwo = (Gateway.Endpoint) gatewayEndpoints.get(1);

		assertEquals("endpoint2", gatewayEndpointTwo.getId());
		assertEquals("localhost", gatewayEndpointTwo.getHost());
		assertEquals(4321, gatewayEndpointTwo.getPort());

		gatewayQueueAttributes = gatewayTwo.getQueueAttributes();

		assertNotNull(gatewayQueueAttributes);
		assertEquals(GatewayQueueAttributes.DEFAULT_ALERT_THRESHOLD, gatewayQueueAttributes.getAlertThreshold());
		assertFalse(gatewayQueueAttributes.getBatchConflation());
		assertEquals(6, gatewayQueueAttributes.getBatchSize());
		assertEquals(20, gatewayQueueAttributes.getBatchTimeInterval());
		assertNull(gatewayQueueAttributes.getDiskStoreName());
		assertTrue(gatewayQueueAttributes.getEnablePersistence());
		assertEquals(GatewayQueueAttributes.DEFAULT_MAXIMUM_QUEUE_MEMORY, gatewayQueueAttributes.getMaximumQueueMemory());

		Gateway gatewayThree = getGatewayById(gateways, "gateway3");

		assertNotNull(gatewayThree);
		assertEquals("gateway3", gatewayThree.getId());
		assertEquals(Gateway.DEFAULT_CONCURRENCY_LEVEL, gatewayThree.getConcurrencyLevel());
		assertNull(gatewayThree.getOrderPolicy());
		assertEquals(Gateway.DEFAULT_SOCKET_BUFFER_SIZE, gatewayThree.getSocketBufferSize());
		assertEquals(Gateway.DEFAULT_SOCKET_READ_TIMEOUT, gatewayThree.getSocketReadTimeout());
		assertTrue(gatewayThree.getEndpoints() == null || gatewayThree.getEndpoints().isEmpty());

		gatewayEventListeners = gatewayThree.getListeners();

		assertNotNull(gatewayEventListeners);
		assertFalse(gatewayEventListeners.isEmpty());
		assertEquals(1, gatewayEventListeners.size());
		assertTrue(gatewayEventListeners.get(0) instanceof TestGatewayListener);
		assertEquals("ListenerTwo", gatewayEventListeners.get(0).toString());
	}

	public static final class TestGatewayListener implements GatewayEventListener {

		private String name;

		public void setName(final String name) {
			this.name = name;
		}

		@Override
		public boolean processEvents(final List<GatewayEvent> events) {
			return false;
		}

		@Override
		public void close() {
		}

		@Override
		public String toString() {
			return name;
		}
	}

}
