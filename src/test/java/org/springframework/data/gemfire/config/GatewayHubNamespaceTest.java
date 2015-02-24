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

package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.util.List;
import javax.annotation.Resource;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.test.GemfireTestApplicationContextInitializer;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.util.Gateway;
import com.gemstone.gemfire.cache.util.GatewayEvent;
import com.gemstone.gemfire.cache.util.GatewayEventListener;
import com.gemstone.gemfire.cache.util.GatewayHub;
import com.gemstone.gemfire.cache.util.GatewayQueueAttributes;

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
		assertNotNull("The 'testGatewayHub' GatewayHub was not properly initialized!", gatewayHub);
		assertEquals("localhost", gatewayHub.getBindAddress());
		assertEquals("testGatewayHub", gatewayHub.getId());
		assertTrue(gatewayHub.getManualStart());
		assertEquals(5000, gatewayHub.getMaximumTimeBetweenPings());
		assertEquals(45123, gatewayHub.getPort());
		assertEquals(16384, gatewayHub.getSocketBufferSize());
		assertEquals("primary", gatewayHub.getStartupPolicy());

		List<Gateway> gateways = gatewayHub.getGateways();

		assertNotNull(gateways);
		assertFalse(gateways.isEmpty());
		assertEquals(2, gateways.size());

		Gateway gatewayOne = gateways.get(0);

		assertEquals("gateway1", gatewayOne.getId());
		assertEquals(8, gatewayOne.getConcurrencyLevel());
		assertEquals(Gateway.OrderPolicy.THREAD, gatewayOne.getOrderPolicy());
		assertEquals(65536, gatewayOne.getSocketBufferSize());
		assertEquals(15000, gatewayOne.getSocketReadTimeout());
		assertNotNull(gatewayOne.getListeners());
		assertFalse(gatewayOne.getListeners().isEmpty());
		assertEquals(1, gatewayOne.getListeners().size());
		assertTrue(gatewayOne.getListeners().get(0) instanceof TestGatewayListener);

		GatewayQueueAttributes gatewayQueueAttributes = gatewayOne.getQueueAttributes();

		assertNotNull(gatewayQueueAttributes);
		assertEquals(99, gatewayQueueAttributes.getAlertThreshold());
		assertTrue(gatewayQueueAttributes.getBatchConflation());
		assertEquals(3, gatewayQueueAttributes.getBatchSize());
		assertEquals(10, gatewayQueueAttributes.getBatchTimeInterval());
		assertEquals(5, gatewayQueueAttributes.getMaximumQueueMemory());
		assertFalse(gatewayQueueAttributes.getEnablePersistence());

		Gateway gatewayTwo = gateways.get(1);

		assertEquals("gateway2", gatewayTwo.getId());

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
	}

	public static final class TestGatewayListener implements GatewayEventListener {

		@Override
		public boolean processEvents(final List<GatewayEvent> events) {
			return false;
		}

		@Override
		public void close() {
		}
	}

}
