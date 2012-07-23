/*
 * Copyright 2010-2012 the original author or authors.
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
package org.springframework.data.gemfire.config;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Set;

import org.junit.Before;
import org.junit.Test;
import org.springframework.data.gemfire.RecreatingContextTest;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.wan.GatewaySenderFactoryBean;

import com.gemstone.gemfire.cache.AsyncEventQueue;
import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.util.Gateway.OrderPolicy;
import com.gemstone.gemfire.cache.wan.AsyncEvent;
import com.gemstone.gemfire.cache.wan.AsyncEventListener;
import com.gemstone.gemfire.cache.wan.GatewayEventFilter;
import com.gemstone.gemfire.cache.wan.GatewayReceiver;
import com.gemstone.gemfire.cache.wan.GatewaySender;
import com.gemstone.gemfire.cache.wan.GatewayTransportFilter;

/**
 * This test is only valid for GF 7.0 and above
 * 
 * @author David Turanski
 * 
 */
public class GemfireV7GatewayNamespaceTest extends RecreatingContextTest {
	@Override
	protected String location() {
		return "/org/springframework/data/gemfire/config/gateway-v7-ns.xml";
	}

	@Before
	@Override
	public void createCtx() {
		if (ParsingUtils.GEMFIRE_VERSION.startsWith("7")) {
			super.createCtx();
		}
	}

	/*
	 * Faster this way
	 */
	@Test
	public void test() throws Exception {
		if (ctx != null) {
			testGatewaySender();
			testInnerGatewaySender();
			testInnerGatewayReceiver();
			testAsyncEventQueue();
		}
	}

	/**
	 * 
	 */
	private void testAsyncEventQueue() {
		AsyncEventQueue aseq = ctx.getBean("async-event-queue", AsyncEventQueue.class);
		assertEquals(10, aseq.getBatchSize());
		assertTrue(aseq.isPersistent());
		assertEquals("diskstore", aseq.getDiskStoreName());
		assertEquals(50, aseq.getMaximumQueueMemory());
	}

	private void testGatewaySender() throws Exception {
		GatewaySenderFactoryBean gwsfb = ctx.getBean("&gateway-sender", GatewaySenderFactoryBean.class);
		Cache cache = TestUtils.readField("cache", gwsfb);
		assertNotNull(cache);
		List<GatewayEventFilter> eventFilters = TestUtils.readField("eventFilters", gwsfb);
		assertNotNull(eventFilters);
		assertEquals(2, eventFilters.size());
		assertTrue(eventFilters.get(0) instanceof TestEventFilter);

		List<GatewayTransportFilter> transportFilters = TestUtils.readField("transportFilters", gwsfb);
		assertNotNull(transportFilters);
		assertEquals(2, transportFilters.size());
		assertTrue(transportFilters.get(0) instanceof TestTransportFilter);

		assertEquals(2, TestUtils.readField("remoteDistributedSystemId", gwsfb));
		assertEquals(10, TestUtils.readField("alertThreshold", gwsfb));
		assertEquals(11, TestUtils.readField("batchSize", gwsfb));
		assertEquals(12, TestUtils.readField("dispatcherThreads", gwsfb));
		assertEquals(true, TestUtils.readField("manualStart", gwsfb));
	}

	private void testInnerGatewaySender() {
		Region<?, ?> region = ctx.getBean("region-inner-gateway-sender", Region.class);
		GatewaySender gws = ctx.getBean("gateway-sender", GatewaySender.class);
		assertNotNull(region.getAttributes().getGatewaySenders());
		assertEquals(2, region.getAttributes().getGatewaySenders().size());

		// Isolate the inner gateway
		Set<GatewaySender> gatewaySenders = region.getAttributes().getGatewaySenders();
		assertTrue(gatewaySenders.remove(gws));
		gatewaySenders.remove(gws);

		gws = gatewaySenders.iterator().next();
		List<GatewayEventFilter> eventFilters = gws.getGatewayEventFilters();
		assertNotNull(eventFilters);
		assertEquals(1, eventFilters.size());
		assertTrue(eventFilters.get(0) instanceof TestEventFilter);

		List<GatewayTransportFilter> transportFilters = gws.getGatewayTransportFilters();

		assertNotNull(transportFilters);
		assertEquals(1, transportFilters.size());
		assertTrue(transportFilters.get(0) instanceof TestTransportFilter);

		assertEquals(1, gws.getRemoteDSId());
		assertEquals(true, gws.isManualStart());
		assertEquals(10, gws.getAlertThreshold());
		assertEquals(11, gws.getBatchSize());
		assertEquals(3000, gws.getBatchTimeInterval());
		assertEquals(2, gws.getDispatcherThreads());
		assertEquals("diskstore", gws.getDiskStoreName());
		assertTrue(gws.isBatchConflationEnabled());
		assertEquals(50, gws.getMaximumQueueMemory());
		assertEquals(OrderPolicy.THREAD, gws.getOrderPolicy());
		assertTrue(gws.isPersistenceEnabled());
		assertTrue(gws.isParallel());
		assertEquals(16536, gws.getSocketBufferSize());
		assertEquals(3000, gws.getSocketReadTimeout());
	}

	private void testInnerGatewayReceiver() {
		GatewayReceiver gwr = ctx.getBean("gateway-receiver", GatewayReceiver.class);
		assertEquals(12345, gwr.getStartPort());
		assertEquals(23456, gwr.getEndPort());
		assertEquals("192.168.0.1", gwr.getBindAddress());
		assertEquals(3000, gwr.getMaximumTimeBetweenPings());
		assertEquals(16536, gwr.getSocketBufferSize());
	}

	@SuppressWarnings("rawtypes")
	public static class TestEventFilter implements GatewayEventFilter {

		@Override
		public void close() {
			// TODO Auto-generated method stub
		}

		@Override
		public void afterAcknowledgement(AsyncEvent arg0) {
			// TODO Auto-generated method stub
		}

		@Override
		public boolean beforeEnqueue(AsyncEvent arg0) {
			// TODO Auto-generated method stub
			return false;
		}

		@Override
		public boolean beforeTransmit(AsyncEvent arg0) {
			// TODO Auto-generated method stub
			return false;
		}

	}

	public static class TestTransportFilter implements GatewayTransportFilter {

		@Override
		public void close() {
			// TODO Auto-generated method stub

		}

		@Override
		public InputStream getInputStream(InputStream arg0) {
			// TODO Auto-generated method stub
			return null;
		}

		@Override
		public OutputStream getOutputStream(OutputStream arg0) {
			// TODO Auto-generated method stub
			return null;
		}

	}

	@SuppressWarnings("rawtypes")
	public static class TestAsyncEventListener implements AsyncEventListener {

		@Override
		public void close() {
			// TODO Auto-generated method stub
		}

		@Override
		public boolean processEvents(List<AsyncEvent> arg0) {
			// TODO Auto-generated method stub
			return false;
		}

	}
}
