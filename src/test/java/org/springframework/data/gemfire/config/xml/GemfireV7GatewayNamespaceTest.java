/*
 * Copyright 2010-2013 the original author or authors.
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
package org.springframework.data.gemfire.config.xml;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FilenameFilter;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.asyncqueue.AsyncEvent;
import org.apache.geode.cache.asyncqueue.AsyncEventListener;
import org.apache.geode.cache.asyncqueue.AsyncEventQueue;
import org.apache.geode.cache.wan.GatewayEventFilter;
import org.apache.geode.cache.wan.GatewayQueueEvent;
import org.apache.geode.cache.wan.GatewayReceiver;
import org.apache.geode.cache.wan.GatewaySender;
import org.apache.geode.cache.wan.GatewaySender.OrderPolicy;
import org.apache.geode.cache.wan.GatewayTransportFilter;
import org.junit.AfterClass;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.data.gemfire.RecreatingSpringApplicationContextTest;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.test.GemfireTestBeanPostProcessor;
import org.springframework.data.gemfire.wan.GatewaySenderFactoryBean;

/**
 * The GemfireV7GatewayNamespaceTest class is a test suite of test cases testing the GemFire 7.0 WAN functionality
 * by configuring various Gateway senders and receivers.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.springframework.data.gemfire.RecreatingSpringApplicationContextTest
 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueue
 * @see org.apache.geode.cache.wan.GatewayReceiver
 * @see org.apache.geode.cache.wan.GatewaySender
 */
@SuppressWarnings("unused")
public class GemfireV7GatewayNamespaceTest extends RecreatingSpringApplicationContextTest {

	@AfterClass
	@SuppressWarnings("all")
	public static void tearDown() {
		for (String name : new File(".").list(new FilenameFilter() {
			@Override
			public boolean accept(File file, String filename) {
				return filename.startsWith("BACKUP");
			}
		})) {
			new File(name).delete();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.gemfire.RecreatingSpringApplicationContextTest#configureContext(ConfigurableApplicationContext)
	 */
	@Override
	protected <T extends ConfigurableApplicationContext> T configureContext(final T context) {
		context.getBeanFactory().addBeanPostProcessor(new GemfireTestBeanPostProcessor());
		return context;
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.gemfire.RecreatingSpringApplicationContextTest#location()
	 */
	@Override
	protected String location() {
		return "/org/springframework/data/gemfire/config/xml/gateway-v7-ns.xml";
	}

	@Test
	public void testAsyncEventQueue() {
		AsyncEventQueue asyncEventQueue = applicationContext.getBean("async-event-queue", AsyncEventQueue.class);

		assertNotNull(asyncEventQueue);
		assertTrue(asyncEventQueue.isBatchConflationEnabled());
		assertEquals(10, asyncEventQueue.getBatchSize());
		assertEquals(3, asyncEventQueue.getBatchTimeInterval());
		assertEquals("diskstore", asyncEventQueue.getDiskStoreName());
		assertTrue(asyncEventQueue.isDiskSynchronous());
		assertEquals(50, asyncEventQueue.getMaximumQueueMemory());
		assertEquals(OrderPolicy.KEY, asyncEventQueue.getOrderPolicy());
		assertFalse(asyncEventQueue.isParallel());
		assertTrue(asyncEventQueue.isPersistent());
	}

	@Test
	public void testGatewaySender() throws Exception {
		GatewaySenderFactoryBean gatewaySenderFactoryBean = applicationContext.getBean("&gateway-sender", GatewaySenderFactoryBean.class);

		assertNotNull(gatewaySenderFactoryBean);
		assertNotNull(TestUtils.readField("cache", gatewaySenderFactoryBean));
		assertThat((Object) TestUtils.readField("remoteDistributedSystemId", gatewaySenderFactoryBean)).isEqualTo(2);
		assertThat((Object) TestUtils.readField("alertThreshold", gatewaySenderFactoryBean)).isEqualTo(10);
		assertTrue(Boolean.TRUE.equals(TestUtils.readField("batchConflationEnabled", gatewaySenderFactoryBean)));
		assertThat((Object) TestUtils.readField("batchSize", gatewaySenderFactoryBean)).isEqualTo(11);
		assertThat((Object) TestUtils.readField("dispatcherThreads", gatewaySenderFactoryBean)).isEqualTo(12);
		assertEquals(false, TestUtils.readField("diskSynchronous", gatewaySenderFactoryBean));
		assertEquals(true, TestUtils.readField("manualStart", gatewaySenderFactoryBean));

		List<GatewayEventFilter> eventFilters = TestUtils.readField("eventFilters", gatewaySenderFactoryBean);

		assertNotNull(eventFilters);
		assertEquals(2, eventFilters.size());
		assertTrue(eventFilters.get(0) instanceof TestEventFilter);

		List<GatewayTransportFilter> transportFilters = TestUtils.readField("transportFilters", gatewaySenderFactoryBean);

		assertNotNull(transportFilters);
		assertEquals(2, transportFilters.size());
		assertTrue(transportFilters.get(0) instanceof TestTransportFilter);
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void testInnerGatewaySender() throws Exception {
		Region<?, ?> region = applicationContext.getBean("region-inner-gateway-sender", Region.class);

		assertNotNull(region.getAttributes().getGatewaySenderIds());
		assertEquals(2, region.getAttributes().getGatewaySenderIds().size());

		RegionFactoryBean regionFactoryBean = applicationContext.getBean("&region-inner-gateway-sender", RegionFactoryBean.class);

		Object[] gatewaySenders = TestUtils.readField("gatewaySenders", regionFactoryBean);

		assertNotNull(gatewaySenders);
		assertEquals(2, gatewaySenders.length);

		GatewaySender gatewaySender = (GatewaySender) gatewaySenders[0];

		assertNotNull(gatewaySender);
		assertEquals(1, gatewaySender.getRemoteDSId());
		assertEquals(false, gatewaySender.isManualStart());
		assertEquals(true, gatewaySender.isRunning());
		assertEquals(10, gatewaySender.getAlertThreshold());
		assertEquals(11, gatewaySender.getBatchSize());
		assertEquals(3000, gatewaySender.getBatchTimeInterval());
		assertEquals(2, gatewaySender.getDispatcherThreads());
		assertEquals("diskstore", gatewaySender.getDiskStoreName());
		assertEquals(true, gatewaySender.isDiskSynchronous());
		assertTrue(gatewaySender.isBatchConflationEnabled());
		assertEquals(50, gatewaySender.getMaximumQueueMemory());
		assertEquals(OrderPolicy.THREAD, gatewaySender.getOrderPolicy());
		assertTrue(gatewaySender.isPersistenceEnabled());
		assertFalse(gatewaySender.isParallel());
		assertEquals(16536, gatewaySender.getSocketBufferSize());
		assertEquals(3000, gatewaySender.getSocketReadTimeout());

		List<GatewayEventFilter> eventFilters = gatewaySender.getGatewayEventFilters();

		assertNotNull(eventFilters);
		assertEquals(1, eventFilters.size());
		assertTrue(eventFilters.get(0) instanceof TestEventFilter);

		List<GatewayTransportFilter> transportFilters = gatewaySender.getGatewayTransportFilters();

		assertNotNull(transportFilters);
		assertEquals(1, transportFilters.size());
		assertTrue(transportFilters.get(0) instanceof TestTransportFilter);
	}

	@Test
	public void testGatewaySenderWithEventTransportFilterRefs() throws Exception {
		GatewaySenderFactoryBean gatewaySenderFactoryBean = applicationContext.getBean("&gateway-sender-with-event-transport-filter-refs",
			GatewaySenderFactoryBean.class);

		assertNotNull(gatewaySenderFactoryBean);
		assertNotNull(TestUtils.readField("cache", gatewaySenderFactoryBean));
		assertThat((Integer) TestUtils.readField("remoteDistributedSystemId", gatewaySenderFactoryBean)).isEqualTo(3);
		assertTrue(Boolean.TRUE.equals(TestUtils.readField("batchConflationEnabled", gatewaySenderFactoryBean)));
		assertThat((Object) TestUtils.readField("batchSize", gatewaySenderFactoryBean)).isEqualTo(50);
		assertThat((Object) TestUtils.readField("dispatcherThreads", gatewaySenderFactoryBean)).isEqualTo(10);
		assertEquals(true, TestUtils.readField("manualStart", gatewaySenderFactoryBean));

		List<GatewayEventFilter> eventFilters = TestUtils.readField("eventFilters", gatewaySenderFactoryBean);

		assertNotNull(eventFilters);
		assertEquals(1, eventFilters.size());
		assertTrue(eventFilters.get(0) instanceof TestEventFilter);
		assertSame(applicationContext.getBean("event-filter"), eventFilters.get(0));

		List<GatewayTransportFilter> transportFilters = TestUtils.readField("transportFilters", gatewaySenderFactoryBean);

		assertNotNull(transportFilters);
		assertEquals(1, transportFilters.size());
		assertTrue(transportFilters.get(0) instanceof TestTransportFilter);
		assertSame(applicationContext.getBean("transport-filter"), transportFilters.get(0));
	}

	@Test
	public void testGatewayReceiver() {
		GatewayReceiver gatewayReceiver = applicationContext.getBean("gateway-receiver", GatewayReceiver.class);

		assertNotNull(gatewayReceiver);
		assertEquals("192.168.0.1", gatewayReceiver.getBindAddress());
		assertEquals(12345, gatewayReceiver.getStartPort());
		assertEquals(23456, gatewayReceiver.getEndPort());
		assertEquals(3000, gatewayReceiver.getMaximumTimeBetweenPings());
		assertEquals(16536, gatewayReceiver.getSocketBufferSize());
	}

	@SuppressWarnings("rawtypes")
	public static class TestEventFilter implements GatewayEventFilter {

		@Override
		public void close() {
		}

		@Override
		public void afterAcknowledgement(GatewayQueueEvent arg0) {
		}

		@Override
		public boolean beforeEnqueue(GatewayQueueEvent arg0) {
			return false;
		}

		@Override
		public boolean beforeTransmit(GatewayQueueEvent arg0) {
			return false;
		}
	}

	public static class TestTransportFilter implements GatewayTransportFilter {

		@Override
		public void close() {
		}

		@Override
		public InputStream getInputStream(InputStream arg0) {
			return null;
		}

		@Override
		public OutputStream getOutputStream(OutputStream arg0) {
			return null;
		}
	}

	@SuppressWarnings("rawtypes")
	public static class TestAsyncEventListener implements AsyncEventListener {

		@Override
		public void close() {
		}

		@Override
		public boolean processEvents(List<AsyncEvent> arg0) {
			return false;
		}
	}
}
