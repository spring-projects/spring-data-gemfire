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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.File;
import java.io.FilenameFilter;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.List;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.data.gemfire.RecreatingContextTest;
import org.springframework.data.gemfire.RegionFactoryBean;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.wan.AsyncEventQueueFactoryBean;
import org.springframework.data.gemfire.wan.GatewaySenderFactoryBean;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEvent;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventListener;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueue;
import com.gemstone.gemfire.cache.asyncqueue.AsyncEventQueueFactory;
import com.gemstone.gemfire.cache.util.Gateway.OrderPolicy;
import com.gemstone.gemfire.cache.wan.GatewayEventFilter;
import com.gemstone.gemfire.cache.wan.GatewayQueueEvent;
import com.gemstone.gemfire.cache.wan.GatewayReceiver;
import com.gemstone.gemfire.cache.wan.GatewaySender;
import com.gemstone.gemfire.cache.wan.GatewaySenderFactory;
import com.gemstone.gemfire.cache.wan.GatewayTransportFilter;

/**
 * This test is only valid for GF 7.0 and above
 * 
 * @author David Turanski
 * 
 */
public class GemfireV7GatewayNamespaceTest extends RecreatingContextTest implements BeanPostProcessor {
	 
	@Override
	protected String location() {
		return "/org/springframework/data/gemfire/config/gateway-v7-ns.xml";
	}
	
	@Override
	protected void configureContext() {
		ctx.getBeanFactory().addBeanPostProcessor(this);
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

	@AfterClass
	public static void tearDown() {
		for (String name : new File(".").list(new FilenameFilter() {

			@Override
			public boolean accept(File dir, String name) {
				return name.startsWith("BACKUP");
			}
		})) {
			new File(name).delete();
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

	@SuppressWarnings("rawtypes")
	private void testInnerGatewaySender() throws Exception {
		Region<?, ?> region = ctx.getBean("region-inner-gateway-sender", Region.class);
		GatewaySender gws = ctx.getBean("gateway-sender", GatewaySender.class);
		assertNotNull(region.getAttributes().getGatewaySenderIds());
		assertEquals(2, region.getAttributes().getGatewaySenderIds().size());

//		// Isolate the inner gateway
//		Set<String> gatewaySenders = region.getAttributes().getGatewaySenderIds();
//		assertTrue(gatewaySenders.remove(gws));
//		gatewaySenders.remove(gws);

		RegionFactoryBean rfb = ctx.getBean("&region-inner-gateway-sender", RegionFactoryBean.class);
		Object[] gwsenders = TestUtils.readField("gatewaySenders", rfb);
		gws = (GatewaySender)gwsenders[0];
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
		public void afterAcknowledgement(GatewayQueueEvent arg0) {
			// TODO Auto-generated method stub
			
		}

		@Override
		public boolean beforeEnqueue(GatewayQueueEvent arg0) {
			// TODO Auto-generated method stub
			return false;
		}

		@Override
		public boolean beforeTransmit(GatewayQueueEvent arg0) {
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
	
	
	public static class StubAsyncEventQueueFactory  implements AsyncEventQueueFactory {
		
		private AsyncEventListener listener;
	
		private AsyncEventQueue asyncEventQueue = mock(AsyncEventQueue.class);
		
		private boolean persistent;
		private int maxQueueMemory;
		private String diskStoreName;
		private int batchSize;

		private String name;
		
		@Override
		public AsyncEventQueue create(String name, AsyncEventListener listener) {
			this.name = name;
			this.listener = listener;
			
			when(asyncEventQueue.getAsyncEventListener()).thenReturn(this.listener);
			when(asyncEventQueue.getBatchSize()).thenReturn(this.batchSize);
			when(asyncEventQueue.getDiskStoreName()).thenReturn(this.diskStoreName);
			when(asyncEventQueue.isPersistent()).thenReturn(this.persistent);
			when(asyncEventQueue.getId()).thenReturn(this.name);
			when(asyncEventQueue.getMaximumQueueMemory()).thenReturn(this.maxQueueMemory);
			return this.asyncEventQueue;
		}

		@Override
		public AsyncEventQueueFactory setBatchSize(int batchSize) {
			this.batchSize = batchSize;
			return this;
		}

		@Override
		public AsyncEventQueueFactory setDiskStoreName(String diskStoreName) {
			this.diskStoreName = diskStoreName;
			return this;
		}

		@Override
		public AsyncEventQueueFactory setMaximumQueueMemory(int maxQueueMemory) {
			this.maxQueueMemory = maxQueueMemory;
			return this;
		}

		@Override
		public AsyncEventQueueFactory setPersistent(boolean persistent) {
			this.persistent = persistent;
			return this;
		}
	}
	
	public static class StubGWSenderFactory implements GatewaySenderFactory {
		
		private GatewaySender gatewaySender = mock(GatewaySender.class);	
		
		private int alertThreshold;
		private boolean batchConflationEnabled;
		private int batchSize;
		private int batchTimeInterval;
		private String diskStoreName;
		private boolean diskSynchronous;
		private int dispatcherThreads;
		private boolean manualStart;
		private int maxQueueMemory;
		private OrderPolicy orderPolicy;
		private boolean parallel;
		private boolean persistenceEnabled;
		private int socketBufferSize;
		private int socketReadTimeout;
		private List<GatewayEventFilter> eventFilters;
		private List<GatewayTransportFilter> transportFilters;

		private String name;

		private int remoteSystemId;
		
		public StubGWSenderFactory() {
			this.eventFilters = new ArrayList<GatewayEventFilter>();
			this.transportFilters = new ArrayList<GatewayTransportFilter>();
	
		}
		
		@Override
		public GatewaySenderFactory addGatewayEventFilter(GatewayEventFilter filter) {
			eventFilters.add(filter);
			return this;
		}

		@Override
		public GatewaySenderFactory addGatewayTransportFilter(GatewayTransportFilter filter) {
			transportFilters.add(filter);
			return this;
		}

		@Override
		public GatewaySender create(String name, int remoteSystemId) {
			this.name = name;
			this.remoteSystemId = remoteSystemId;
			when(gatewaySender.getId()).thenReturn(this.name);
			when(gatewaySender.getRemoteDSId()).thenReturn(this.remoteSystemId);
			when(gatewaySender.getAlertThreshold()).thenReturn(this.alertThreshold);
			when(gatewaySender.getBatchSize()).thenReturn(this.batchSize);
			when(gatewaySender.getBatchTimeInterval()).thenReturn(this.batchTimeInterval);
			when(gatewaySender.getDiskStoreName()).thenReturn(this.diskStoreName);
			when(gatewaySender.getDispatcherThreads()).thenReturn(this.dispatcherThreads);
			when(gatewaySender.getGatewayEventFilters()).thenReturn(this.eventFilters);
			when(gatewaySender.getGatewayTransportFilters()).thenReturn(this.transportFilters);
			when(gatewaySender.getMaximumQueueMemory()).thenReturn(this.maxQueueMemory);
			when(gatewaySender.getOrderPolicy()).thenReturn(this.orderPolicy);
			when(gatewaySender.getSocketBufferSize()).thenReturn(this.socketBufferSize);
			when(gatewaySender.getSocketReadTimeout()).thenReturn(this.socketReadTimeout);
			when(gatewaySender.isManualStart()).thenReturn(this.manualStart);
			when(gatewaySender.isBatchConflationEnabled()).thenReturn(this.batchConflationEnabled);
			when(gatewaySender.isDiskSynchronous()).thenReturn(this.diskSynchronous);
			when(gatewaySender.isParallel()).thenReturn(this.parallel);
			when(gatewaySender.isPersistenceEnabled()).thenReturn(this.persistenceEnabled);
			return gatewaySender;
		}

		@Override
		public GatewaySenderFactory removeGatewayEventFilter(GatewayEventFilter filter) {
			gatewaySender.removeGatewayEventFilter(filter);
			return this;
		}

		@Override
		public GatewaySenderFactory removeGatewayTransportFilter(GatewayTransportFilter filter) {
			gatewaySender.removeGatewayTransportFilter(filter);
			return this;
		}

		@Override
		public GatewaySenderFactory setAlertThreshold(int alertThreshold) {
			this.alertThreshold = alertThreshold;
			return this;
		}

		@Override
		public GatewaySenderFactory setBatchConflationEnabled(boolean batchConflationEnabled) {
			this.batchConflationEnabled = batchConflationEnabled;
			return this;
		}

		@Override
		public GatewaySenderFactory setBatchSize(int batchSize) {
			this.batchSize = batchSize;
			return this;
		}

		@Override
		public GatewaySenderFactory setBatchTimeInterval(int batchTimeInterval) {
			this.batchTimeInterval = batchTimeInterval;
			return this;
		}

		@Override
		public GatewaySenderFactory setDiskStoreName(String diskStoreName) {
			this.diskStoreName = diskStoreName;
			return this;
		}

		@Override
		public GatewaySenderFactory setDiskSynchronous(boolean diskSynchronous) {
			this.diskSynchronous = diskSynchronous;
			return this;
		}

		@Override
		public GatewaySenderFactory setDispatcherThreads(int dispatcherThreads) {
			this.dispatcherThreads = dispatcherThreads;
			return this;
		}

		@Override
		public GatewaySenderFactory setManualStart(boolean manualStart) {
			this.manualStart = manualStart;
			return this;
		}

		@Override
		public GatewaySenderFactory setMaximumQueueMemory(int maxQueueMemory) {
			this.maxQueueMemory = maxQueueMemory;
			return this;
		}

		@Override
		public GatewaySenderFactory setOrderPolicy(OrderPolicy orderPolicy) {
			this.orderPolicy = orderPolicy;
			return this;
		}

		@Override
		public GatewaySenderFactory setParallel(boolean parallel) {
			this.parallel = parallel;
			return this;
		}

		@Override
		public GatewaySenderFactory setPersistenceEnabled(boolean persistenceEnabled) {
			this.persistenceEnabled = persistenceEnabled;
			return this;
		}

		@Override
		public GatewaySenderFactory setSocketBufferSize(int socketBufferSize) {
			this.socketBufferSize = socketBufferSize;
			return this;
		}

		@Override
		public GatewaySenderFactory setSocketReadTimeout(int socketReadTimeout) {
			this.socketReadTimeout = socketReadTimeout;
			return this;
		}
	}


	/*
	 * This mocks out the WAN components which are disabled in the developer edition
	 */
	@Override
	public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
		if (bean instanceof GatewaySenderFactoryBean) {
			((GatewaySenderFactoryBean)bean).setFactory(new StubGWSenderFactory());
		}
		if (bean instanceof AsyncEventQueueFactoryBean) {
			((AsyncEventQueueFactoryBean)bean).setFactory(new StubAsyncEventQueueFactory());
		}
		return bean;
	}

	@Override
	public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {	
		return bean;
	}
}
