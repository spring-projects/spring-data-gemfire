/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.config.xml;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;

import org.junit.AfterClass;
import org.junit.Test;

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

import org.springframework.data.gemfire.PeerRegionFactoryBean;
import org.springframework.data.gemfire.RecreatingContextTest;
import org.springframework.data.gemfire.TestUtils;
import org.springframework.data.gemfire.test.GemfireTestBeanPostProcessor;
import org.springframework.data.gemfire.wan.GatewaySenderFactoryBean;

/**
 * Integration Tests testing and asserting GemFire 7.0 WAN functionality and configuration.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.asyncqueue.AsyncEventQueue
 * @see org.apache.geode.cache.wan.GatewayReceiver
 * @see org.apache.geode.cache.wan.GatewaySender
 * @see org.springframework.data.gemfire.RecreatingContextTest
 */
@SuppressWarnings("unused")
public class GemfireV7GatewayNamespaceTest extends RecreatingContextTest {

	@AfterClass
	public static void tearDown() {

		for (String name : new File(".").list((file, filename) -> filename.startsWith("BACKUP"))) {
			new File(name).delete();
		}
	}

	@Override
	protected void configureContext() {
		this.applicationContext.getBeanFactory().addBeanPostProcessor(new GemfireTestBeanPostProcessor());
	}

	@Override
	protected String location() {
		return "/org/springframework/data/gemfire/config/xml/gateway-v7-ns.xml";
	}

	@Test
	public void asyncEventQueueConfigurationIsCorrect() {

		AsyncEventQueue asyncEventQueue =
			this.applicationContext.getBean("async-event-queue", AsyncEventQueue.class);

		assertThat(asyncEventQueue).isNotNull();
		assertThat(asyncEventQueue.isBatchConflationEnabled()).isTrue();
		assertThat(asyncEventQueue.getBatchSize()).isEqualTo(10);
		assertThat(asyncEventQueue.getBatchTimeInterval()).isEqualTo(3);
		assertThat(asyncEventQueue.getDiskStoreName()).isEqualTo("diskstore");
		assertThat(asyncEventQueue.isDiskSynchronous()).isTrue();
		assertThat(asyncEventQueue.getMaximumQueueMemory()).isEqualTo(50);
		assertThat(asyncEventQueue.getOrderPolicy()).isEqualTo(OrderPolicy.KEY);
		assertThat(asyncEventQueue.isParallel()).isFalse();
		assertThat(asyncEventQueue.isPersistent()).isTrue();
	}

	@Test
	public void gatewaySenderFactoryBeanConfigurationIsCorrect() throws Exception {

		GatewaySenderFactoryBean gatewaySenderFactoryBean =
			this.applicationContext.getBean("&gateway-sender", GatewaySenderFactoryBean.class);

		assertThat(gatewaySenderFactoryBean).isNotNull();
		assertThat(gatewaySenderFactoryBean.getCache()).isNotNull();
		assertThat(gatewaySenderFactoryBean.getRemoteDistributedSystemId()).isEqualTo(2);
		assertThat(gatewaySenderFactoryBean.getAlertThreshold()).isEqualTo(10);
		assertThat(gatewaySenderFactoryBean.getBatchConflationEnabled()).isTrue();
		assertThat(gatewaySenderFactoryBean.getBatchSize()).isEqualTo(11);
		assertThat(gatewaySenderFactoryBean.getDispatcherThreads()).isEqualTo(12);
		assertThat(gatewaySenderFactoryBean.getDiskSynchronous()).isFalse();
		assertThat(gatewaySenderFactoryBean.isManualStart()).isTrue();

		List<GatewayEventFilter> eventFilters = TestUtils.readField("eventFilters", gatewaySenderFactoryBean);

		assertThat(eventFilters).isNotNull();
		assertThat(eventFilters.size()).isEqualTo(2);
		assertThat(eventFilters.get(0)).isInstanceOf(TestEventFilter.class);

		List<GatewayTransportFilter> transportFilters = TestUtils
			.readField("transportFilters", gatewaySenderFactoryBean);

		assertThat(transportFilters).isNotNull();
		assertThat(transportFilters.size()).isEqualTo(2);
		assertThat(transportFilters.get(0)).isInstanceOf(TestTransportFilter.class);
	}

	@Test
	@SuppressWarnings("rawtypes")
	public void nestedGatewaySenderConfigurationIsCorrect() throws Exception {

		Region<?, ?> region = this.applicationContext.getBean("region-with-nested-gateway-sender", Region.class);

		assertThat(region).isNotNull();
		assertThat(region.getAttributes()).isNotNull();
		assertThat(region.getAttributes().getGatewaySenderIds()).isNotNull();
		assertThat(region.getAttributes().getGatewaySenderIds()).hasSize(2);

		PeerRegionFactoryBean regionFactoryBean = applicationContext.getBean("&region-with-nested-gateway-sender", PeerRegionFactoryBean.class);

		List<GatewaySender> gatewaySenders = TestUtils.readField("gatewaySenders", regionFactoryBean);

		assertThat(gatewaySenders).isNotNull();
		assertThat(gatewaySenders).hasSize(2);

		GatewaySender gatewaySender = gatewaySenders.get(0);

		assertThat(gatewaySender).isNotNull();
		assertThat(gatewaySender.getRemoteDSId()).isEqualTo(1);
		assertThat(gatewaySender.isManualStart()).isFalse();
		assertThat(gatewaySender.isRunning()).isTrue();
		assertThat(gatewaySender.getAlertThreshold()).isEqualTo(10);
		assertThat(gatewaySender.getBatchSize()).isEqualTo(11);
		assertThat(gatewaySender.getBatchTimeInterval()).isEqualTo(3000);
		assertThat(gatewaySender.getDispatcherThreads()).isEqualTo(2);
		assertThat(gatewaySender.getDiskStoreName()).isEqualTo("diskstore");
		assertThat(gatewaySender.isDiskSynchronous()).isEqualTo(true);
		assertThat(gatewaySender.isBatchConflationEnabled()).isTrue();
		assertThat(gatewaySender.getMaximumQueueMemory()).isEqualTo(50);
		assertThat(gatewaySender.getOrderPolicy()).isEqualTo(OrderPolicy.THREAD);
		assertThat(gatewaySender.isPersistenceEnabled()).isTrue();
		assertThat(gatewaySender.isParallel()).isFalse();
		assertThat(gatewaySender.getSocketBufferSize()).isEqualTo(16536);
		assertThat(gatewaySender.getSocketReadTimeout()).isEqualTo(3000);

		List<GatewayEventFilter> eventFilters = gatewaySender.getGatewayEventFilters();

		assertThat(eventFilters).isNotNull();
		assertThat(eventFilters).hasSize(1);
		assertThat(eventFilters.get(0)).isInstanceOf(TestEventFilter.class);

		List<GatewayTransportFilter> transportFilters = gatewaySender.getGatewayTransportFilters();

		assertThat(transportFilters).isNotNull();
		assertThat(transportFilters).hasSize(1);
		assertThat(transportFilters.get(0)).isInstanceOf(TestTransportFilter.class);
	}

	@Test
	public void gatewaySenderWithEventTransportFilterRefsConfigurationIsCorrect() throws Exception {

		GatewaySenderFactoryBean gatewaySenderFactoryBean =
			this.applicationContext.getBean("&gateway-sender-with-event-transport-filter-refs",
				GatewaySenderFactoryBean.class);

		assertThat(gatewaySenderFactoryBean).isNotNull();
		assertThat(gatewaySenderFactoryBean.getCache()).isNotNull();
		assertThat(gatewaySenderFactoryBean.getRemoteDistributedSystemId()).isEqualTo(3);
		assertThat(gatewaySenderFactoryBean.getBatchConflationEnabled()).isTrue();
		assertThat(gatewaySenderFactoryBean.getBatchSize()).isEqualTo(50);
		assertThat(gatewaySenderFactoryBean.getDispatcherThreads()).isEqualTo(10);
		assertThat(gatewaySenderFactoryBean.isManualStart()).isTrue();

		List<GatewayEventFilter> eventFilters = TestUtils.readField("eventFilters", gatewaySenderFactoryBean);

		assertThat(eventFilters).isNotNull();
		assertThat(eventFilters).hasSize(1);
		assertThat(eventFilters.get(0)).isInstanceOf(TestEventFilter.class);
		assertThat(eventFilters.get(0)).isSameAs(applicationContext.getBean("event-filter"));

		List<GatewayTransportFilter> transportFilters =
			TestUtils.readField("transportFilters", gatewaySenderFactoryBean);

		assertThat(transportFilters).isNotNull();
		assertThat(transportFilters).hasSize(1);
		assertThat(transportFilters.get(0)).isInstanceOf(TestTransportFilter.class);
		assertThat(transportFilters.get(0)).isSameAs(applicationContext.getBean("transport-filter"));
	}

	@Test
	public void gatewayReceiverConfigurationIsCorrect() {

		GatewayReceiver gatewayReceiver =
			this.applicationContext.getBean("gateway-receiver", GatewayReceiver.class);

		assertThat(gatewayReceiver).isNotNull();
		assertThat(gatewayReceiver.getBindAddress()).isEqualTo("192.168.0.1");
		assertThat(gatewayReceiver.getStartPort()).isEqualTo(12345);
		assertThat(gatewayReceiver.getEndPort()).isEqualTo(23456);
		assertThat(gatewayReceiver.getMaximumTimeBetweenPings()).isEqualTo(3000);
		assertThat(gatewayReceiver.getSocketBufferSize()).isEqualTo(16536);
	}

	public static class TestAsyncEventListener implements AsyncEventListener {

		@Override
		public void close() { }

		@Override
		public boolean processEvents(List<AsyncEvent> events) {
			return false;
		}
	}

	public static class TestEventFilter implements GatewayEventFilter {

		@Override
		public void close() {
		}

		@Override
		public void afterAcknowledgement(GatewayQueueEvent event) { }

		@Override
		public boolean beforeEnqueue(GatewayQueueEvent event) {
			return false;
		}

		@Override
		public boolean beforeTransmit(GatewayQueueEvent event) {
			return false;
		}
	}

	public static class TestTransportFilter implements GatewayTransportFilter {

		@Override
		public void close() {
		}

		@Override
		public InputStream getInputStream(InputStream in) {
			return null;
		}

		@Override
		public OutputStream getOutputStream(OutputStream out) {
			return null;
		}
	}
}
