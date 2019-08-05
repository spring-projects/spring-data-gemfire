/*
 * Copyright 2010-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.TreeMap;
import java.util.stream.Collectors;

import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.EntryEvent;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.wan.GatewayEventFilter;
import org.apache.geode.cache.wan.GatewayEventSubstitutionFilter;
import org.apache.geode.cache.wan.GatewayQueueEvent;
import org.apache.geode.cache.wan.GatewaySender;
import org.apache.geode.cache.wan.GatewayTransportFilter;
import org.apache.geode.distributed.internal.DistributionAdvisor;
import org.apache.geode.internal.cache.EntryEventImpl;
import org.apache.geode.internal.cache.wan.AbstractGatewaySender;

import org.junit.After;
import org.junit.Test;

import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertySource;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.data.gemfire.wan.GatewaySenderFactoryBean;
import org.springframework.data.gemfire.wan.OrderPolicyType;
import org.springframework.mock.env.MockPropertySource;

/**
 * Tests for {@link EnableGatewaySenders} and {@link EnableGatewaySender} to test the configuration of {@link GatewaySender} using properties.
 *
 * @author Udo Kohlmeyer
 * @see Test
 * @see Configuration
 * @see org.mockito.Mockito
 * @see GatewaySenderConfigurer
 * @see GatewaySenderConfiguration
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @see org.springframework.data.gemfire.wan.GatewayReceiverFactoryBean
 * @since 2.2.0
 */
public class GatewaySenderPropertiesTests {

	private ConfigurableApplicationContext applicationContext;

	@After
	public void shutdown() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
	}

	@Test
	public void gatewayReceiverPropertiesConfigurationOnMultipleChildren() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.manual-start", true)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.remote-distributed-system-id", 2)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.disk-synchronous", true)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.batch-conflation-enabled", true)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.parallel", true)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.persistent", false)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.order-policy", "PARTITION")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.event-substitution-filter",
				"SomeEventSubstitutionFilter")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.alert-threshold", 1234)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.batch-size", 100)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.batch-time-interval", 2000)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.dispatcher-threads", 22)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.maximum-queue-memory", 400)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.socket-buffer-size", 16384)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.socket-read-timeout", 4000)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.disk-store-reference", "someDiskStore")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.event-filters", "SomeEventFilter")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.transport-filters",
				"transportBean2, transportBean1")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.regions", "Region1,Region2")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.manual-start", false)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.remote-distributed-system-id", 3)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.disk-synchronous", false)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.batch-conflation-enabled", false)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.parallel", false)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.persistent", true)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.order-policy", "KEY")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.event-substitution-filter",
				"SomeEventSubstitutionFilter")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.alert-threshold", 4321)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.batch-size", 1000)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.batch-time-interval", 20000)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.dispatcher-threads", 2200)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.maximum-queue-memory", 40000)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.socket-buffer-size", 1638400)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.socket-read-timeout", 400000)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.disk-store-reference", "someDiskStore")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.event-filters", "SomeEventFilter")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.transport-filters",
				"transportBean1")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.regions", "Region1");

		this.applicationContext = newApplicationContext(testPropertySource, BaseGatewaySenderTestConfiguration.class,
			TestConfigurationWithPropertiesMultipleGatewaySenders.class);

		TestGatewaySenderConfigurer gatewaySenderConfigurer =
			this.applicationContext.getBean(TestGatewaySenderConfigurer.class);

		GatewaySender gatewaySender = this.applicationContext.getBean("TestGatewaySender", GatewaySender.class);

		assertThat(gatewaySender.isManualStart()).isEqualTo(true);
		assertThat(gatewaySender.getRemoteDSId()).isEqualTo(2);
		assertThat(gatewaySender.getId()).isEqualTo("TestGatewaySender");
		assertThat(gatewaySender.getDispatcherThreads()).isEqualTo(22);
		assertThat(gatewaySender.isBatchConflationEnabled()).isEqualTo(true);
		assertThat(gatewaySender.isParallel()).isEqualTo(true);
		assertThat(gatewaySender.isPersistenceEnabled()).isEqualTo(false);
		assertThat(gatewaySender.getDiskStoreName()).isEqualTo("someDiskStore");
		assertThat(gatewaySender.getOrderPolicy()).isEqualTo(GatewaySender.OrderPolicy.PARTITION);
		assertThat(((TestGatewayEventSubstitutionFilter) gatewaySender.getGatewayEventSubstitutionFilter()).name)
			.isEqualTo("SomeEventSubstitutionFilter");
		assertThat(gatewaySender.getAlertThreshold()).isEqualTo(1234);
		assertThat(gatewaySender.getBatchSize()).isEqualTo(100);
		assertThat(gatewaySender.getBatchTimeInterval()).isEqualTo(2000);
		assertThat(gatewaySender.getMaximumQueueMemory()).isEqualTo(400);
		assertThat(gatewaySender.getSocketReadTimeout()).isEqualTo(4000);
		assertThat(gatewaySender.getSocketBufferSize()).isEqualTo(16384);

		assertThat(gatewaySender.getGatewayTransportFilters().size()).isEqualTo(2);
		assertThat(gatewaySenderConfigurer.beanNames.get(gatewaySender.getId()).toArray())
			.isEqualTo(new String[] { "transportBean2", "transportBean1" });

		gatewaySender = this.applicationContext.getBean("TestGatewaySender2", GatewaySender.class);

		assertThat(gatewaySender.isManualStart()).isEqualTo(false);
		assertThat(gatewaySender.getRemoteDSId()).isEqualTo(3);
		assertThat(gatewaySender.getId()).isEqualTo("TestGatewaySender2");
		assertThat(gatewaySender.getDispatcherThreads()).isEqualTo(2200);
		assertThat(gatewaySender.isBatchConflationEnabled()).isEqualTo(false);
		assertThat(gatewaySender.isParallel()).isEqualTo(false);
		assertThat(gatewaySender.isPersistenceEnabled()).isEqualTo(true);
		assertThat(gatewaySender.getDiskStoreName()).isEqualTo("someDiskStore");
		assertThat(gatewaySender.getOrderPolicy()).isEqualTo(GatewaySender.OrderPolicy.KEY);
		assertThat(((TestGatewayEventSubstitutionFilter) gatewaySender.getGatewayEventSubstitutionFilter()).name)
			.isEqualTo("SomeEventSubstitutionFilter");
		assertThat(gatewaySender.getAlertThreshold()).isEqualTo(4321);
		assertThat(gatewaySender.getBatchSize()).isEqualTo(1000);
		assertThat(gatewaySender.getBatchTimeInterval()).isEqualTo(20000);
		assertThat(gatewaySender.getMaximumQueueMemory()).isEqualTo(40000);
		assertThat(gatewaySender.getSocketReadTimeout()).isEqualTo(400000);
		assertThat(gatewaySender.getSocketBufferSize()).isEqualTo(1638400);

		assertThat(gatewaySender.getGatewayTransportFilters().size()).isEqualTo(1);
		assertThat(gatewaySenderConfigurer.beanNames.get(gatewaySender.getId()).toArray())
			.isEqualTo(new String[] { "transportBean1" });

		Region<?, ?> region1 = this.applicationContext.getBean("Region1", Region.class);
		Region<?, ?> region2 = this.applicationContext.getBean("Region2", Region.class);

		assertThat(region1.getAttributes().getGatewaySenderIds())
			.containsExactlyInAnyOrder("TestGatewaySender", "TestGatewaySender2");
		assertThat(region2.getAttributes().getGatewaySenderIds())
			.containsExactlyInAnyOrder("TestGatewaySender");

	}

	@Test
	public void gatewayReceiverPropertiesConfigurationOnMultipleChildrenAndAnnotations() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.gateway.sender.socket-read-timeout", 4000)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.manual-start", true)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.remote-distributed-system-id", 2)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.disk-synchronous", true)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.batch-conflation-enabled", true)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.parallel", true)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.persistent", false)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.order-policy", "THREAD")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.event-substitution-filter",
				"SomeEventSubstitutionFilter")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.alert-threshold", 1234)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.batch-size", 1020)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.batch-time-interval", 2300)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.dispatcher-threads", 22)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.maximum-queue-memory", 400)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.socket-buffer-size", 16384)

			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.disk-store-reference", "someDiskStore")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.event-filters", "SomeEventFilter")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.transport-filters",
				"transportBean2, transportBean1")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.regions", "Region1,Region2")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.manual-start", false)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.remote-distributed-system-id", 3)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.disk-synchronous", false)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.batch-conflation-enabled", false)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.parallel", false)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.persistent", true)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.order-policy", "KEY")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.event-substitution-filter",
				"SomeEventSubstitutionFilter")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.alert-threshold", 4321)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.batch-size", 1000)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.batch-time-interval", 20000)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.dispatcher-threads", 2200)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.maximum-queue-memory", 40000)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.socket-buffer-size", 1638400)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.disk-store-reference", "someDiskStore")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.event-filters", "SomeEventFilter")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.transport-filters",
				"transportBean1")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender2.regions", "");

		this.applicationContext = newApplicationContext(testPropertySource, BaseGatewaySenderTestConfiguration.class,
			TestConfigurationWithMultipleGatewaySenderAnnotations.class);

		TestGatewaySenderConfigurer gatewaySenderConfigurer =
			this.applicationContext.getBean(TestGatewaySenderConfigurer.class);

		GatewaySender gatewaySender = this.applicationContext.getBean("TestGatewaySender", GatewaySender.class);

		assertThat(gatewaySender.isManualStart()).isEqualTo(true);
		assertThat(gatewaySender.getRemoteDSId()).isEqualTo(2);
		assertThat(gatewaySender.getId()).isEqualTo("TestGatewaySender");
		assertThat(gatewaySender.getDispatcherThreads()).isEqualTo(22);
		assertThat(gatewaySender.isBatchConflationEnabled()).isEqualTo(true);
		assertThat(gatewaySender.isParallel()).isEqualTo(true);
		assertThat(gatewaySender.isPersistenceEnabled()).isEqualTo(false);
		assertThat(gatewaySender.getDiskStoreName()).isEqualTo("someDiskStore");
		assertThat(gatewaySender.getOrderPolicy()).isEqualTo(GatewaySender.OrderPolicy.THREAD);
		assertThat(((TestGatewayEventSubstitutionFilter) gatewaySender.getGatewayEventSubstitutionFilter()).name)
			.isEqualTo("SomeEventSubstitutionFilter");
		assertThat(gatewaySender.getAlertThreshold()).isEqualTo(1234);
		assertThat(gatewaySender.getBatchSize()).isEqualTo(1020);
		assertThat(gatewaySender.getBatchTimeInterval()).isEqualTo(2300);
		assertThat(gatewaySender.getMaximumQueueMemory()).isEqualTo(400);
		assertThat(gatewaySender.getSocketReadTimeout()).isEqualTo(4000);
		assertThat(gatewaySender.getSocketBufferSize()).isEqualTo(16384);

		assertThat(gatewaySender.getGatewayTransportFilters().size()).isEqualTo(2);
		assertThat(gatewaySenderConfigurer.beanNames.get(gatewaySender.getId()).toArray())
			.isEqualTo(new String[] { "transportBean2", "transportBean1" });

		gatewaySender = this.applicationContext.getBean("TestGatewaySender2", GatewaySender.class);

		assertThat(gatewaySender.isManualStart()).isEqualTo(false);
		assertThat(gatewaySender.getRemoteDSId()).isEqualTo(3);
		assertThat(gatewaySender.getId()).isEqualTo("TestGatewaySender2");
		assertThat(gatewaySender.getDispatcherThreads()).isEqualTo(2200);
		assertThat(gatewaySender.isBatchConflationEnabled()).isEqualTo(false);
		assertThat(gatewaySender.isParallel()).isEqualTo(false);
		assertThat(gatewaySender.isPersistenceEnabled()).isEqualTo(true);
		assertThat(gatewaySender.getDiskStoreName()).isEqualTo("someDiskStore");
		assertThat(gatewaySender.getOrderPolicy()).isEqualTo(GatewaySender.OrderPolicy.KEY);
		assertThat(((TestGatewayEventSubstitutionFilter) gatewaySender.getGatewayEventSubstitutionFilter()).name)
			.isEqualTo("SomeEventSubstitutionFilter");
		assertThat(gatewaySender.getAlertThreshold()).isEqualTo(4321);
		assertThat(gatewaySender.getBatchSize()).isEqualTo(1000);
		assertThat(gatewaySender.getBatchTimeInterval()).isEqualTo(20000);
		assertThat(gatewaySender.getMaximumQueueMemory()).isEqualTo(40000);
		assertThat(gatewaySender.getSocketReadTimeout()).isEqualTo(4000);
		assertThat(gatewaySender.getSocketBufferSize()).isEqualTo(1638400);

		assertThat(gatewaySender.getGatewayTransportFilters().size()).isEqualTo(1);
		assertThat(gatewaySenderConfigurer.beanNames.get(gatewaySender.getId()).toArray())
			.isEqualTo(new String[] { "transportBean1" });

		Region<?, ?> region1 = this.applicationContext.getBean("Region1", Region.class);
		Region<?, ?> region2 = this.applicationContext.getBean("Region2", Region.class);

		assertThat(region1.getAttributes().getGatewaySenderIds())
			.containsExactlyInAnyOrder("TestGatewaySender", "TestGatewaySender2");
		assertThat(region2.getAttributes().getGatewaySenderIds())
			.containsExactlyInAnyOrder("TestGatewaySender", "TestGatewaySender2");

	}

	@Test
	public void gatewayReceiverPropertiesConfigurationOnChild() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.manual-start", true)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.remote-distributed-system-id", 2)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.disk-synchronous", true)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.batch-conflation-enabled", true)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.parallel", true)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.persistent", false)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.order-policy", "PARTITION")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.event-substitution-filter",
				"SomeEventSubstitutionFilter")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.alert-threshold", 1234)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.batch-size", 100)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.batch-time-interval", 2000)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.dispatcher-threads", 22)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.maximum-queue-memory", 400)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.socket-buffer-size", 16384)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.socket-read-timeout", 4000)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.disk-store-reference", "someDiskStore")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.event-filters", "SomeEventFilter")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.transport-filters",
				"transportBean2, transportBean1")
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.regions", "Region1,Region2");

		this.applicationContext = newApplicationContext(testPropertySource, BaseGatewaySenderTestConfiguration.class,
			TestConfigurationWithProperties.class);

		TestGatewaySenderConfigurer gatewaySenderConfigurer =
			this.applicationContext.getBean(TestGatewaySenderConfigurer.class);

		GatewaySender gatewaySender = this.applicationContext.getBean("TestGatewaySender", GatewaySender.class);

		assertThat(gatewaySender.isManualStart()).isEqualTo(true);
		assertThat(gatewaySender.getRemoteDSId()).isEqualTo(2);
		assertThat(gatewaySender.getId()).isEqualTo("TestGatewaySender");
		assertThat(gatewaySender.getDispatcherThreads()).isEqualTo(22);
		assertThat(gatewaySender.isBatchConflationEnabled()).isEqualTo(true);
		assertThat(gatewaySender.isParallel()).isEqualTo(true);
		assertThat(gatewaySender.isPersistenceEnabled()).isEqualTo(false);
		assertThat(gatewaySender.getDiskStoreName()).isEqualTo("someDiskStore");
		assertThat(gatewaySender.getOrderPolicy()).isEqualTo(GatewaySender.OrderPolicy.PARTITION);
		assertThat(((TestGatewayEventSubstitutionFilter) gatewaySender.getGatewayEventSubstitutionFilter()).name)
			.isEqualTo("SomeEventSubstitutionFilter");
		assertThat(gatewaySender.getAlertThreshold()).isEqualTo(1234);
		assertThat(gatewaySender.getBatchSize()).isEqualTo(100);
		assertThat(gatewaySender.getBatchTimeInterval()).isEqualTo(2000);
		assertThat(gatewaySender.getMaximumQueueMemory()).isEqualTo(400);
		assertThat(gatewaySender.getSocketReadTimeout()).isEqualTo(4000);
		assertThat(gatewaySender.getSocketBufferSize()).isEqualTo(16384);

		assertThat(gatewaySender.getGatewayTransportFilters().size()).isEqualTo(2);
		assertThat(gatewaySenderConfigurer.beanNames.get(gatewaySender.getId()).toArray())
			.isEqualTo(new String[] { "transportBean2", "transportBean1" });

		Region<?, ?> region1 = this.applicationContext.getBean("Region1", Region.class);
		Region<?, ?> region2 = this.applicationContext.getBean("Region2", Region.class);

		assertThat(region1.getAttributes().getGatewaySenderIds())
			.containsExactlyInAnyOrder("TestGatewaySender");
		assertThat(region2.getAttributes().getGatewaySenderIds())
			.containsExactlyInAnyOrder("TestGatewaySender");

	}

	@Test
	public void gatewayReceiverPropertiesConfigurationOnParent() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.gateway.sender.manual-start", true)
			.withProperty("spring.data.gemfire.gateway.sender.remote-distributed-system-id", 2)
			.withProperty("spring.data.gemfire.gateway.sender.disk-synchronous", true)
			.withProperty("spring.data.gemfire.gateway.sender.batch-conflation-enabled", true)
			.withProperty("spring.data.gemfire.gateway.sender.parallel", true)
			.withProperty("spring.data.gemfire.gateway.sender.persistent", false)
			.withProperty("spring.data.gemfire.gateway.sender.order-policy", "PARTITION")
			.withProperty("spring.data.gemfire.gateway.sender.event-substitution-filter",
				"SomeEventSubstitutionFilter")
			.withProperty("spring.data.gemfire.gateway.sender.alert-threshold", 1234)
			.withProperty("spring.data.gemfire.gateway.sender.batch-size", 100)
			.withProperty("spring.data.gemfire.gateway.sender.batch-time-interval", 2000)
			.withProperty("spring.data.gemfire.gateway.sender.dispatcher-threads", 22)
			.withProperty("spring.data.gemfire.gateway.sender.maximum-queue-memory", 400)
			.withProperty("spring.data.gemfire.gateway.sender.socket-buffer-size", 16384)
			.withProperty("spring.data.gemfire.gateway.sender.socket-read-timeout", 4000)
			.withProperty("spring.data.gemfire.gateway.sender.disk-store-reference", "someDiskStore")
			.withProperty("spring.data.gemfire.gateway.sender.event-filters", "SomeEventFilter")
			.withProperty("spring.data.gemfire.gateway.sender.transport-filters",
				"transportBean2, transportBean1")
			.withProperty("spring.data.gemfire.gateway.sender.regions", "Region1,Region2");

		this.applicationContext = newApplicationContext(testPropertySource, BaseGatewaySenderTestConfiguration.class,
			TestConfigurationWithProperties.class);

		TestGatewaySenderConfigurer gatewaySenderConfigurer =
			this.applicationContext.getBean(TestGatewaySenderConfigurer.class);

		GatewaySender gatewaySender = this.applicationContext.getBean("TestGatewaySender", GatewaySender.class);

		assertThat(gatewaySender.isManualStart()).isEqualTo(true);
		assertThat(gatewaySender.getRemoteDSId()).isEqualTo(2);
		assertThat(gatewaySender.getId()).isEqualTo("TestGatewaySender");
		assertThat(gatewaySender.getDispatcherThreads()).isEqualTo(22);
		assertThat(gatewaySender.isBatchConflationEnabled()).isEqualTo(true);
		assertThat(gatewaySender.isParallel()).isEqualTo(true);
		assertThat(gatewaySender.isPersistenceEnabled()).isEqualTo(false);
		assertThat(gatewaySender.getDiskStoreName()).isEqualTo("someDiskStore");
		assertThat(gatewaySender.getOrderPolicy()).isEqualTo(GatewaySender.OrderPolicy.PARTITION);
		assertThat(((TestGatewayEventSubstitutionFilter) gatewaySender.getGatewayEventSubstitutionFilter()).name)
			.isEqualTo("SomeEventSubstitutionFilter");
		assertThat(gatewaySender.getAlertThreshold()).isEqualTo(1234);
		assertThat(gatewaySender.getBatchSize()).isEqualTo(100);
		assertThat(gatewaySender.getBatchTimeInterval()).isEqualTo(2000);
		assertThat(gatewaySender.getMaximumQueueMemory()).isEqualTo(400);
		assertThat(gatewaySender.getSocketReadTimeout()).isEqualTo(4000);
		assertThat(gatewaySender.getSocketBufferSize()).isEqualTo(16384);

		assertThat(gatewaySender.getGatewayTransportFilters().size()).isEqualTo(2);
		assertThat(gatewaySenderConfigurer.beanNames.get(gatewaySender.getId()).toArray())
			.isEqualTo(new String[] { "transportBean2", "transportBean1" });

		Region<?, ?> region1 = this.applicationContext.getBean("Region1", Region.class);
		Region<?, ?> region2 = this.applicationContext.getBean("Region2", Region.class);

		assertThat(region1.getAttributes().getGatewaySenderIds())
			.containsExactlyInAnyOrder("TestGatewaySender");
		assertThat(region2.getAttributes().getGatewaySenderIds())
			.containsExactlyInAnyOrder("TestGatewaySender");

	}

	@Test
	public void gatewayReceiverPropertiesConfigurationOnParentWithChildOverride() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.gateway.sender.manual-start", true)
			.withProperty("spring.data.gemfire.gateway.sender.TestGatewaySender.manual-start", false)
			.withProperty("spring.data.gemfire.gateway.sender.remote-distributed-system-id", 2)
			.withProperty("spring.data.gemfire.gateway.sender.disk-synchronous", true)
			.withProperty("spring.data.gemfire.gateway.sender.batch-conflation-enabled", true)
			.withProperty("spring.data.gemfire.gateway.sender.parallel", true)
			.withProperty("spring.data.gemfire.gateway.sender.persistent", false)
			.withProperty("spring.data.gemfire.gateway.sender.order-policy", "PARTITION")
			.withProperty("spring.data.gemfire.gateway.sender.event-substitution-filter",
				"SomeEventSubstitutionFilter")
			.withProperty("spring.data.gemfire.gateway.sender.alert-threshold", 1234)
			.withProperty("spring.data.gemfire.gateway.sender.batch-size", 100)
			.withProperty("spring.data.gemfire.gateway.sender.batch-time-interval", 2000)
			.withProperty("spring.data.gemfire.gateway.sender.dispatcher-threads", 22)
			.withProperty("spring.data.gemfire.gateway.sender.maximum-queue-memory", 400)
			.withProperty("spring.data.gemfire.gateway.sender.socket-buffer-size", 16384)
			.withProperty("spring.data.gemfire.gateway.sender.socket-read-timeout", 4000)
			.withProperty("spring.data.gemfire.gateway.sender.disk-store-reference", "someDiskStore")
			.withProperty("spring.data.gemfire.gateway.sender.event-filters", "SomeEventFilter")
			.withProperty("spring.data.gemfire.gateway.sender.transport-filters",
				"transportBean2, transportBean1")
			.withProperty("spring.data.gemfire.gateway.sender.regions", "Region1,Region2");

		this.applicationContext = newApplicationContext(testPropertySource, BaseGatewaySenderTestConfiguration.class,
			TestConfigurationWithProperties.class);

		TestGatewaySenderConfigurer gatewaySenderConfigurer =
			this.applicationContext.getBean(TestGatewaySenderConfigurer.class);

		GatewaySender gatewaySender = this.applicationContext.getBean("TestGatewaySender", GatewaySender.class);

		assertThat(gatewaySender.isManualStart()).isEqualTo(false);
		assertThat(gatewaySender.getRemoteDSId()).isEqualTo(2);
		assertThat(gatewaySender.getId()).isEqualTo("TestGatewaySender");
		assertThat(gatewaySender.getDispatcherThreads()).isEqualTo(22);
		assertThat(gatewaySender.isBatchConflationEnabled()).isEqualTo(true);
		assertThat(gatewaySender.isParallel()).isEqualTo(true);
		assertThat(gatewaySender.isPersistenceEnabled()).isEqualTo(false);
		assertThat(gatewaySender.getDiskStoreName()).isEqualTo("someDiskStore");
		assertThat(gatewaySender.getOrderPolicy()).isEqualTo(GatewaySender.OrderPolicy.PARTITION);
		assertThat(((TestGatewayEventSubstitutionFilter) gatewaySender.getGatewayEventSubstitutionFilter()).name)
			.isEqualTo("SomeEventSubstitutionFilter");
		assertThat(gatewaySender.getAlertThreshold()).isEqualTo(1234);
		assertThat(gatewaySender.getBatchSize()).isEqualTo(100);
		assertThat(gatewaySender.getBatchTimeInterval()).isEqualTo(2000);
		assertThat(gatewaySender.getMaximumQueueMemory()).isEqualTo(400);
		assertThat(gatewaySender.getSocketReadTimeout()).isEqualTo(4000);
		assertThat(gatewaySender.getSocketBufferSize()).isEqualTo(16384);

		assertThat(gatewaySender.getGatewayTransportFilters().size()).isEqualTo(2);
		assertThat(gatewaySenderConfigurer.beanNames.get(gatewaySender.getId()).toArray())
			.isEqualTo(new String[] { "transportBean2", "transportBean1" });

		Region<?, ?> region1 = this.applicationContext.getBean("Region1", Region.class);
		Region<?, ?> region2 = this.applicationContext.getBean("Region2", Region.class);

		assertThat(region1.getAttributes().getGatewaySenderIds())
			.containsExactlyInAnyOrder("TestGatewaySender");
		assertThat(region2.getAttributes().getGatewaySenderIds())
			.containsExactlyInAnyOrder("TestGatewaySender");

	}

	private ConfigurableApplicationContext newApplicationContext(PropertySource<?> testPropertySource,
			Class<?>... annotatedClasses) {

		AnnotationConfigApplicationContext applicationContext = new AnnotationConfigApplicationContext();

		MutablePropertySources propertySources = applicationContext.getEnvironment().getPropertySources();

		propertySources.addFirst(testPropertySource);

		applicationContext.registerShutdownHook();
		applicationContext.register(annotatedClasses);
		applicationContext.refresh();

		return applicationContext;
	}

	@EnableGatewaySenders(gatewaySenders = {
		@EnableGatewaySender(name = "TestGatewaySender", manualStart = true, remoteDistributedSystemId = 2,
			diskSynchronous = true, batchConflationEnabled = true, parallel = true, persistent = false,
			diskStoreReference = "someDiskStore", orderPolicy = OrderPolicyType.PARTITION, alertThreshold = 1234, batchSize = 100,
			batchTimeInterval = 2000, dispatcherThreads = 22, maximumQueueMemory = 400, socketBufferSize = 16384,
			socketReadTimeout = 4000, regions = { "Region1", "Region2" }),
		@EnableGatewaySender(name = "TestGatewaySender2", manualStart = true, remoteDistributedSystemId = 2,
			diskSynchronous = true, batchConflationEnabled = true, parallel = true, persistent = false,
			diskStoreReference = "someDiskStore", orderPolicy = OrderPolicyType.PARTITION, alertThreshold = 1234, batchSize = 100,
			batchTimeInterval = 2000, dispatcherThreads = 22, maximumQueueMemory = 400, socketBufferSize = 16384,
			socketReadTimeout = 4000, regions = { "Region1", "Region2" })
	})
	static class TestConfigurationWithMultipleGatewaySenderAnnotations { }

	@EnableGatewaySender(name = "TestGatewaySender")
	static class TestConfigurationWithProperties {

		@Bean("gatewayConfigurer")
		GatewaySenderConfigurer gatewaySenderConfigurer() {
			return new TestGatewaySenderConfigurer();
		}
	}

	@EnableGatewaySenders(gatewaySenders = {
		@EnableGatewaySender(name = "TestGatewaySender"),
		@EnableGatewaySender(name = "TestGatewaySender2")
	})
	static class TestConfigurationWithPropertiesMultipleGatewaySenders { }

	private static class TestGatewaySenderConfigurer implements GatewaySenderConfigurer {

		private final Map<String, List> beanNames = new TreeMap<>();

		@Override
		public void configure(String beanName, GatewaySenderFactoryBean bean) {
			beanNames.put(beanName, bean.getTransportFilters().stream()
				.map(transportFilter -> ((TestGatewayTransportFilter) transportFilter).name)
				.collect(Collectors.toList()));
		}
	}

	private static class TestGatewayEventSubstitutionFilter implements GatewayEventSubstitutionFilter {

		private String name;

		public TestGatewayEventSubstitutionFilter(String name) {
			this.name = name;
		}

		@Override
		public Object getSubstituteValue(EntryEvent entryEvent) {
			return null;
		}

		@Override
		public void close() { }

	}

	private static class TestGatewayEventFilter implements GatewayEventFilter {

		private String name;

		public TestGatewayEventFilter(String name) {
			this.name = name;
		}

		@Override
		public boolean beforeEnqueue(GatewayQueueEvent gatewayQueueEvent) {
			return false;
		}

		@Override
		public boolean beforeTransmit(GatewayQueueEvent gatewayQueueEvent) {
			return false;
		}

		@Override
		public void afterAcknowledgement(GatewayQueueEvent gatewayQueueEvent) { }

	}

	private static class TestGatewayTransportFilter implements GatewayTransportFilter {

		private String name;

		public TestGatewayTransportFilter(String name) {
			this.name = name;
		}

		@Override
		public InputStream getInputStream(InputStream inputStream) {
			return null;
		}

		@Override
		public OutputStream getOutputStream(OutputStream outputStream) {
			return null;
		}

		@Override
		public int hashCode() {
			return this.name.hashCode();
		}

		@Override
		public boolean equals(Object obj) {
			return this.name.equals(((TestGatewayTransportFilter) obj).name);
		}
	}

	private static class TestGatewaySender extends AbstractGatewaySender implements GatewaySender {

		@Override
		public void start() { }

		@Override
		public void stop() { }

		@Override
		public void setModifiedEventId(EntryEventImpl entryEvent) { }

		@Override
		public void fillInProfile(DistributionAdvisor.Profile profile) { }

	}

	@PeerCacheApplication
	@EnableGemFireMockObjects
	static class BaseGatewaySenderTestConfiguration {

		@Bean("Region1")
		PartitionedRegionFactoryBean createRegion1(GemFireCache gemFireCache) {
			return createRegion("Region1", gemFireCache);
		}

		@Bean("Region2")
		PartitionedRegionFactoryBean createRegion2(GemFireCache gemFireCache) {
			return createRegion("Region2", gemFireCache);
		}

		@Bean("gatewayConfigurer")
		GatewaySenderConfigurer gatewaySenderConfigurer() {
			return new TestGatewaySenderConfigurer();
		}

		@Bean("transportBean1")
		GatewayTransportFilter createGatewayTransportBean1() {
			return new TestGatewayTransportFilter("transportBean1");
		}

		@Bean("transportBean2")
		GatewayTransportFilter createGatewayTransportBean2() {
			return new TestGatewayTransportFilter("transportBean2");
		}

		@Bean("SomeEventFilter")
		GatewayEventFilter createGatewayEventFilter() {
			return new TestGatewayEventFilter("SomeEventFilter");
		}

		@Bean("SomeEventSubstitutionFilter")
		GatewayEventSubstitutionFilter createGatewayEventSubstitutionFilter() {
			return new TestGatewayEventSubstitutionFilter("SomeEventSubstitutionFilter");
		}

		public PartitionedRegionFactoryBean createRegion(String name, GemFireCache gemFireCache) {
			final PartitionedRegionFactoryBean regionFactoryBean = new PartitionedRegionFactoryBean();
			regionFactoryBean.setCache(gemFireCache);
			regionFactoryBean.setDataPolicy(DataPolicy.PARTITION);
			regionFactoryBean.setName(name);
			return regionFactoryBean;
		}
	}
}
