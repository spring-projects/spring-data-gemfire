/*
 * Copyright 2010-2020 the original author or authors.
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

import org.junit.After;
import org.junit.Test;

import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.data.gemfire.wan.GatewaySenderFactoryBean;
import org.springframework.data.gemfire.wan.OrderPolicyType;

/**
 * Tests for {@link EnableGatewaySenders} and {@link EnableGatewaySender}.
 *
 * @author Udo Kohlmeyer
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.server.CacheServer
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @see org.springframework.data.gemfire.wan.GatewayReceiverFactoryBean
 * @see GatewaySenderConfigurer
 * @see GatewaySenderConfiguration
 * @since 2.2.0
 */
public class GatewaySenderConfigurationTests {

	private ConfigurableApplicationContext applicationContext;

	@After
	public void shutdown() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
	}

	@Test
	public void annotationConfigurationOfMultipleGatewaySendersWithDefaultsFromParent() {

		this.applicationContext = newApplicationContext(BaseGatewaySenderTestConfiguration.class,
			TestConfigurationOfMultipleGatewaySenderAnnotationsButWithDefaultsFromParent.class);

		TestGatewaySenderConfigurer gatewaySenderConfigurer =
			this.applicationContext.getBean(TestGatewaySenderConfigurer.class);

		Map<String, GatewaySender> beansOfType = this.applicationContext.getBeansOfType(GatewaySender.class);

		String[] senders = new String[] { "TestGatewaySender", "TestGatewaySender2" };

		assertThat(beansOfType).hasSize(2);
		assertThat(beansOfType.keySet()).containsExactlyInAnyOrder(senders);

		for (String sender : senders) {

			GatewaySender gatewaySender = this.applicationContext.getBean(sender, GatewaySender.class);

			assertThat(gatewaySender.isManualStart()).isEqualTo(true);
			assertThat(gatewaySender.getRemoteDSId()).isEqualTo(2);
			assertThat(gatewaySender.getId()).isEqualTo(sender);
			assertThat(gatewaySender.getDispatcherThreads()).isEqualTo(22);
			assertThat(gatewaySender.isBatchConflationEnabled()).isEqualTo(true);
			assertThat(gatewaySender.isParallel()).isEqualTo(true);
			assertThat(gatewaySender.isPersistenceEnabled()).isEqualTo(true);
			assertThat(gatewaySender.getDiskStoreName()).isEqualTo("someDiskStore");
			assertThat(gatewaySender.getOrderPolicy()).isEqualTo(GatewaySender.OrderPolicy.PARTITION);
			assertThat(gatewaySender.getGatewayEventSubstitutionFilter()).isNull();
			assertThat(gatewaySender.getAlertThreshold()).isEqualTo(1234);
			assertThat(gatewaySender.getBatchSize()).isEqualTo(1002);
			assertThat(gatewaySender.getBatchTimeInterval()).isEqualTo(2000);
			assertThat(gatewaySender.getMaximumQueueMemory()).isEqualTo(400);
			assertThat(gatewaySender.getSocketReadTimeout()).isEqualTo(4000);
			assertThat(gatewaySender.getSocketBufferSize()).isEqualTo(16384);

			assertThat(gatewaySender.getGatewayTransportFilters().size()).isEqualTo(2);
			assertThat(((GatewaySenderConfigurationTests.TestGatewayTransportFilter) gatewaySender
				.getGatewayTransportFilters().get(0)).name).isEqualTo("transportBean2");
			assertThat(((GatewaySenderConfigurationTests.TestGatewayTransportFilter) gatewaySender
				.getGatewayTransportFilters().get(1)).name).isEqualTo("transportBean1");
			assertThat(gatewaySenderConfigurer.beanNames.get(gatewaySender.getId()).toArray())
				.isEqualTo(new String[] { "transportBean2", "transportBean1" });

			Region<?, ?> region1 = this.applicationContext.getBean("Region1", Region.class);
			Region<?, ?> region2 = this.applicationContext.getBean("Region2", Region.class);

			assertThat(region1.getAttributes().getGatewaySenderIds())
				.containsExactlyInAnyOrder("TestGatewaySender", "TestGatewaySender2");
			assertThat(region2.getAttributes().getGatewaySenderIds())
				.containsExactlyInAnyOrder("TestGatewaySender", "TestGatewaySender2");
		}
	}

	@Test
	public void annotationConfiguredMultipleGatewaySenders() {

		this.applicationContext = newApplicationContext(BaseGatewaySenderTestConfiguration.class,
			TestConfigurationWithMultipleGatewaySenderAnnotations.class);

		Map<String, GatewaySender> beansOfType = this.applicationContext.getBeansOfType(GatewaySender.class);

		assertThat(beansOfType.keySet().toArray()).containsExactlyInAnyOrder("TestGatewaySender", "TestGatewaySender2");
	}

	@Test
	public void annotationConfiguredGatewaySender() {

		this.applicationContext = newApplicationContext(BaseGatewaySenderTestConfiguration.class,
			TestConfigurationWithAnnotations.class);

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
		assertThat(gatewaySender.getGatewayEventFilters())
			.containsExactlyInAnyOrder(this.applicationContext.getBean("SomeEventFilter", GatewayEventFilter.class));
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

		assertThat(region1.getAttributes().getGatewaySenderIds()).containsExactlyInAnyOrder("TestGatewaySender");
		assertThat(region2.getAttributes().getGatewaySenderIds()).containsExactlyInAnyOrder("TestGatewaySender");
	}

	@Test
	public void annotationConfigurationOfMultipleGatewaySendersWithOverrides() {

		this.applicationContext = newApplicationContext(BaseGatewaySenderTestConfiguration.class,
			TestConfigurationOfMultipleGatewaySenderAnnotationsWithOverrides.class);

		TestGatewaySenderConfigurer gatewaySenderConfigurer =
			this.applicationContext.getBean("gatewayConfigurer", TestGatewaySenderConfigurer.class);

		Map<String, GatewaySender> beansOfType = this.applicationContext.getBeansOfType(GatewaySender.class);

		String[] senders = new String[] { "TestGatewaySender", "TestGatewaySender2" };

		assertThat(beansOfType).hasSize(2);
		assertThat(beansOfType.keySet()).containsExactlyInAnyOrder(senders);

		for (String sender : senders) {

			GatewaySender gatewaySender = this.applicationContext.getBean(sender, GatewaySender.class);

			assertThat(gatewaySender.isManualStart()).isEqualTo(true);
			assertThat(gatewaySender.getRemoteDSId()).isEqualTo(2);
			assertThat(gatewaySender.getId()).isEqualTo(sender);
			assertThat(gatewaySender.getDispatcherThreads()).isEqualTo(22);
			assertThat(gatewaySender.isBatchConflationEnabled()).isEqualTo(true);
			assertThat(gatewaySender.isParallel()).isEqualTo(true);
			assertThat(gatewaySender.isPersistenceEnabled()).isEqualTo(true);
			assertThat(gatewaySender.getDiskStoreName()).isEqualTo("someDiskStore");
			assertThat(gatewaySender.getOrderPolicy()).isEqualTo(GatewaySender.OrderPolicy.PARTITION);
			assertThat(gatewaySender.getGatewayEventSubstitutionFilter()).isNull();
			assertThat(gatewaySender.getAlertThreshold()).isEqualTo(1234);
			assertThat(gatewaySender.getBatchSize()).isEqualTo(1002);
			assertThat(gatewaySender.getBatchTimeInterval()).isEqualTo(2000);
			assertThat(gatewaySender.getMaximumQueueMemory()).isEqualTo(400);
			assertThat(gatewaySender.getSocketReadTimeout()).isEqualTo(4000);
			assertThat(gatewaySender.getSocketBufferSize()).isEqualTo(16384);
		}

		GatewaySender gatewaySender = this.applicationContext.getBean("TestGatewaySender", GatewaySender.class);

		assertThat(gatewaySender.getGatewayTransportFilters().size()).isEqualTo(1);
		assertThat(((GatewaySenderConfigurationTests.TestGatewayTransportFilter) gatewaySender
			.getGatewayTransportFilters().get(0)).name).isEqualTo("transportBean1");

		gatewaySender = this.applicationContext.getBean("TestGatewaySender2", GatewaySender.class);

		assertThat(gatewaySender.getGatewayTransportFilters().size()).isEqualTo(2);
		assertThat(gatewaySenderConfigurer.beanNames.get(gatewaySender.getId()).toArray())
			.isEqualTo(new String[] { "transportBean2", "transportBean1" });

		Region<?, ?> region1 = this.applicationContext.getBean("Region1", Region.class);
		Region<?, ?> region2 = this.applicationContext.getBean("Region2", Region.class);

		assertThat(region1.getAttributes().getGatewaySenderIds()).containsExactlyInAnyOrder("TestGatewaySender2");
		assertThat(region2.getAttributes().getGatewaySenderIds())
			.containsExactlyInAnyOrder("TestGatewaySender", "TestGatewaySender2");
	}

	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {

		AnnotationConfigApplicationContext applicationContext =
			new AnnotationConfigApplicationContext(annotatedClasses);

		applicationContext.registerShutdownHook();

		return applicationContext;
	}

	@EnableGatewaySenders(gatewaySenders = {
		@EnableGatewaySender(name = "TestGatewaySender", manualStart = true, remoteDistributedSystemId = 2,
			diskSynchronous = true, batchConflationEnabled = true, parallel = true, persistent = false,
			diskStoreReference = "someDiskStore", orderPolicy = OrderPolicyType.PARTITION, eventFilters = "SomeEventFilter",
			eventSubstitutionFilter = "SomeEventSubstitutionFilter", alertThreshold = 1234, batchSize = 100,
			batchTimeInterval = 2000, dispatcherThreads = 22, maximumQueueMemory = 400, socketBufferSize = 16384,
			socketReadTimeout = 4000, transportFilters = { "transportBean2", "transportBean1" },
			regions = { "Region1", "Region2" })
	})
	static class TestConfigurationWithAnnotations {

		@Bean("SomeEventSubstitutionFilter")
		GatewayEventSubstitutionFilter createGatewayEventSubstitutionFilter() {
			return new TestGatewayEventSubstitutionFilter("SomeEventSubstitutionFilter");
		}
	}

	@EnableGatewaySenders(gatewaySenders = {
		@EnableGatewaySender(name = "TestGatewaySender", manualStart = true, remoteDistributedSystemId = 2,
			diskSynchronous = true, batchConflationEnabled = true, parallel = true, persistent = false,
			diskStoreReference = "someDiskStore", orderPolicy = OrderPolicyType.PARTITION, alertThreshold = 1234, batchSize = 100,
			eventFilters = "SomeEventFilter", batchTimeInterval = 2000, dispatcherThreads = 22, maximumQueueMemory = 400, socketBufferSize = 16384,
			socketReadTimeout = 4000, regions = { "Region1", "Region2" }),
		@EnableGatewaySender(name = "TestGatewaySender2", manualStart = true, remoteDistributedSystemId = 2,
			diskSynchronous = true, batchConflationEnabled = true, parallel = true, persistent = false,
			diskStoreReference = "someDiskStore", orderPolicy = OrderPolicyType.PARTITION, alertThreshold = 1234, batchSize = 100,
			eventFilters = "SomeEventFilter", batchTimeInterval = 2000, dispatcherThreads = 22, maximumQueueMemory = 400, socketBufferSize = 16384,
			socketReadTimeout = 4000, regions = { "Region1", "Region2" })
	})
	static class TestConfigurationWithMultipleGatewaySenderAnnotations {

		@Bean("SomeEventSubstitutionFilter")
		GatewayEventSubstitutionFilter createGatewayEventSubstitutionFilter() {
			return new TestGatewayEventSubstitutionFilter("SomeEventSubstitutionFilter");
		}
	}

	@EnableGatewaySenders(gatewaySenders = {
		@EnableGatewaySender(name = "TestGatewaySender"),
		@EnableGatewaySender(name = "TestGatewaySender2")
	},
		manualStart = true, remoteDistributedSystemId = 2,
		diskSynchronous = false, batchConflationEnabled = true, parallel = true, persistent = true,
		diskStoreReference = "someDiskStore", orderPolicy = OrderPolicyType.PARTITION, alertThreshold = 1234, batchSize = 1002,
		eventFilters = "SomeEventFilter", batchTimeInterval = 2000, dispatcherThreads = 22, maximumQueueMemory = 400, socketBufferSize = 16384,
		socketReadTimeout = 4000, regions = { "Region1", "Region2" },
		transportFilters = { "transportBean2", "transportBean1" })
	static class TestConfigurationOfMultipleGatewaySenderAnnotationsButWithDefaultsFromParent { }

	@EnableGatewaySenders(gatewaySenders = {
		@EnableGatewaySender(name = "TestGatewaySender", transportFilters = "transportBean1", regions = "Region2"),
		@EnableGatewaySender(name = "TestGatewaySender2")
	},
		manualStart = true, remoteDistributedSystemId = 2,
		diskSynchronous = false, batchConflationEnabled = true, parallel = true, persistent = true,
		diskStoreReference = "someDiskStore", orderPolicy = OrderPolicyType.PARTITION, alertThreshold = 1234, batchSize = 1002,
		eventFilters = "SomeEventFilter", batchTimeInterval = 2000, dispatcherThreads = 22, maximumQueueMemory = 400, socketBufferSize = 16384,
		socketReadTimeout = 4000, regions = { "Region1", "Region2" },
		transportFilters = { "transportBean2", "transportBean1" })
	static class TestConfigurationOfMultipleGatewaySenderAnnotationsWithOverrides { }

	private static class TestGatewaySenderConfigurer implements GatewaySenderConfigurer {

		private final Map<String, List> beanNames = new TreeMap<>();

		@Override
		public void configure(String beanName, GatewaySenderFactoryBean bean) {
			beanNames.put(beanName, bean.getTransportFilters().stream()
				.map(transportFilter -> ((TestGatewayTransportFilter) transportFilter).name)
				.collect(Collectors.toList()));
		}
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
			return name.hashCode();
		}

		@Override
		public boolean equals(Object obj) {
			return this.name.equals(((TestGatewayTransportFilter) obj).name);
		}
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

		public PartitionedRegionFactoryBean createRegion(String name, GemFireCache gemFireCache) {
			final PartitionedRegionFactoryBean regionFactoryBean = new PartitionedRegionFactoryBean();
			regionFactoryBean.setCache(gemFireCache);
			regionFactoryBean.setDataPolicy(DataPolicy.PARTITION);
			regionFactoryBean.setName(name);
			return regionFactoryBean;
		}
	}
}
