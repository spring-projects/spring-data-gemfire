/*
 * Copyright 2017 the original author or authors.
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

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Optional;

import org.apache.geode.cache.DiskStore;
import org.apache.geode.cache.DiskStoreFactory;
import org.apache.geode.cache.GemFireCache;
import org.junit.After;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.MutablePropertySources;
import org.springframework.core.env.PropertySource;
import org.springframework.data.gemfire.test.mock.MockGemFireObjectsSupport;
import org.springframework.mock.env.MockPropertySource;

/**
 * The DiskStorePropertiesIntegrationTests class...
 *
 * @author John Blum
 * @since 1.0.0
 */
public class DiskStorePropertiesIntegrationTests {

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
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

	@Test
	public void diskStoreConfigurationIsCorrect() {

		MockPropertySource testPropertySource = new MockPropertySource()
			.withProperty("spring.data.gemfire.disk.store.compaction-threshold", 85)
			.withProperty("spring.data.gemfire.disk.store.disk-usage-critical-percentage", 90.0f)
			.withProperty("spring.data.gemfire.disk.store.disk-usage-warning-percentage", 80.0f)
			.withProperty("spring.data.gemfire.disk.store.TestDiskStore.disk-usage-warning-percentage", 85.0f)
			.withProperty("spring.data.gemfire.disk.store.time-interval", 500L)
			.withProperty("spring.data.gemfire.disk.store.NonExistingDiskStore.time-interval", 30000L);

		this.applicationContext = newApplicationContext(testPropertySource, TestDiskStoreConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("TestDiskStore")).isTrue();

		DiskStore testDiskStore = this.applicationContext.getBean("TestDiskStore", DiskStore.class);

		assertThat(testDiskStore).isNotNull();
		assertThat(testDiskStore.getName()).isEqualTo("TestDiskStore");
		assertThat(testDiskStore.getAllowForceCompaction()).isTrue();
		assertThat(testDiskStore.getAutoCompact()).isTrue();
		assertThat(testDiskStore.getCompactionThreshold()).isEqualTo(75);
		assertThat(testDiskStore.getDiskUsageCriticalPercentage()).isEqualTo(90.0f);
		assertThat(testDiskStore.getDiskUsageWarningPercentage()).isEqualTo(85.0f);
		assertThat(testDiskStore.getMaxOplogSize()).isEqualTo(512L);
		assertThat(testDiskStore.getQueueSize()).isEqualTo(DiskStoreFactory.DEFAULT_QUEUE_SIZE);
		assertThat(testDiskStore.getTimeInterval()).isEqualTo(500L);
		assertThat(testDiskStore.getWriteBufferSize()).isEqualTo(DiskStoreFactory.DEFAULT_WRITE_BUFFER_SIZE);
	}

	// TODO add more tests!

	@Configuration
	@EnableDiskStore(name = "TestDiskStore", allowForceCompaction = true, compactionThreshold = 65,
		diskUsageCriticalPercentage = 95.0f, diskUsageWarningPercentage = 75.0f, maxOplogSize = 2048L)
	@SuppressWarnings("unused")
	static class TestDiskStoreConfiguration {

		@Bean("gemfireCache")
		GemFireCache mockGemFireCache() {
			return MockGemFireObjectsSupport.mockGemFireCache();
		}

		@Bean
		DiskStoreConfigurer testDiskStoreConfigurer() {
			return (beanName, factoryBean) -> {
				factoryBean.setCompactionThreshold(75);
				//factoryBean.setDiskUsageWarningPercentage(95.0f);
				factoryBean.setMaxOplogSize(512L);
			};
		}
	}
}
