/*
 * Copyright 2017-2020 the original author or authors.
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

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;
import java.util.Optional;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.control.ResourceManager;

import org.junit.After;
import org.junit.Test;

import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.ReplicatedRegionFactoryBean;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.data.gemfire.test.model.Person;

/**
 * Unit tests for {@link EnableOffHeap} and {@link OffHeapConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.config.annotation.EnableOffHeap
 * @see org.springframework.data.gemfire.config.annotation.OffHeapConfiguration
 * @since 2.0.2
 */
public class EnableOffHeapConfigurationUnitTests {

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
	}

	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		return new AnnotationConfigApplicationContext(annotatedClasses);
	}

	private void assertRegionOffHeap(Region<?, ?> region, String regionName, boolean offHeapEnabled) {

		assertThat(region).isNotNull();
		assertThat(region.getName()).isEqualTo(regionName);
		assertThat(region.getFullPath()).isEqualTo(GemfireUtils.toRegionPath(regionName));
		assertThat(region.getAttributes()).isNotNull();
		assertThat(region.getAttributes().getOffHeap()).isEqualTo(offHeapEnabled);
	}

	@Test
	public void offHeapCriticalAndEvictionMemoryPercentagesConfiguredProperly() {

		this.applicationContext = newApplicationContext(OffHeapCriticalAndEvictionMemoryPercentagesConfiguration.class);

		assertThat(this.applicationContext).isNotNull();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();
		assertThat(gemfireCache.getDistributedSystem().getProperties()).containsKey("off-heap-memory-size");
		assertThat(gemfireCache.getDistributedSystem().getProperties().getProperty("off-heap-memory-size"))
			.isEqualTo("1024g");

		ResourceManager resourceManager = gemfireCache.getResourceManager();

		assertThat(resourceManager).isNotNull();
		assertThat(resourceManager.getCriticalHeapPercentage()).isEqualTo(95.55f);
		assertThat(resourceManager.getCriticalOffHeapPercentage()).isEqualTo(90.5f);
		assertThat(resourceManager.getEvictionHeapPercentage()).isEqualTo(85.75f);
		assertThat(resourceManager.getEvictionOffHeapPercentage()).isEqualTo(75.25f);
	}

	@Test
	public void offHeapConfiguredForAllRegions() {

		this.applicationContext = newApplicationContext(EnableOffHeapForAllRegionsConfiguration.class);

		assertThat(this.applicationContext).isNotNull();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();
		assertThat(gemfireCache.getDistributedSystem().getProperties()).containsKey("off-heap-memory-size");
		assertThat(gemfireCache.getDistributedSystem().getProperties().getProperty("off-heap-memory-size"))
			.isEqualTo("8192m");

		Arrays.asList("People", "ExampleLocalRegion", "ExamplePartitionRegion", "ExampleReplicateRegion")
			.forEach(regionName -> {
				assertThat(this.applicationContext.containsBean(regionName)).isTrue();
				assertRegionOffHeap(this.applicationContext.getBean(regionName, Region.class),
					regionName, true);
			});
	}

	@Test
	public void offHeapConfiguredForSelectRegions() {

		this.applicationContext = newApplicationContext(EnableOffHeapForSelectRegionsConfiguration.class);

		assertThat(this.applicationContext).isNotNull();

		GemFireCache gemfireCache = this.applicationContext.getBean("gemfireCache", GemFireCache.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getDistributedSystem()).isNotNull();
		assertThat(gemfireCache.getDistributedSystem().getProperties()).containsKey("off-heap-memory-size");
		assertThat(gemfireCache.getDistributedSystem().getProperties().getProperty("off-heap-memory-size"))
			.isEqualTo("1024m");

		Arrays.asList("People", "ExampleLocalRegion", "ExamplePartitionRegion", "ExampleReplicateRegion")
			.forEach(regionName -> {
				assertThat(this.applicationContext.containsBean(regionName)).isTrue();
				assertRegionOffHeap(this.applicationContext.getBean(regionName, Region.class),
					regionName, Arrays.asList("People", "ExamplePartitionRegion").contains(regionName));
			});
	}

	@Configuration
	@SuppressWarnings("unused")
	static class TestRegionConfiguration {

		@Bean("ExampleLocalRegion")
		public LocalRegionFactoryBean<Object, Object> localRegion(GemFireCache gemfireCache) {

			LocalRegionFactoryBean<Object, Object> localRegion = new LocalRegionFactoryBean<>();

			localRegion.setCache(gemfireCache);
			localRegion.setClose(false);
			localRegion.setPersistent(false);

			return localRegion;
		}

		@Bean("ExamplePartitionRegion")
		public PartitionedRegionFactoryBean<Object, Object> partitionRegion(GemFireCache gemfireCache) {

			PartitionedRegionFactoryBean<Object, Object> partitionRegion = new PartitionedRegionFactoryBean<>();

			partitionRegion.setCache(gemfireCache);
			partitionRegion.setClose(false);
			partitionRegion.setPersistent(false);

			return partitionRegion;
		}

		@Bean("ExampleReplicateRegion")
		public ReplicatedRegionFactoryBean<Object, Object> replicateRegion(GemFireCache gemfireCache) {

			ReplicatedRegionFactoryBean<Object, Object> replicateRegion = new ReplicatedRegionFactoryBean<>();

			replicateRegion.setCache(gemfireCache);
			replicateRegion.setClose(false);
			replicateRegion.setPersistent(false);

			return replicateRegion;
		}
	}

	@PeerCacheApplication
	@EnableGemFireMockObjects
	@EnableEntityDefinedRegions(basePackageClasses = Person.class)
	@EnableOffHeap(memorySize = "8192m")
	@Import(TestRegionConfiguration.class)
	static class EnableOffHeapForAllRegionsConfiguration {
	}

	@PeerCacheApplication
	@EnableGemFireMockObjects
	@EnableEntityDefinedRegions(basePackageClasses = Person.class)
	@EnableOffHeap(memorySize = "1024m", regionNames = { "People", "ExamplePartitionRegion" })
	@Import(TestRegionConfiguration.class)
	static class EnableOffHeapForSelectRegionsConfiguration {
	}

	@EnableGemFireMockObjects
	@PeerCacheApplication(
		criticalHeapPercentage = 95.55f,
		criticalOffHeapPercentage = 90.5f,
		evictionHeapPercentage = 85.75f,
		evictionOffHeapPercentage = 75.25f
	)
	@EnableOffHeap(memorySize = "1024g")
	static class OffHeapCriticalAndEvictionMemoryPercentagesConfiguration {
	}
}
