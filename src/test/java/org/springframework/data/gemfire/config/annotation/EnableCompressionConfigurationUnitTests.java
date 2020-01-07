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
import static org.mockito.Mockito.mock;
import static org.springframework.data.gemfire.config.annotation.CompressionConfiguration.SNAPPY_COMPRESSOR_BEAN_NAME;

import java.util.Arrays;
import java.util.Optional;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.compression.Compressor;
import org.apache.geode.compression.SnappyCompressor;
import org.junit.After;
import org.junit.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.ReplicatedRegionFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.data.gemfire.test.model.Person;

/**
 * The EnableCompressionConfigurationUnitTests class...
 *
 * @author John Blum
 * @since 1.0.0
 */
public class EnableCompressionConfigurationUnitTests {

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
		//GemFireMockObjectsSupport.destroy();
	}

	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		return new AnnotationConfigApplicationContext(annotatedClasses);
	}

	private void assertRegionCompressor(Region<?, ?> region, String regionName, Compressor compressor) {

		assertThat(region).isNotNull();
		assertThat(region.getName()).isEqualTo(regionName);
		assertThat(region.getFullPath()).isEqualTo(GemfireUtils.toRegionPath(regionName));
		assertThat(region.getAttributes()).isNotNull();
		assertThat(region.getAttributes().getCompressor()).isEqualTo(compressor);
	}

	@Test
	public void enableCompressionForAllRegions() {

		this.applicationContext = newApplicationContext(EnableCompressionForAllRegionsConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("ExampleClientRegion")).isFalse();

		Compressor compressor = this.applicationContext.getBean(Compressor.class);

		assertThat(compressor).isInstanceOf(SnappyCompressor.class);

		Arrays.asList("People", "ExampleLocalRegion", "ExamplePartitionRegion", "ExampleReplicateRegion")
			.forEach(regionName -> {
				assertThat(this.applicationContext.containsBean(regionName)).isTrue();
				assertRegionCompressor(this.applicationContext.getBean(regionName, Region.class),
					regionName, compressor);
			});
	}

	@Test
	public void enableCompressionForSelectRegions() {

		this.applicationContext = newApplicationContext(EnableCompressionForSelectRegionsConfiguration.class);

		assertThat(this.applicationContext).isNotNull();

		Compressor compressor = this.applicationContext.getBean("MockCompressor", Compressor.class);

		assertThat(compressor).isNotNull();
		assertThat(compressor).isNotInstanceOf(SnappyCompressor.class);
		assertThat(this.applicationContext.containsBean(SNAPPY_COMPRESSOR_BEAN_NAME)).isTrue();

		Arrays.asList("People", "ExampleClientRegion").forEach(regionName -> {
			assertThat(this.applicationContext.containsBean(regionName)).isTrue();
			assertRegionCompressor(this.applicationContext.getBean(regionName, Region.class),
				regionName, "People".equals(regionName) ? compressor : null);
		});
	}

	@PeerCacheApplication
	@EnableGemFireMockObjects
	@EnableEntityDefinedRegions(basePackageClasses = Person.class)
	@EnableCompression
	@SuppressWarnings("unused")
	static class EnableCompressionForAllRegionsConfiguration {

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

	@ClientCacheApplication
	@EnableGemFireMockObjects
	@EnableEntityDefinedRegions(basePackageClasses = Person.class)
	@EnableCompression(compressorBeanName = "MockCompressor", regionNames = "People")
	@SuppressWarnings("unused")
	static class EnableCompressionForSelectRegionsConfiguration {

		@Bean("ExampleClientRegion")
		public ClientRegionFactoryBean<Object, Object> clientRegion(GemFireCache gemfireCache) {

			ClientRegionFactoryBean<Object, Object> clientRegion = new ClientRegionFactoryBean<>();

			clientRegion.setCache(gemfireCache);
			clientRegion.setClose(false);
			clientRegion.setShortcut(ClientRegionShortcut.LOCAL);

			return clientRegion;
		}

		@Bean("MockCompressor")
		Compressor mockCompressor() {
			return mock(Compressor.class);
		}
	}
}
