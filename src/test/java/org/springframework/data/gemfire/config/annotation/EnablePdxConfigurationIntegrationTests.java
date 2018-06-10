/*
 * Copyright 2018 the original author or authors.
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
import static org.mockito.Mockito.mock;

import java.util.Optional;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.apache.geode.pdx.PdxSerializer;
import org.junit.After;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.DiskStoreFactoryBean;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.mapping.MappingPdxSerializer;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;

/**
 * The EnablePdxConfigurationIntegrationTests class...
 *
 * @author John Blum
 * @since 1.0.0
 */
public class EnablePdxConfigurationIntegrationTests {

	@Autowired
	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
	}

	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		return new AnnotationConfigApplicationContext(annotatedClasses);
	}

	@Test
	public void regionBeanDefinitionDependsOnPdxDiskStoreBean() {

		this.applicationContext = newApplicationContext(TestEnablePdxWithDiskStoreConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();
		assertThat(this.applicationContext.containsBean("MockPdxSerializer")).isTrue();
		assertThat(this.applicationContext.containsBean("TestDiskStore")).isTrue();
		assertThat(this.applicationContext.containsBean("TestRegion")).isTrue();

		CacheFactoryBean gemfireCache = this.applicationContext.getBean("&gemfireCache", CacheFactoryBean.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getPdxSerializer())
			.isEqualTo(this.applicationContext.getBean("MockPdxSerializer", PdxSerializer.class));

		BeanDefinition testDiskStoreBeanDefinition =
			this.applicationContext.getBeanFactory().getBeanDefinition("TestDiskStore");

		assertThat(testDiskStoreBeanDefinition).isNotNull();
		assertThat(testDiskStoreBeanDefinition.getDependsOn()).isNullOrEmpty();

		BeanDefinition testRegionBeanDefinition =
			this.applicationContext.getBeanFactory().getBeanDefinition("TestRegion");

		assertThat(testRegionBeanDefinition).isNotNull();
		assertThat(testRegionBeanDefinition.getDependsOn()).containsExactly("TestDiskStore");
	}

	@Test
	public void regionBeanDefinitionHasNoDependencies() {

		this.applicationContext = newApplicationContext(TestEnablePdxConfigurationWithNoDiskStoreConfiguration.class);

		assertThat(this.applicationContext).isNotNull();
		assertThat(this.applicationContext.containsBean("gemfireCache")).isTrue();
		assertThat(this.applicationContext.containsBean("TestDiskStore")).isTrue();
		assertThat(this.applicationContext.containsBean("TestRegion")).isTrue();

		CacheFactoryBean gemfireCache = this.applicationContext.getBean("&gemfireCache", CacheFactoryBean.class);

		assertThat(gemfireCache).isNotNull();
		assertThat(gemfireCache.getPdxSerializer()).isInstanceOf(MappingPdxSerializer.class);

		BeanDefinition testDiskStoreBeanDefinition =
			this.applicationContext.getBeanFactory().getBeanDefinition("TestDiskStore");

		assertThat(testDiskStoreBeanDefinition).isNotNull();
		assertThat(testDiskStoreBeanDefinition.getDependsOn()).isNullOrEmpty();

		BeanDefinition testRegionBeanDefinition =
			this.applicationContext.getBeanFactory().getBeanDefinition("TestRegion");

		assertThat(testRegionBeanDefinition).isNotNull();
		assertThat(testRegionBeanDefinition.getDependsOn()).isNullOrEmpty();

	}

	@EnableGemFireMockObjects
	@PeerCacheApplication
	@EnablePdx(diskStoreName = "TestDiskStore", serializerBeanName = "MockPdxSerializer")
	@SuppressWarnings("unused")
	static class TestEnablePdxWithDiskStoreConfiguration {

		@Bean("TestDiskStore")
		DiskStoreFactoryBean testPdxDiskStore(GemFireCache gemfireCache) {

			DiskStoreFactoryBean testDiskStore = new DiskStoreFactoryBean();

			testDiskStore.setCache(gemfireCache);

			return testDiskStore;
		}

		@Bean("TestRegion")
		LocalRegionFactoryBean<Object, Object> testRegion(GemFireCache gemfireCache) {

			LocalRegionFactoryBean<Object, Object> testRegion = new LocalRegionFactoryBean<>();

			testRegion.setCache(gemfireCache);
			testRegion.setClose(false);
			testRegion.setPersistent(false);

			return testRegion;
		}

		@Bean("MockPdxSerializer")
		PdxSerializer mockPdxSerializer() {
			return mock(PdxSerializer.class);
		}
	}

	@EnableGemFireMockObjects
	@ClientCacheApplication
	@EnablePdx
	@SuppressWarnings("unused")
	static class TestEnablePdxConfigurationWithNoDiskStoreConfiguration {

		@Bean("TestDiskStore")
		DiskStoreFactoryBean testPdxDiskStore(GemFireCache gemfireCache) {

			DiskStoreFactoryBean testDiskStore = new DiskStoreFactoryBean();

			testDiskStore.setCache(gemfireCache);

			return testDiskStore;
		}

		@Bean("TestRegion")
		public ClientRegionFactoryBean<Object, Object> testRegion(GemFireCache gemfireCache) {

			ClientRegionFactoryBean<Object, Object> testRegion = new ClientRegionFactoryBean<>();

			testRegion.setCache(gemfireCache);
			testRegion.setClose(false);
			testRegion.setShortcut(ClientRegionShortcut.LOCAL);

			return testRegion;
		}
	}
}
