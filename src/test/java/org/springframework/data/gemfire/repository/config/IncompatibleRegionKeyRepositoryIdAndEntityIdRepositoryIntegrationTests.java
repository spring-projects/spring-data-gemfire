/*
 * Copyright 2017-2019 the original author or authors.
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

package org.springframework.data.gemfire.repository.config;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.function.Supplier;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.client.ClientRegionShortcut;
import org.junit.Test;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.FilterType;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.ClientCacheApplication;
import org.springframework.data.gemfire.repository.sample.Animal;
import org.springframework.data.gemfire.repository.sample.Plant;
import org.springframework.data.gemfire.repository.sample.PlantRepository;
import org.springframework.data.gemfire.repository.sample.RabbitRepository;
import org.springframework.data.gemfire.test.mock.annotation.EnableGemFireMockObjects;
import org.springframework.data.mapping.PersistentEntity;
import org.springframework.data.repository.Repository;

/**
 * Integration tests for Spring Data [GemFire] Repositories testing compatibility of {@link Region}
 * {@link RegionAttributes#getKeyConstraint() key type}, {@link Repository} {@link Class ID type}
 * and {@link PersistentEntity entity} {@link Class ID type}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.context.ConfigurableApplicationContext
 * @since 1.4.0
 * @link https://github.com/spring-projects/spring-data-gemfire/pull/55
 */
public class IncompatibleRegionKeyRepositoryIdAndEntityIdRepositoryIntegrationTests {

	private static ConfigurableApplicationContext newApplicationContext(Class<?> testConfiguration) {
		return new AnnotationConfigApplicationContext(testConfiguration);
	}

	private void withTestConfigurationExpectIllegalArgumentExceptionWithMessage(Class<?> testConfiguration,
		Class<? extends Repository> repositoryType, Supplier<String> exceptionMessage) {

		try {

			ConfigurableApplicationContext applicationContext = newApplicationContext(testConfiguration);

			assertThat(applicationContext.getBean(repositoryType)).isNotNull();
		}
		catch (BeanCreationException expected) {

			assertThat(expected).hasCauseInstanceOf(IllegalArgumentException.class);
			assertThat(expected.getCause()).hasMessage(exceptionMessage.get());

			throw (IllegalArgumentException) expected.getCause();
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void withRegionUsingStringKeyAndRepositoryUsingLongId() {

		withTestConfigurationExpectIllegalArgumentExceptionWithMessage(
			TestIncompatibleRegionKeyRepositoryIdTypeConfiguration.class,
			RabbitRepository.class,
			() -> String.format("Region [/Rabbits] requires keys of type [%1$s], but Repository [%2$s] declared an id of type [%3$s]",
				String.class.getName(), RabbitRepository.class.getName(), Long.class.getName()));
	}

	@Test(expected = IllegalArgumentException.class)
	public void withRepositoryUsingStringIdAndEntityUsingLongId() {

		withTestConfigurationExpectIllegalArgumentExceptionWithMessage(
			TestIncompatibleRepositoryIdEntityIdTypeConfiguration.class,
			PlantRepository.class,
			() -> String.format("Repository [%1$s] declared an id of type [%2$s], but entity [%3$s] has an id of type [%4$s]",
				PlantRepository.class.getName(), String.class.getName(), Plant.class.getName(), Long.class.getName()));
	}

	@ClientCacheApplication
	@EnableGemFireMockObjects
	@EnableGemfireRepositories(basePackageClasses = RabbitRepository.class,
		includeFilters = @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, classes = RabbitRepository.class)
	)
	@SuppressWarnings("unused")
	static class TestIncompatibleRegionKeyRepositoryIdTypeConfiguration {

		@Bean("Rabbits")
		public ClientRegionFactoryBean<String, Animal> clientRegion(GemFireCache gemfireCache) {

			ClientRegionFactoryBean<String, Animal> clientRegion = new ClientRegionFactoryBean<>();

			clientRegion.setCache(gemfireCache);
			clientRegion.setClose(false);
			clientRegion.setKeyConstraint(String.class);
			clientRegion.setShortcut(ClientRegionShortcut.LOCAL);
			clientRegion.setValueConstraint(Animal.class);

			return clientRegion;
		}
	}

	@ClientCacheApplication
	@EnableGemFireMockObjects
	@EnableGemfireRepositories(basePackageClasses = PlantRepository.class,
		includeFilters = @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, classes = PlantRepository.class)
	)
	@SuppressWarnings("unused")
	static class TestIncompatibleRepositoryIdEntityIdTypeConfiguration {

		@Bean("Plants")
		public ClientRegionFactoryBean<Object, Object> clientRegion(GemFireCache gemfireCache) {

			ClientRegionFactoryBean<Object, Object> clientRegion = new ClientRegionFactoryBean<>();

			clientRegion.setCache(gemfireCache);
			clientRegion.setClose(false);
			clientRegion.setShortcut(ClientRegionShortcut.PROXY);

			return clientRegion;
		}
	}
}
