/*
 * Copyright 2012-2020 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.repository;

import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasItem;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

import java.util.Arrays;
import java.util.Map;
import java.util.Properties;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.RegionAttributes;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.FilterType;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.RegionAttributesFactoryBean;
import org.springframework.data.gemfire.repository.config.EnableGemfireRepositories;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.gemfire.repository.sample.PersonRepository;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean;
import org.springframework.data.repository.core.support.RepositoryFactoryInformation;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * Test suite of test cases testing that the GemFire-based {@link org.springframework.data.repository.Repository}
 * factories, implementing the {@link RepositoryFactoryInformation} interface, can in fact be looked up in the
 * Spring {@link ApplicationContext}.
 *
 * @author John Blum
 * @since 1.9.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class GemFireRepositoryFactoryInformationIntegrationTests {

	@Autowired
	private ApplicationContext applicationContext;

	@Test
	public void canAccessRepositoryFactoryInformationFactoryBeans() {
		Map<String, RepositoryFactoryInformation> repositoryFactories =
			applicationContext.getBeansOfType(RepositoryFactoryInformation.class);

		assertThat(repositoryFactories, is(notNullValue(Map.class)));
		assertThat(repositoryFactories.size(), is(greaterThan(0)));
		assertThat(repositoryFactories.keySet(), hasItem("&personRepository"));
		assertThat(repositoryFactories.get("&personRepository"), is(instanceOf(GemfireRepositoryFactoryBean.class)));
		assertThat(Arrays.asList(applicationContext.getBeanNamesForType(PersonRepository.class)),
			hasItem("personRepository"));
	}

	@Configuration
	@EnableGemfireRepositories(basePackageClasses = { Person.class },
		includeFilters = @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE,
			value = org.springframework.data.gemfire.repository.sample.PersonRepository.class))
	static class GemFireConfiguration {

		String applicationName() {
			return GemFireRepositoryFactoryInformationIntegrationTests.class.getSimpleName();
		}

		Properties gemfireProperties() {
			Properties gemfireProperties = new Properties();

			gemfireProperties.setProperty("name", applicationName());
			gemfireProperties.setProperty("mcast-port", "0");
			gemfireProperties.setProperty("log-level", "warning");

			return gemfireProperties;
		}

		@Bean
		CacheFactoryBean gemfireCache() {
			CacheFactoryBean gemfireCache = new CacheFactoryBean();

			gemfireCache.setClose(true);
			gemfireCache.setProperties(gemfireProperties());

			return gemfireCache;
		}

		@Bean(name = "simple")
		LocalRegionFactoryBean<Long, Person> simpleRegion(Cache gemfireCache,
				RegionAttributes<Long, Person> simpleRegionAttributes) {

			LocalRegionFactoryBean<Long, Person> simpleRegion = new LocalRegionFactoryBean<Long, Person>();

			simpleRegion.setAttributes(simpleRegionAttributes);
			simpleRegion.setCache(gemfireCache);
			simpleRegion.setClose(false);
			simpleRegion.setPersistent(false);

			return simpleRegion;
		}

		@Bean
		@SuppressWarnings("unchecked")
		RegionAttributesFactoryBean simpleRegionAttributes() {
			RegionAttributesFactoryBean simpleRegionAttributes = new RegionAttributesFactoryBean();

			simpleRegionAttributes.setKeyConstraint(Long.class);
			simpleRegionAttributes.setValueConstraint(Person.class);

			return simpleRegionAttributes;
		}
	}
}
