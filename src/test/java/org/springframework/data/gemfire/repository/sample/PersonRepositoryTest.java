/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire.repository.sample;

import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicLong;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.RegionAttributes;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.FilterType;
import org.springframework.data.domain.Sort;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.LocalRegionFactoryBean;
import org.springframework.data.gemfire.RegionAttributesFactoryBean;
import org.springframework.data.gemfire.repository.config.EnableGemfireRepositories;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The PersonRepositoryTest class...
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.data.gemfire.repository.sample.Person
 * @see org.springframework.data.gemfire.repository.sample.PersonRepository
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.4.0
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = PersonRepositoryTest.GemFireConfiguration.class)
@SuppressWarnings("unused")
public class PersonRepositoryTest {

	private static final String GEMFIRE_LOG_LEVEL = System.getProperty("gemfire.log-level", "warning");

	protected final AtomicLong ID_SEQUENCE = new AtomicLong(0l);

	private Person cookieDoe = newPerson("Cookie", "Doe");
	private Person janeDoe = newPerson("Jane", "Doe");
	private Person jonDoe = newPerson("Jon", "Doe");
	private Person pieDoe = newPerson("Pie", "Doe");
	private Person jackHandy = newPerson("Jack", "Handy");
	private Person sandyHandy = newPerson("Sandy", "Handy");
	private Person imaPigg = newPerson("Ima", "Pigg");

	@Autowired
	private PersonRepository personRepository;

	@Before
	public void setup() {
		if (personRepository.count() == 0) {
			sandyHandy = personRepository.save(sandyHandy);
			jonDoe = personRepository.save(jonDoe);
			jackHandy = personRepository.save(jackHandy);
			janeDoe = personRepository.save(janeDoe);
			pieDoe = personRepository.save(pieDoe);
			imaPigg = personRepository.save(imaPigg);
			cookieDoe = personRepository.save(cookieDoe);
		}

		assertThat(personRepository.count(), is(equalTo(7L)));
	}

	protected Person newPerson(String firstName, String lastName) {
		return new Person(ID_SEQUENCE.incrementAndGet(), firstName, lastName);
	}

	protected Sort.Order newOrder(String property) {
		return newOrder(property, Sort.Direction.ASC);
	}

	protected Sort.Order newOrder(String property, Sort.Direction direction) {
		return new Sort.Order(direction, property);
	}

	protected Sort newSort(Sort.Order... orders) {
		return new Sort(orders);
	}

	@Test
	public void findDistinctPeopleOrderedByFirstnameDescending() {
		List<Person> actualPeople = personRepository.findDistinctPeopleByOrderByLastnameDesc(
			newSort(newOrder("firstname")));

		assertThat(actualPeople, is(notNullValue(List.class)));
		assertThat(actualPeople.size(), is(equalTo(7)));
		assertThat(actualPeople, is(equalTo(Arrays.asList(
			imaPigg, jackHandy, sandyHandy, cookieDoe, janeDoe, jonDoe, pieDoe))));
	}

	@Test
	public void findDistinctPersonWithUnordered() {
		List<Person> actualPeople = personRepository.findDistinctByLastname("Handy", null);

		assertThat(actualPeople, is(notNullValue(List.class)));
		assertThat(actualPeople.size(), is(equalTo(2)));
		assertThat(String.format("Expected '%1$s'; but was '%2$s'", Arrays.asList(jackHandy, sandyHandy), actualPeople),
			actualPeople, contains(jackHandy, sandyHandy));
	}

	@Test
	public void findPersonByFirstAndLastNameIgnoringCase() {
		Collection<Person> people = personRepository.findByFirstnameIgnoreCaseAndLastnameIgnoreCase("jON", "doE");

		assertThat(people, is(notNullValue(Collection.class)));
		assertThat(people.size(), is(equalTo(1)));
		assertThat(people.iterator().next(), is(equalTo(jonDoe)));
	}

	@Configuration
	@EnableGemfireRepositories(basePackages = "org.springframework.data.gemfire.repository.sample",
		includeFilters = @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE,
			value = org.springframework.data.gemfire.repository.sample.PersonRepository.class))
	public static class GemFireConfiguration {

		String applicationName() {
			return PersonRepositoryTest.class.getSimpleName();
		}

		String logLevel() {
			return GEMFIRE_LOG_LEVEL;
		}

		Properties gemfireProperties() {
			Properties gemfireProperties = new Properties();

			gemfireProperties.setProperty("name", applicationName());
			gemfireProperties.setProperty("mcast-port", "0");
			gemfireProperties.setProperty("log-level", logLevel());

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
		LocalRegionFactoryBean simpleRegion(Cache gemfireCache, RegionAttributes<Long, Person> simpleRegionAttributes) {
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
