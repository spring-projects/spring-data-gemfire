/*
 * Copyright 2010-2019 the original author or authors.
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

import static org.assertj.core.api.Assertions.assertThat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicLong;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.RegionAttributes;
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
 * The PersonRepositoryIntegrationTests class...
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
@ContextConfiguration(classes = PersonRepositoryIntegrationTests.GemFireConfiguration.class)
@SuppressWarnings("unused")
public class PersonRepositoryIntegrationTests {

	private static final String DEFAULT_GEMFIRE_LOG_LEVEL = "warning";
	private static final String GEMFIRE_LOG_LEVEL = System.getProperty("gemfire.log-level", DEFAULT_GEMFIRE_LOG_LEVEL);

	protected final AtomicLong ID_SEQUENCE = new AtomicLong(0L);

	private Person cookieDoe = newPerson("Cookie", "Doe");
	private Person janeDoe = newPerson("Jane", "Doe");
	private Person jonDoe = newPerson("Jon", "Doe");
	private Person pieDoe = newPerson("Pie", "Doe");
	private Person sourDoe = newPerson("Sour", "Doe");
	private Person jackHandy = newPerson("Jack", "Handy");
	private Person sandyHandy = newPerson("Sandy", "Handy");
	private Person imaPigg = newPerson("Ima", "Pigg");

	@Autowired
	private PersonRepository personRepository;

	@Before
	public void setup() {
		if (personRepository.count() == 0) {
			sourDoe = personRepository.save(sourDoe);
			sandyHandy = personRepository.save(sandyHandy);
			jonDoe = personRepository.save(jonDoe);
			jackHandy = personRepository.save(jackHandy);
			janeDoe = personRepository.save(janeDoe);
			pieDoe = personRepository.save(pieDoe);
			imaPigg = personRepository.save(imaPigg);
			cookieDoe = personRepository.save(cookieDoe);
		}

		assertThat(personRepository.count()).isEqualTo(8L);
	}

	protected <T> List<T> asList(Iterable<T> iterable) {
		List<T> list = new ArrayList<T>();

		for (T element : iterable) {
			list.add(element);
		}

		return list;
	}

	protected Person newPerson(String firstName, String lastName) {
		return new Person(ID_SEQUENCE.incrementAndGet(), firstName, lastName);
	}

	protected Sort.Order newSortOrder(String property) {
		return newSortOrder(property, Sort.Direction.ASC);
	}

	protected Sort.Order newSortOrder(String property, Sort.Direction direction) {
		return new Sort.Order(direction, property);
	}

	protected Sort newSort(Sort.Order... orders) {
		return new Sort(orders);
	}

	@Test
	public void findAllPeopleSorted() {
		Iterable<Person> people = personRepository.findAll(newSort(newSortOrder("firstname")));

		assertThat(people).isNotNull();

		List<Person> peopleList = asList(people);

		assertThat(peopleList.size()).isEqualTo(8);
		assertThat(peopleList).isEqualTo(
			Arrays.asList(cookieDoe, imaPigg, jackHandy, janeDoe, jonDoe, pieDoe, sandyHandy, sourDoe));
	}

	@Test
	public void findDistinctPeopleOrderedByLastnameDescendingFirstnameAscending() {
		List<Person> actualPeople = personRepository.findDistinctPeopleByOrderByLastnameDesc(
			newSort(newSortOrder("firstname")));

		assertThat(actualPeople).isNotNull();
		assertThat(actualPeople.size()).isEqualTo(8);
		assertThat(actualPeople).isEqualTo(Arrays.asList(
			imaPigg, jackHandy, sandyHandy, cookieDoe, janeDoe, jonDoe, pieDoe, sourDoe));
	}

	@Test
	public void findDistinctPeopleByLastnameUnordered() {
		List<Person> actualPeople = personRepository.findDistinctByLastname("Handy", null);

		assertThat(actualPeople).isNotNull();
		assertThat(actualPeople.size()).isEqualTo(2);
		assertThat(actualPeople).containsAll(Arrays.asList(jackHandy, sandyHandy));
	}

	@Test
	public void findDistinctPeopleByFirstOrLastNameWithSort() {
		Collection<Person> people = personRepository.findDistinctByFirstnameOrLastname("Cookie", "Pigg",
			newSort(newSortOrder("lastname", Sort.Direction.DESC), newSortOrder("firstname", Sort.Direction.ASC)));

		assertThat(people).isNotNull();
		assertThat(people.size()).isEqualTo(2);

		Iterator<Person> peopleIterator = people.iterator();

		assertThat(peopleIterator.hasNext()).isTrue();
		assertThat(peopleIterator.next()).isEqualTo(imaPigg);
		assertThat(peopleIterator.hasNext()).isTrue();
		assertThat(peopleIterator.next()).isEqualTo(cookieDoe);
		assertThat(peopleIterator.hasNext()).isFalse();
	}

	@Test
	public void findPersonByFirstAndLastNameIgnoringCase() {
		Collection<Person> people = personRepository.findByFirstnameIgnoreCaseAndLastnameIgnoreCase("jON", "doE");

		assertThat(people).isNotNull();
		assertThat(people.size()).isEqualTo(1);
		assertThat(people.iterator().next()).isEqualTo(jonDoe);
	}

	@Test
	public void findByFirstAndLastNameAllIgnoringCase() {
		Collection<Person> people = personRepository.findByFirstnameAndLastnameAllIgnoringCase("IMa", "PIGg");

		assertThat(people).isNotNull();
		assertThat(people.size()).isEqualTo(1);
		assertThat(people.iterator().next()).isEqualTo(imaPigg);
	}

	@Configuration
	@EnableGemfireRepositories(basePackages = "org.springframework.data.gemfire.repository.sample",
		includeFilters = @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE,
			value = org.springframework.data.gemfire.repository.sample.PersonRepository.class))
	public static class GemFireConfiguration {

		Properties gemfireProperties() {
			Properties gemfireProperties = new Properties();

			gemfireProperties.setProperty("name", applicationName());
			gemfireProperties.setProperty("mcast-port", "0");
			gemfireProperties.setProperty("locators", "");
			gemfireProperties.setProperty("log-level", logLevel());

			return gemfireProperties;
		}

		String applicationName() {
			return PersonRepositoryIntegrationTests.class.getSimpleName();
		}

		String logLevel() {
			return GEMFIRE_LOG_LEVEL;
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
