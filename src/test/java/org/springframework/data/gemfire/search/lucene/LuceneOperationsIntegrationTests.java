/*
 * Copyright 2016-2019 the original author or authors.
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
package org.springframework.data.gemfire.search.lucene;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.Month;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicLong;
import java.util.stream.Collectors;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.lucene.LuceneService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.DependsOn;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.EventListener;
import org.springframework.data.annotation.Id;
import org.springframework.data.gemfire.PartitionedRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.PeerCacheApplication;
import org.springframework.data.gemfire.util.SpringUtils;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * Integration tests for the Spring Data Geode, Apache Geode and Apache Lucene Integration.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see lombok
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.lucene.LuceneIndex
 * @see org.springframework.data.gemfire.config.annotation.PeerCacheApplication
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 1.1.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class LuceneOperationsIntegrationTests {

	private static final AtomicLong IDENTIFIER = new AtomicLong(0L);

	protected static final String GEMFIRE_LOG_LEVEL = "none";

	private static Person jonDoe = Person.newPerson(LocalDate.of(1969, Month.JULY, 4), "Jon", "Doe").with("Master of Science");
	private static Person janeDoe = Person.newPerson(LocalDate.of(1969, Month.AUGUST, 16), "Jane", "Doe").with("Doctor of Astrophysics");
	private static Person cookieDoe = Person.newPerson(LocalDate.of(1991, Month.APRIL, 2), "Cookie", "Doe").with("Bachelor of Physics");
	private static Person froDoe = Person.newPerson(LocalDate.of(1988, Month.MAY, 25), "Fro", "Doe").with("Doctor of Computer Science");
	private static Person hoDoe = Person.newPerson(LocalDate.of(1984, Month.NOVEMBER, 11), "Ho", "Doe").with("Doctor of Math");
	private static Person pieDoe = Person.newPerson(LocalDate.of(1996, Month.JUNE, 4), "Pie", "Doe").with("Master of Astronomy");
	private static Person sourDoe = Person.newPerson(LocalDate.of(1999, Month.DECEMBER, 1), "Sour", "Doe").with("Bachelor of Art");

	private static List<String> asNames(List<? extends Nameable> nameables) {

		return nameables.stream()
			.map(Nameable::getName)
			.collect(Collectors.toList());
	}

	private static List<User> asUsers(Person... people) {

		return Arrays.stream(people)
			.map(User::from)
			.collect(Collectors.toList());
	}

	@Autowired
	private ProjectingLuceneOperations template;

	@Test
	public void findsDoctorDoesAsTypePersonSuccessfully() {

		Collection<Person> doctorDoes = template.queryForValues("title: Doctor*", "title");

		assertThat(doctorDoes).isNotNull();
		assertThat(doctorDoes).hasSize(3);
		assertThat(doctorDoes).contains(janeDoe, froDoe, hoDoe);
	}

	@Test
	@SuppressWarnings("all")
	public void findsMasterDoesAsTypeUserSuccessfully() {

		List<User> masterDoes = template.query("title: Master*", "title", User.class);

		assertThat(masterDoes).isNotNull();
		assertThat(masterDoes).hasSize(2);
		assertThat(masterDoes.stream().allMatch(user -> user instanceof User)).isTrue();
		assertThat(asNames(masterDoes)).containsAll(asNames(asUsers(jonDoe, pieDoe)));
	}

	@SuppressWarnings("unused")
	@PeerCacheApplication(name = "LuceneOperationsIntegrationTests", logLevel = GEMFIRE_LOG_LEVEL)
	static class TestConfiguration {

		@Bean(name = "People")
		@DependsOn("personTitleIndex")
		PartitionedRegionFactoryBean<Long, Person> peopleRegion(GemFireCache gemfireCache) {

			PartitionedRegionFactoryBean<Long, Person> peopleRegion = new PartitionedRegionFactoryBean<>();

			peopleRegion.setCache(gemfireCache);
			peopleRegion.setClose(false);
			peopleRegion.setPersistent(false);

			return peopleRegion;
		}

		@Bean
		LuceneServiceFactoryBean luceneService(GemFireCache gemfireCache) {

			LuceneServiceFactoryBean luceneService = new LuceneServiceFactoryBean();

			luceneService.setCache(gemfireCache);

			return luceneService;
		}

		@Bean
		LuceneIndexFactoryBean personTitleIndex(GemFireCache gemfireCache) {

			LuceneIndexFactoryBean luceneIndex = new LuceneIndexFactoryBean();

			luceneIndex.setCache(gemfireCache);
			luceneIndex.setFields("title");
			luceneIndex.setIndexName("PersonTitleIndex");
			luceneIndex.setRegionPath("/People");

			return luceneIndex;
		}

		@Bean
		@DependsOn("personTitleIndex")
		ProjectingLuceneOperations luceneTemplate() {
			return new ProjectingLuceneTemplate("PersonTitleIndex", "/People");
		}

		@EventListener(ContextRefreshedEvent.class)
		@SuppressWarnings("unchecked")
		public void loadPeople(ContextRefreshedEvent event) {

			Region<Long, Person> people = event.getApplicationContext().getBean("People", Region.class);

			Arrays.asList(jonDoe, janeDoe, cookieDoe, froDoe, hoDoe, pieDoe, sourDoe)
				.forEach(person -> {
					person.setId(IDENTIFIER.incrementAndGet());
					people.put(person.getId(), person);
				});

			LuceneService luceneService =
				event.getApplicationContext().getBean("luceneService", LuceneService.class);

			SpringUtils.safeRunOperation(() ->
				luceneService.waitUntilFlushed("PersonTitleIndex", "/People", 15L, TimeUnit.SECONDS));
		}
	}

	interface Nameable {
		String getName();
	}

	@Data
	@RequiredArgsConstructor(staticName = "newPerson")
	static class Person implements Nameable, Serializable {

		@Id
		Long id;

		@NonNull LocalDate birthDate;

		@NonNull String firstName;
		@NonNull String lastName;

		String title;

		public String getName() {
			return String.format("%1$s %2$s", getFirstName(), getLastName());
		}

		Person with(String title) {
			setTitle(title);
			return this;
		}
	}

	interface User extends Nameable {
		static User from(Person person) {
			return person::getName;
		}
	}
}
