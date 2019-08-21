/*
 * Copyright 2017-2019 the original author or authors.
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

package org.springframework.data.gemfire.repository.query;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicLong;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.client.ClientRegionShortcut;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.data.gemfire.client.ClientRegionFactoryBean;
import org.springframework.data.gemfire.config.annotation.ClientCacheApplication;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.gemfire.repository.sample.PersonRepository;
import org.springframework.data.gemfire.repository.sample.User;
import org.springframework.data.gemfire.repository.sample.UserRepository;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean;
import org.springframework.data.repository.Repository;
import org.springframework.data.repository.query.QueryMethod;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * The QueryPostProcessorIntegrationTests class...
 *
 * @author John Blum
 * @since 1.0.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class QueryPostProcessorIntegrationTests {

	private static final AtomicLong idSequence = new AtomicLong(0L);

	@Autowired
	private FindOrderedLimitedPeopleByFirstNameQueryPostProcessor peopleQueryPostProcessor;

	@Autowired
	private PersonRepository personRepository;

	@Autowired
	private RecordingQueryPostProcessor recordingQueryPostProcessor;

	@Autowired
	private UserRepository userRepository;

	private static Person newPerson(String firstName, String lastName) {

		Person person = new Person(firstName, lastName);

		person.id = idSequence.incrementAndGet();

		return person;
	}

	@Before
	public void setup() {

		this.personRepository.save(newPerson("Jon", "Doe"));
		this.personRepository.save(newPerson("Cookie", "Doe"));
		this.personRepository.save(newPerson("Pie", "Doe"));
		this.personRepository.save(newPerson("Sour", "Doe"));
		this.personRepository.save(newPerson("Jack", "BeNimble"));
		this.personRepository.save(newPerson("Jack", "BeQuick"));
		this.personRepository.save(newPerson("Jack", "Black"));
		this.personRepository.save(newPerson("Jack", "JumpedOverTheCandleStick"));
		this.personRepository.save(newPerson("Jack", "Handy"));
		this.personRepository.save(newPerson("Jack", "Sparrow"));
		this.personRepository.save(newPerson("Agent", "Smith"));

		this.userRepository.save(new User("abuser"));
		this.userRepository.save(new User("jdoe"));
		this.userRepository.save(new User("root"));
	}

	@Test
	public void queryPostProcessingProcessesGeneratedOqlQueries() {

		assertThat(this.personRepository.count()).isEqualTo(11);
		assertThat(this.userRepository.count()).isEqualTo(3);
		assertThat(this.recordingQueryPostProcessor.queries).isEmpty();

		List<User> users = this.userRepository.findDistinctByUsernameLike("%doe");

		assertThat(users).hasSize(1);
		assertThat(this.recordingQueryPostProcessor.queries).hasSize(1);
		assertThat(this.recordingQueryPostProcessor.queries)
			.containsExactly("SELECT DISTINCT * FROM /Users x WHERE x.username LIKE $1");

		Collection<Person> bakingDoes = this.personRepository.findByFirstnameIn("Cookie", "Pie", "Sour");

		assertThat(bakingDoes).hasSize(3);
		assertThat(this.recordingQueryPostProcessor.queries).hasSize(2);
		assertThat(this.recordingQueryPostProcessor.queries).containsExactly(
			"SELECT DISTINCT * FROM /Users x WHERE x.username LIKE $1",
			"SELECT * FROM /simple x WHERE x.firstname IN SET ('Cookie', 'Pie', 'Sour')"
		);

		Collection<Person> jacks = this.personRepository.findByFirstname("Jack");

		assertThat(jacks).hasSize(1);
		assertThat(jacks.stream().findFirst().map(Person::getName).orElse(null)).isEqualTo("Jack Sparrow");

		assertThat(this.recordingQueryPostProcessor.queries).hasSize(3);
		assertThat(this.recordingQueryPostProcessor.queries).containsExactly(
			"SELECT DISTINCT * FROM /Users x WHERE x.username LIKE $1",
			"SELECT * FROM /simple x WHERE x.firstname IN SET ('Cookie', 'Pie', 'Sour')",
			"SELECT DISTINCT * FROM /simple x WHERE x.firstname = $1 ORDER BY lastname DESC LIMIT 1"
		);
	}

	@ClientCacheApplication(logLevel = "error")
	@SuppressWarnings("unused")
	static class TestConfiguration {

		@Bean("simple")
		public ClientRegionFactoryBean<Object, Object> peopleRegion(GemFireCache gemfireCache) {

			ClientRegionFactoryBean<Object, Object> clientRegion = new ClientRegionFactoryBean<>();

			clientRegion.setCache(gemfireCache);
			clientRegion.setClose(false);
			clientRegion.setShortcut(ClientRegionShortcut.LOCAL);

			return clientRegion;
		}

		@Bean("Users")
		public ClientRegionFactoryBean<Object, Object> usersRegion(GemFireCache gemfireCache) {

			ClientRegionFactoryBean<Object, Object> clientRegion = new ClientRegionFactoryBean<>();

			clientRegion.setCache(gemfireCache);
			clientRegion.setClose(false);
			clientRegion.setShortcut(ClientRegionShortcut.LOCAL);

			return clientRegion;
		}

		@Bean
		GemfireMappingContext mappingContext() {
			return new GemfireMappingContext();
		}

		@Bean
		GemfireRepositoryFactoryBean<PersonRepository, Person, Long> personRepository() {
			return new GemfireRepositoryFactoryBean<>(PersonRepository.class);
		}

		@Bean
		GemfireRepositoryFactoryBean<UserRepository, User, String> userRepository() {
			return new GemfireRepositoryFactoryBean<>(UserRepository.class);
		}

		@Bean
		FindOrderedLimitedPeopleByFirstNameQueryPostProcessor personQueryPostProcess() {
			return new FindOrderedLimitedPeopleByFirstNameQueryPostProcessor(1);
		}

		@Bean
		RecordingQueryPostProcessor recordingQueryPostProcessor() {
			return new RecordingQueryPostProcessor();
		}
	}

	static class FindOrderedLimitedPeopleByFirstNameQueryPostProcessor implements QueryPostProcessor<PersonRepository, String> {

		private final int limit;

		FindOrderedLimitedPeopleByFirstNameQueryPostProcessor(int limit) {
			this.limit = limit;
		}

		@Override
		public int getOrder() {
			return 0;
		}

		@Override
		public String postProcess(QueryMethod queryMethod, String query, Object... arguments) {

			return "findByFirstname".equals(queryMethod.getName())
				? query.trim().replace("SELECT", "SELECT DISTINCT")
					.concat(" ORDER BY lastname DESC").concat(String.format(" LIMIT %d", this.limit))
				: query;
		}
	}

	static class RecordingQueryPostProcessor implements QueryPostProcessor<Repository, String> {

		List<String> queries = new CopyOnWriteArrayList<>();

		@Override
		public int getOrder() {
			return 1;
		}

		@Override
		public String postProcess(QueryMethod queryMethod, String query, Object... arguments) {
			this.queries.add(query);
			return query;
		}
	}
}
