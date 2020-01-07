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

import java.util.Collections;
import java.util.Optional;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;

import org.apache.geode.cache.query.CqEvent;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.data.gemfire.listener.annotation.ContinuousQuery;
import org.springframework.data.gemfire.process.ProcessWrapper;
import org.springframework.data.gemfire.repository.config.EnableGemfireRepositories;
import org.springframework.data.gemfire.support.ConnectionEndpoint;
import org.springframework.data.gemfire.test.model.Gender;
import org.springframework.data.gemfire.test.model.Person;
import org.springframework.data.gemfire.test.repo.PersonRepository;
import org.springframework.data.gemfire.test.support.ClientServerIntegrationTestsSupport;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Integration tests testing the combination of {@link EnableContinuousQueries} with {@link EnableClusterConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.query.CqEvent
 * @see org.springframework.data.gemfire.config.annotation.EnableClusterConfiguration
 * @see org.springframework.data.gemfire.config.annotation.EnableContinuousQueries
 * @see org.springframework.data.gemfire.listener.annotation.ContinuousQuery
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @see <a href="https://jira.spring.io/browse/DATAGEODE-73">Fix race condition between ContinuousQuery registration and EnableClusterConfiguration Region creation.</a>
 * @since 2.0.3
 */
@RunWith(SpringRunner.class)
@ContextConfiguration(classes = EnableContinuousQueriesWithClusterConfigurationIntegrationTests.TestConfiguration.class)
@SuppressWarnings("unused")
public class EnableContinuousQueriesWithClusterConfigurationIntegrationTests
		extends ClientServerIntegrationTestsSupport {

	private static final BlockingQueue<Person> events = new ArrayBlockingQueue<>(2);

	private static ProcessWrapper gemfireServer;

	@BeforeClass
	public static void startGemFireServer() throws Exception {

		int availablePort = findAvailablePort();

		gemfireServer = run(EnableContinuousQueriesWithClusterConfigurationIntegrationTests.GemFireServerConfiguration.class,
			String.format("-D%s=%d", GEMFIRE_CACHE_SERVER_PORT_PROPERTY, availablePort));

		waitForServerToStart("localhost", availablePort);

		System.setProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY, String.valueOf(availablePort));
	}

	@AfterClass
	public static void stopGemFireServer() {
		System.clearProperty(GEMFIRE_CACHE_SERVER_PORT_PROPERTY);
		stop(gemfireServer);
	}

	@Autowired
	private PersonRepository personRepository;

	@Test
	public void personEventsFired() throws Exception {

		Person jonDoe = Person.newPerson(1L, "Jon", "Doe", null, Gender.MALE);

		jonDoe = this.personRepository.save(jonDoe);

		assertThat(this.personRepository.findById(jonDoe.getId()).orElse(null)).isEqualTo(jonDoe);
		assertThat(events.poll(5L, TimeUnit.SECONDS)).isEqualTo(jonDoe);

		Person janeDoe = Person.newPerson(2L, "Jane", "Doe", null, Gender.FEMALE);

		janeDoe = this.personRepository.save(janeDoe);

		assertThat(this.personRepository.findById(janeDoe.getId()).orElse(null)).isEqualTo(janeDoe);
		assertThat(events.poll(5L, TimeUnit.SECONDS)).isEqualTo(janeDoe);
	}

	@Configuration
	@EnableContinuousQueries
	@Import(GemFireClientConfiguration.class)
	static class TestConfiguration {

		@ContinuousQuery(name = "PersonEvents", query = "SELECT * FROM /People")
		public void peopleEventHandler(CqEvent event) {

			Optional.ofNullable(event)
				.map(CqEvent::getNewValue)
				.filter(newValue -> newValue instanceof Person)
				.map(newValue -> (Person) newValue)
				.ifPresent(events::offer);
		}
	}

	@ClientCacheApplication(logLevel = "error", subscriptionEnabled = true)
	@EnableClusterConfiguration
	@EnableEntityDefinedRegions(basePackageClasses = Person.class)
	@EnableGemfireRepositories(basePackageClasses = PersonRepository.class)
	static class GemFireClientConfiguration {

		@Bean
		ClientCacheConfigurer clientCachePoolPortConfigurer(
				@Value("${" + GEMFIRE_CACHE_SERVER_PORT_PROPERTY + ":40404}") int port) {

			return (bean, clientCacheFactoryBean) -> clientCacheFactoryBean.setServers(
				Collections.singletonList(new ConnectionEndpoint("localhost", port)));
		}
	}

	@CacheServerApplication(name = "EnableContinuousQueriesWithClusterConfigurationIntegrationTests", logLevel = "error")
	static class GemFireServerConfiguration {

		public static void main(String[] args) {

			AnnotationConfigApplicationContext applicationContext =
				new AnnotationConfigApplicationContext(GemFireServerConfiguration.class);

			applicationContext.registerShutdownHook();
		}
	}
}
