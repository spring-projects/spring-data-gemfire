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

package org.springframework.data.gemfire.repository.cdi;

import javax.enterprise.inject.se.SeContainer;
import javax.enterprise.inject.se.SeContainerInitializer;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import org.apache.geode.cache.CacheClosedException;
import org.apache.geode.cache.CacheFactory;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.data.gemfire.repository.sample.Person;

/**
 * The CdiExtensionIntegrationTest class...
 *
 * @author John Blum
 * @author Mark Paluch
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.repository.cdi.GemfireRepositoryBean
 * @see org.springframework.data.gemfire.repository.cdi.GemfireRepositoryExtension
 * @since 1.8.0
 */
public class CdiExtensionIntegrationTest {

	static SeContainer container;

	@BeforeClass
	public static void setUp() {

		container = SeContainerInitializer.newInstance() //
				.disableDiscovery() //
				.addPackages(RepositoryClient.class) //
				.initialize();
	}

	@AfterClass
	public static void tearDown() {
		container.close();
		closeGemfireCache();
	}

	private static void closeGemfireCache() {
		try {
			CacheFactory.getAnyInstance().close();
		}
		catch (CacheClosedException ignore) {
		}
	}

	protected void assertIsExpectedPerson(Person actual, Person expected) {
		assertThat(actual.getId(), is(equalTo(expected.getId())));
		assertThat(actual.getFirstname(), is(equalTo(expected.getFirstname())));
		assertThat(actual.getLastname(), is(equalTo(expected.getLastname())));
	}

	@Test // SGF-669
	public void bootstrapsRepositoryCorrectly() {

		RepositoryClient repositoryClient = container.select(RepositoryClient.class).get();

		assertThat(repositoryClient.getPersonRepository(), is(notNullValue()));

		Person expectedJonDoe = repositoryClient.newPerson("Jon", "Doe");

		assertThat(expectedJonDoe, is(notNullValue()));
		assertThat(expectedJonDoe.getId(), is(greaterThan(0L)));
		assertThat(expectedJonDoe.getName(), is(equalTo("Jon Doe")));

		Person savedJonDoe = repositoryClient.save(expectedJonDoe);

		assertIsExpectedPerson(savedJonDoe, expectedJonDoe);

		Person foundJonDoe = repositoryClient.find(expectedJonDoe.getId());

		assertIsExpectedPerson(foundJonDoe, expectedJonDoe);

		assertThat(repositoryClient.delete(foundJonDoe), is(true));
		assertThat(repositoryClient.find(foundJonDoe.getId()), is(nullValue()));
	}

	@Test // SGF-669
	public void returnOneFromCustomImplementation() {

		RepositoryClient repositoryClient = container.select(RepositoryClient.class).get();

		assertThat(repositoryClient.getPersonRepository().returnOne(), is(equalTo(1)));
	}

}
