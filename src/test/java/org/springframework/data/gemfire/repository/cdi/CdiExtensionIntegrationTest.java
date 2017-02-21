/*
 * Copyright 2012 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.repository.cdi;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

import org.apache.geode.cache.CacheClosedException;
import org.apache.geode.cache.CacheFactory;
import org.apache.webbeans.cditest.CdiTestContainer;
import org.apache.webbeans.cditest.CdiTestContainerLoader;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.data.gemfire.repository.sample.Person;

/**
 * The CdiExtensionIntegrationTest class...
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.repository.cdi.GemfireRepositoryBean
 * @see org.springframework.data.gemfire.repository.cdi.GemfireRepositoryExtension
 * @see org.apache.webbeans.cditest.CdiTestContainer
 * @see org.apache.webbeans.cditest.CdiTestContainerLoader
 * @since 1.8.0
 */
public class CdiExtensionIntegrationTest {

	static CdiTestContainer container;

	@BeforeClass
	public static void setUp() throws Exception {
		container = CdiTestContainerLoader.getCdiContainer();
		container.bootContainer();
	}

	@AfterClass
	public static void tearDown() throws Exception {
		container.shutdownContainer();
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

	@Test
	public void bootstrapsRepositoryCorrectly() {
		RepositoryClient repositoryClient = container.getInstance(RepositoryClient.class);

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

	@Test
	public void returnOneFromCustomImplementation() {
		RepositoryClient repositoryClient = container.getInstance(RepositoryClient.class);

		assertThat(repositoryClient.getPersonRepository().returnOne(), is(equalTo(1)));
	}

}
