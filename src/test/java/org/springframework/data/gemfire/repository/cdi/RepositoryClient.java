/*
 * Copyright 2012-2018 the original author or authors.
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

import java.util.concurrent.atomic.AtomicLong;

import javax.inject.Inject;

import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.util.Assert;

/**
 * The RepositoryClient class is a user/consumer of the {@link SamplePersonRepository} bean in a CDI context.
 *
 * @author John Blum
 * @see javax.inject.Inject
 * @see org.springframework.data.gemfire.repository.cdi.SamplePersonRepository
 * @since 1.8.0
 */
public class RepositoryClient {

	private static final AtomicLong ID_SEQUENCE = new AtomicLong(0L);

	@Inject
	private SamplePersonRepository personRepository;

	protected SamplePersonRepository getPersonRepository() {
		Assert.state(personRepository != null, "PersonRepository was not properly initialized");
		return personRepository;
	}

	public Person newPerson(String firstName, String lastName) {
		return new Person(ID_SEQUENCE.incrementAndGet(), firstName, lastName);
	}

	public Person find(Long id) {
		return getPersonRepository().findById(id).orElse(null);
	}

	public Person save(Person person) {
		return getPersonRepository().save(person);
	}

	public boolean delete(Person person) {
		getPersonRepository().delete(person);
		return (find(person.getId()) == null);
	}
}
