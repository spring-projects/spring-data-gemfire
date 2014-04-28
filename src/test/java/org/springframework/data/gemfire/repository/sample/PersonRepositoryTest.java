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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Sort;
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
@ContextConfiguration
@RunWith(SpringJUnit4ClassRunner.class)
@SuppressWarnings("unused")
public class PersonRepositoryTest {

	protected final AtomicLong ID_SEQUENCE = new AtomicLong(0l);

	private Person cookieDoe = createPerson("Cookie", "Doe");
	private Person janeDoe = createPerson("Jane", "Doe");
	private Person jonDoe = createPerson("Jon", "Doe");
	private Person pieDoe = createPerson("Pie", "Doe");
	private Person jackHandy = createPerson("Jack", "Handy");
	private Person sandyHandy = createPerson("Sandy", "Handy");
	private Person imaPigg = createPerson("Ima", "Pigg");

	@Autowired
	private PersonRepository personRepo;

	@Before
	public void setup() {
		if (personRepo.count() == 0) {
			sandyHandy = personRepo.save(sandyHandy);
			jonDoe = personRepo.save(jonDoe);
			jackHandy = personRepo.save(jackHandy);
			janeDoe = personRepo.save(janeDoe);
			pieDoe = personRepo.save(pieDoe);
			imaPigg = personRepo.save(imaPigg);
			cookieDoe = personRepo.save(cookieDoe);
		}

		assertEquals(7l, personRepo.count());
	}

	protected Person createPerson(final String firstName, final String lastName) {
		return new Person(ID_SEQUENCE.incrementAndGet(), firstName, lastName);
	}

	protected Sort.Order createOrder(final String property) {
		return createOrder(property, Sort.Direction.ASC);
	}

	protected Sort.Order createOrder(final String property, final Sort.Direction direction) {
		return new Sort.Order(direction, property);
	}

	protected Sort createSort(final Sort.Order... orders) {
		return new Sort(orders);
	}

	@Test
	public void testFindDistinctPeopleWithOrder() {
		List<Person> actualPeople = personRepo.findDistinctPeopleByOrderByLastnameDesc(
			createSort(createOrder("firstname")));

		assertNotNull(actualPeople);
		assertFalse(actualPeople.isEmpty());
		assertEquals(7, actualPeople.size());
		assertEquals(Arrays.asList(imaPigg, jackHandy, sandyHandy, cookieDoe, janeDoe, jonDoe, pieDoe), actualPeople);
	}

	@Test
	public void testFindDistinctPersonWithNoOrder() {
		List<Person> actualPeople = personRepo.findDistinctByLastname("Pigg", null);

		assertNotNull(actualPeople);
		assertFalse(actualPeople.isEmpty());
		assertEquals(1, actualPeople.size());
		assertEquals(String.format("Expected '%1$s'; but was '%2$s'", imaPigg, actualPeople.get(0)),
			imaPigg, actualPeople.get(0));
	}

}
