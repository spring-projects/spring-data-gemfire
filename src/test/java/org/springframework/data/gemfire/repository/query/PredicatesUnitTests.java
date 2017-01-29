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
 */
package org.springframework.data.gemfire.repository.query;

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;

import org.junit.Test;
import org.springframework.data.gemfire.repository.query.Predicates.AtomicPredicate;
import org.springframework.data.repository.query.parser.Part;

/**
 * Unit tests for {@link Predicates}.
 *
 * @author Oliver Gierke
 * @author John Blum
 */
@SuppressWarnings("unused")
public class PredicatesUnitTests {

	@Test
	public void atomicPredicateDefaultsAlias() {
		Part part = new Part("firstname", Person.class);

		Iterator<Integer> indexes = Collections.singletonList(1).iterator();

		Predicate predicate = new AtomicPredicate(part, indexes);

		assertThat(predicate.toString(null), is("x.firstname = $1"));
	}

	@Test
	public void concatenatesAndPredicateCorrectly() {
		Part left = new Part("firstname", Person.class);
		Part right = new Part("lastname", Person.class);

		Iterator<Integer> indexes = Arrays.asList(1, 2).iterator();

		Predicate predicate = Predicates.create(left, indexes).and(Predicates.create(right, indexes));

		assertThat(predicate, is(notNullValue(Predicate.class)));
		assertThat(predicate.toString(null), is("x.firstname = $1 AND x.lastname = $2"));
	}

	@Test
	public void concatenatesOrPredicateCorrectly() {
		Part left = new Part("firstname", Person.class);
		Part right = new Part("lastname", Person.class);

		Iterator<Integer> indexes = Arrays.asList(1, 2).iterator();

		Predicate predicate = Predicates.create(left, indexes).or(Predicates.create(right, indexes));

		assertThat(predicate, is(notNullValue(Predicate.class)));
		assertThat(predicate.toString(null), is(equalTo("x.firstname = $1 OR x.lastname = $2")));
	}

	@Test
	public void handlesBooleanBasedPredicateCorrectly() {
		Part part = new Part("activeTrue", User.class);

		Iterator<Integer> indexes = Collections.singletonList(1).iterator();

		Predicates predicate = Predicates.create(part, indexes);

		assertThat(predicate, is(notNullValue(Predicate.class)));
		assertThat(predicate.toString("user"), is(equalTo("user.active = true")));
	}

	/**
	 * @link https://jira.spring.io/browse/SGF-507
	 */
	@Test
	public void handlesIgnoreCasePredicateCorrectly() {
		Part left = new Part("firstnameIgnoreCase", Person.class);
		Part right = new Part("lastnameIgnoreCase", Person.class);

		Iterator<Integer> indexes = Arrays.asList(1, 2).iterator();

		Predicate predicate = Predicates.create(left, indexes).and(Predicates.create(right, indexes));

		assertThat(predicate, is(notNullValue(Predicate.class)));
		assertThat(predicate.toString("person"), is(equalTo("person.firstname.equalsIgnoreCase($1) AND person.lastname.equalsIgnoreCase($2)")));
	}

	static class Person {
		String firstname;
		String lastname;
	}

	// TODO refactor Person to include boolean state; remove User
	static class User {
		Boolean active;
		String username;
	}
}
