/*
 * Copyright 2012 the original author or authors.
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

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;

import org.junit.Test;
import org.springframework.data.gemfire.repository.query.Predicates.AtomicPredicate;
import org.springframework.data.repository.query.parser.Part;

/**
 * 
 * @author Oliver Gierke
 * @author John Blum
 */
@SuppressWarnings("unused")
public class PredicatesUnitTests {

	@Test
	public void atomicPredicateDefaultsAlias() {

		Part part = new Part("firstname", Person.class);

		Iterable<Integer> indexes = Arrays.asList(1);

		Predicate predicate = new AtomicPredicate(part, indexes.iterator());
		assertThat(predicate.toString(null), is("x.firstname = $1"));
	}

	@Test
	public void concatenatesAndPredicateCorrectly() {

		Part left = new Part("firstname", Person.class);
		Part right = new Part("lastname", Person.class);
		Iterator<Integer> indexes = Arrays.asList(1, 2).iterator();

		Predicates predicate = Predicates.create(left, indexes);
		predicate = predicate.and(new AtomicPredicate(right, indexes));

		assertThat(predicate.toString(null), is("x.firstname = $1 AND x.lastname = $2"));
	}

	@Test
	public void concatenatesOrPredicateCorrectly() {

		Part left = new Part("firstname", Person.class);
		Part right = new Part("lastname", Person.class);
		Iterator<Integer> indexes = Arrays.asList(1, 2).iterator();

		Predicates predicate = Predicates.create(left, indexes);
		predicate = predicate.or(new AtomicPredicate(right, indexes));

		assertThat(predicate.toString(null), is("x.firstname = $1 OR x.lastname = $2"));
	}

	@Test
	public void testBooleanBasedPredicate() {
		Part part = new Part("activeTrue", User.class);
		Iterator<Integer> indexes = Collections.<Integer>emptyList().iterator();
		Predicates predicate = Predicates.create(part, indexes);

		assertNotNull(predicate);
		assertThat(predicate.toString("user"), is("user.active = true"));
	}

	static class Person {
		String firstname;
		String lastname;
	}

	static class User {
		Boolean active;
		String username;
	}

}
