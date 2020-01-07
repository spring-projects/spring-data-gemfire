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
 */
package org.springframework.data.gemfire.repository.sample;

import java.util.Collection;
import java.util.List;

import org.springframework.data.domain.Sort;
import org.springframework.data.gemfire.repository.GemfireRepository;
import org.springframework.data.gemfire.repository.Query;

/**
 * Sample Repository interface managing {@link Person}s.
 *
 * @author Oliver Gierke
 * @author David Turanski
 * @author John Blum
 */
public interface PersonRepository extends GemfireRepository<Person, Long> {

	@Query("SELECT * FROM /simple p WHERE p.firstname = $1")
	Collection<Person> findByFirstnameAnnotated(String firstname);

	@Query("SELECT * FROM /simple p WHERE p.firstname IN SET $1")
	Collection<Person> findByFirstnamesAnnotated(Collection<String> firstnames);

	Collection<Person> findByFirstname(String firstname);

	Collection<Person> findByFirstnameContaining(String firstName);

	Collection<Person> findByFirstnameIn(Collection<String> firstNames);

	Collection<Person> findByFirstnameIn(String... firstNames);

	Collection<Person> findByFirstnameLike(String firstName);

	Collection<Person> findByFirstnameStartingWith(String firstName);

	Collection<Person> findByFirstnameAndLastname(String firstName, String lastName);

	Collection<Person> findByFirstnameIgnoreCaseAndLastnameIgnoreCase(String firstName, String lastName);

	Collection<Person> findByFirstnameAndLastnameAllIgnoringCase(String firstName, String lastName);

	Collection<Person> findByFirstnameOrLastname(String firstName, String lastName);

	Collection<Person> findDistinctByFirstnameOrLastname(String firstName, String lastName, Sort order);

	Person findByLastname(String lastName);

	Collection<Person> findByLastnameEndingWith(String lastName);

	List<Person> findDistinctByLastname(String lastName, Sort order);

	List<Person> findDistinctPeopleByOrderByLastnameDesc(Sort order);

}
