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
 */
package org.springframework.data.gemfire.repository.support;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.repository.core.EntityInformation;
import org.springframework.data.repository.core.support.ReflectionEntityInformation;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.query.SelectResults;

/**
 * Integration tests for {@link SimpleGemfireRepository}.
 * 
 * @author Oliver Gierke
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("../../basic-template.xml")
public class SimpleGemfireRepositoryIntegrationTest {

	@Autowired
	GemfireTemplate template;

	SimpleGemfireRepository<Person, Long> repository;

	@Before
	public void setUp() {

		EntityInformation<Person, Long> information = new ReflectionEntityInformation<Person, Long>(Person.class);
		repository = new SimpleGemfireRepository<Person, Long>(template, information);
	}

	@Test
	public void storeAndDeleteEntity() {

		Person person = new Person(1L, "Oliver", "Gierke");

		repository.save(person);

		assertThat(repository.count(), is(1L));
		assertThat(repository.findOne(person.id), is(person));
		assertThat(repository.findAll().size(), is(1));

		repository.delete(person);

		assertThat(repository.count(), is(0L));
		assertThat(repository.findOne(person.id), is(nullValue()));
		assertThat(repository.findAll().size(), is(0));
	}

	@Test
	public void queryRegion() throws Exception {

		Person person = new Person(1L, "Oliver", "Gierke");

		template.put(1L, person);

		SelectResults<Person> persons = template.find("SELECT * FROM /simple s WHERE s.firstname = $1", person.firstname);

		assertThat(persons.size(), is(1));
		assertThat(persons.iterator().next(), is(person));
	}

	@Test
	public void findAllWithGivenIds() {

		Person dave = new Person(1L, "Dave", "Matthews");
		Person carter = new Person(2L, "Carter", "Beauford");
		Person leroi = new Person(3L, "Leroi", "Moore");

		template.put(dave.id, dave);
		template.put(carter.id, carter);
		template.put(leroi.id, leroi);

		Collection<Person> result = repository.findAll(Arrays.asList(carter.id, leroi.id));
		assertThat(result, hasItems(carter, leroi));
		assertThat(result, not(hasItems(dave)));
	}
}
