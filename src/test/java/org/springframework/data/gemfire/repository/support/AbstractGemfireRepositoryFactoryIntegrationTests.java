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
import java.util.List;

import org.hamcrest.Matchers;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.Regions;
import org.springframework.data.gemfire.repository.sample.Person;
import org.springframework.data.gemfire.repository.sample.PersonRepository;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import com.gemstone.gemfire.cache.Region;

/**
 * Integration test for {@link GemfireRepositoryFactory}.
 * 
 * @author Oliver Gierke
 */
@RunWith(SpringJUnit4ClassRunner.class)
public abstract class AbstractGemfireRepositoryFactoryIntegrationTests {

	@Autowired
	List<Region<?, ?>> regions;

	Person dave, carter, boyd, stefan, leroi, jeff;
	PersonRepository repository;

	@Before
	public void setUp() {

		dave = new Person(1L, "Dave", "Matthews");
		carter = new Person(2L, "Carter", "Beauford");
		boyd = new Person(3L, "Boyd", "Tinsley");
		stefan = new Person(4L, "Stefan", "Lessard");
		leroi = new Person(5L, "Leroi", "Moore");
		jeff = new Person(6L, "Jeff", "Coffin");

		GemfireMappingContext context = new GemfireMappingContext();

		Regions regions = new Regions(this.regions, context);
		GemfireTemplate template = new GemfireTemplate(regions.getRegion(Person.class));

		template.put(dave.id, dave);
		template.put(carter.id, carter);
		template.put(boyd.id, boyd);
		template.put(stefan.id, stefan);
		template.put(leroi.id, leroi);
		template.put(jeff.id, jeff);

		repository = getRepository(regions);
	}

	protected abstract PersonRepository getRepository(Regions regions);

	@Test
	public void foo() {
		assertResultsFound(repository.findByFirstnameAnnotated("Dave"), dave);
	}

	@Test
	public void executesAnnotatedInQueryMethodCorrectly() {
		assertResultsFound(repository.findByFirstnamesAnnotated(Arrays.asList("Carter", "Dave")), carter, dave);
	}

	@Test
	public void executesInQueryMethodCorrectly() {
		assertResultsFound(repository.findByFirstnameIn(Arrays.asList("Carter", "Dave")), carter, dave);
	}

	@Test
	public void executesDerivedQueryCorrectly() {
		assertResultsFound(repository.findByFirstname("Carter"), carter);
		assertResultsFound(repository.findByFirstnameIn(Arrays.asList("Stefan", "Boyd")), stefan, boyd);
		assertResultsFound(repository.findByFirstnameIn("Leroi"), leroi);
	}

	@Test
	public void executesDerivedQueryWithAndCorrectly() {
		assertResultsFound(repository.findByFirstnameAndLastname("Carter", "Beauford"), carter);
	}

	@Test
	public void executesDerivedQueryWithOrCorrectly() {
		assertResultsFound(repository.findByFirstnameOrLastname("Carter", "Matthews"), carter, dave);
	}

	/**
	 * @see SGF-101
	 */
	@Test
	public void deletesAllEntitiesFromRegions() {

		repository.deleteAll();

		assertResultsFound(repository.findAll());
	}

	private <T> void assertResultsFound(Iterable<T> result, T... expected) {

		assertThat(result, is(notNullValue()));
		assertThat(result, is(Matchers.<T> iterableWithSize(expected.length)));

		for (T element : expected) {
			assertThat(result, hasItem(element));
		}
	}
}
