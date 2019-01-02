/*
 * Copyright 2010-2019 the original author or authors.
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
import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * The AnimalRepositoryTest class is a test suite of test cases testing the functionality behind PR #55 involving
 * persisting application domain object/entities to multiple Regions in GemFire's Cache.
 *
 * @author Stuart Williams
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringJUnit4ClassRunner
 * @since 1.4.0
 * @link https://github.com/spring-projects/spring-data-gemfire/pull/55
 */
@ContextConfiguration
@RunWith(SpringJUnit4ClassRunner.class)
@SuppressWarnings("unused")
public class AnimalRepositoryTest {

	@Autowired
	private CatRepository catRepo;

	@Autowired
	private DogRepository dogRepo;

	protected static Animal createAnimal(final long id, final String name) {
		Animal animal = new Animal();
		animal.setId(id);
		animal.setName(name);
		return animal;
	}

	@Test
	public void testEntityStoredInMultipleRegions() {
		Animal felix = createAnimal(1, "Felix");
		Animal leo = createAnimal(2, "Leo");
		Animal cerberus = createAnimal(3, "Cerberus");
		Animal fido = createAnimal(1, "Fido");

		assertNotNull(catRepo.save(felix));
		assertNotNull(catRepo.save(leo));
		assertNotNull(catRepo.save(cerberus));
		assertNotNull(dogRepo.save(fido));
		assertNotNull(dogRepo.save(cerberus));
		assertEquals(3L, catRepo.count());
		assertEquals(2L, dogRepo.count());

		Animal foundFelix = catRepo.findOne(1L);

		assertEquals(felix, foundFelix);

		Animal foundLeo = catRepo.findBy("Leo");

		assertEquals(leo, foundLeo);

		Animal foundCerberusTheCat = catRepo.findByName("Cerberus");

		assertEquals(cerberus, foundCerberusTheCat);
		assertEquals(foundCerberusTheCat, catRepo.findBy("Cerberus"));
		assertEquals(foundCerberusTheCat, catRepo.findOne(3L));

		Animal foundFido = dogRepo.findBy("Fido");

		assertEquals(fido, foundFido);

		Animal foundCerberusTheDog = dogRepo.findByName("Cerberus");

		assertEquals(cerberus, foundCerberusTheDog);
		assertEquals(foundCerberusTheDog, dogRepo.findBy("Cerberus"));
		assertEquals(foundCerberusTheDog, dogRepo.findOne(3L));
	}

}
