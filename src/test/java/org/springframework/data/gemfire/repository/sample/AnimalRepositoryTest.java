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

import static org.assertj.core.api.Assertions.*;

import java.util.Optional;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

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
@RunWith(SpringRunner.class)
@SuppressWarnings("unused")
public class AnimalRepositoryTest {

	@Autowired
	private CatRepository catRepo;

	@Autowired
	private DogRepository dogRepo;

	protected static Animal newAnimal(long id, String name) {
		Animal animal = new Animal();
		animal.setId(id);
		animal.setName(name);
		return animal;
	}

	@Test
	public void testEntityStoredInMultipleRegions() {
		Animal felix = newAnimal(1, "Felix");
		Animal leo = newAnimal(2, "Leo");
		Animal cerberus = newAnimal(3, "Cerberus");
		Animal fido = newAnimal(1, "Fido");

		assertThat(catRepo.save(felix)).isNotNull();
		assertThat(catRepo.save(leo)).isNotNull();
		assertThat(catRepo.save(cerberus)).isNotNull();
		assertThat(dogRepo.save(fido)).isNotNull();
		assertThat(dogRepo.save(cerberus)).isNotNull();
		assertThat(catRepo.count()).isEqualTo(3L);
		assertThat(dogRepo.count()).isEqualTo(2L);

		Optional<Animal> foundFelix = catRepo.findById(1L);

		assertThat(foundFelix.isPresent()).isTrue();
		assertThat(foundFelix.get()).isEqualTo(felix);

		Animal foundLeo = catRepo.findBy("Leo");

		assertThat(foundLeo).isEqualTo(leo);

		Animal foundCerberusTheCat = catRepo.findByName("Cerberus");

		assertThat(foundCerberusTheCat).isEqualTo(cerberus);
		assertThat(catRepo.findBy("Cerberus")).isEqualTo(foundCerberusTheCat);
		assertThat(catRepo.findById(3L).orElse(null)).isEqualTo(foundCerberusTheCat);

		Animal foundFido = dogRepo.findBy("Fido");

		assertThat(foundFido).isEqualTo(fido);

		Animal foundCerberusTheDog = dogRepo.findByName("Cerberus");

		assertThat(foundCerberusTheDog).isEqualTo(cerberus);
		assertThat(dogRepo.findBy("Cerberus")).isEqualTo(foundCerberusTheDog);
		assertThat(dogRepo.findById(3L).orElse(null)).isEqualTo(foundCerberusTheDog);
	}
}
