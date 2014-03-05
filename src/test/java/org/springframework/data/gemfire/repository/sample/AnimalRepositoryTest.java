package org.springframework.data.gemfire.repository.sample;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Stuart Williams
 * @author John Blum
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
	public void testAnimals() {
		Animal felix = createAnimal(1, "Felix");
		Animal leo = createAnimal(2, "Leo");
		Animal credo = createAnimal(3, "Credo");
		Animal fido = createAnimal(1, "Fido");

		assertNotNull(catRepo.save(felix));
		assertNotNull(catRepo.save(leo));
		assertNotNull(catRepo.save(credo));
		assertNotNull(dogRepo.save(fido));
		assertNotNull(dogRepo.save(credo));
		assertEquals(3L, catRepo.count());
		assertEquals(2L, dogRepo.count());

		Animal foundFelix = catRepo.findOne(1L);

		assertEquals(felix, foundFelix);

		Animal foundLeo = catRepo.findBy("Leo");

		assertEquals(leo, foundLeo);

		Animal foundCredoTheCat = catRepo.findByName("Credo");

		assertEquals(credo, foundCredoTheCat);
		assertEquals(foundCredoTheCat, catRepo.findBy("Credo"));
		assertEquals(foundCredoTheCat, catRepo.findOne(3L));

		Animal foundFido = dogRepo.findBy("Fido");

		assertEquals(fido, foundFido);

		Animal foundCredoTheDog = dogRepo.findByName("Credo");

		assertEquals(credo, foundCredoTheDog);
		assertEquals(foundCredoTheDog, dogRepo.findBy("Credo"));
		assertEquals(foundCredoTheDog, dogRepo.findOne(3L));
	}

}
