package org.springframework.data.gemfire.repository.sample;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * @author Stuart Williams
 * @author John Blum
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

	@Autowired
	private RabbitRepository rabbitRepo;

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

	@Test(expected = ClassCastException.class)
	public void testStoreAnimalHavingLongIdInRabbitsRegionWithStringKey() {
		try {
			rabbitRepo.save(createAnimal(123L, "Harry"));
		}
		// NOTE the ClassCastException thrown from GemFire is expected; this is not correct and the identifying type
		// mismatch should have been caught by GemfireRepositoryFactory.getTemplate(..) method on line 129
		// (appropriately throwing an IllegalArgumentException) after satisfying the condition on line 128,
		// if only the @Region annotation were set on the domain class/entity!
		catch (ClassCastException unexpected) {
			//unexpected.printStackTrace(System.err);
			assertTrue(unexpected.getMessage().contains("key ( java.lang.Long ) does not satisfy keyConstraint ( java.lang.String )"));
			throw unexpected;
		}
	}

}
