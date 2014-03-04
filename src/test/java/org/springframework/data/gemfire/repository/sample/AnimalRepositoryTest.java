package org.springframework.data.gemfire.repository.sample;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@ContextConfiguration
@RunWith(SpringJUnit4ClassRunner.class)
public class AnimalRepositoryTest {

	@Autowired
	CatRepository catRepo;

	@Autowired
	DogRepository dogRepo;

	@Test
	public void foo() {

		Animal felix = new Animal();
		felix.setId(1);
		felix.setName("Felix");

		Animal leo = new Animal();
		leo.setId(2);
		leo.setName("Leo");

		Animal fido = new Animal();
		fido.setId(1);
		fido.setName("Fido");

		Animal leo_ = catRepo.save(leo);
		assertNotNull(leo_);

		Animal felix_ = catRepo.save(felix);
		assertNotNull(felix_);

		Animal fido_ = dogRepo.save(fido);
		assertNotNull(fido_);

		assertEquals(2L, catRepo.count());
		assertEquals(1L, dogRepo.count());

		Animal foundFelix = catRepo.findOne(1L);
		assertEquals(felix, foundFelix);

		Animal findOther = catRepo.findByName("Leo");
		assertEquals(leo, findOther);

		Animal foundFido = dogRepo.findBy("Fido");
		assertEquals(fido, foundFido);
	}

}
