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

package org.springframework.data.gemfire.repository.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.Serializable;

import org.junit.Before;
import org.junit.Test;
import org.springframework.data.annotation.Id;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.gemfire.repository.sample.Algorithm;
import org.springframework.data.gemfire.repository.sample.Animal;
import org.springframework.data.mapping.context.MappingContext;

/**
 * The DefaultGemfireEntityInformationTest class is a test suite of test cases testing the contract and functionality
 * of the DefaultGemfireEntityInformation class used to extract entity information during persistence/mapping operations
 * during data access to the underlying data store (GemFire).
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.repository.query.DefaultGemfireEntityInformation
 * @since 1.4.0
 */
public class DefaultGemfireEntityInformationTest {

	private MappingContext mappingContext;

	@Before
	public void setup() {
		mappingContext = new GemfireMappingContext();
	}

	protected Algorithm createAlgorithm(final String name) {
		return new Algorithm() {
			public String getName() {
				return name;
			}
		};
	}

	protected Animal createAnimal(final Long id, final String name) {
		Animal animal = new Animal();
		animal.setId(id);
		animal.setName(name);
		return animal;
	}

	protected <T, ID extends Serializable> GemfireEntityInformation<T, ID> createEntityInformation(
			final GemfirePersistentEntity<T> persistentEntity) {
		return new DefaultGemfireEntityInformation<T, ID>(persistentEntity);
	}

	@SuppressWarnings("unchecked")
	protected <T> GemfirePersistentEntity<T> createPersistentEntity(final Class<T> domainEntityType) {
		return (GemfirePersistentEntity<T>) mappingContext.getPersistentEntity(domainEntityType);
	}

	@Test
	public void testInterfaceBasedEntity() {
		GemfireEntityInformation<Algorithm, String> entityInfo = createEntityInformation(
			createPersistentEntity(Algorithm.class));

		assertNotNull(entityInfo);
		assertEquals("Algorithms", entityInfo.getRegionName());
		assertTrue(Algorithm.class.isAssignableFrom(entityInfo.getJavaType()));
		assertEquals(String.class, entityInfo.getIdType());
		assertEquals("Quick Sort", entityInfo.getId(createAlgorithm("Quick Sort")));
	}

	@Test
	public void testClassBasedEntity() {
		GemfireEntityInformation<Animal, Long> entityInfo = createEntityInformation(
			createPersistentEntity(Animal.class));

		assertNotNull(entityInfo);
		assertEquals("Animal", entityInfo.getRegionName());
		assertEquals(Animal.class, entityInfo.getJavaType());
		assertEquals(Long.class, entityInfo.getIdType());
		assertEquals(new Long(1l), entityInfo.getId(createAnimal(1l, "Tyger")));
	}

	@Test
	public void testConfusedDomainEntityHavingLongId() {
		GemfireEntityInformation<MyConfusedDomainEntity, Long> entityInfo = createEntityInformation(
			createPersistentEntity(MyConfusedDomainEntity.class));

		assertNotNull(entityInfo);
		assertEquals("MyConfusedDomainEntity", entityInfo.getRegionName());
		assertEquals(MyConfusedDomainEntity.class, entityInfo.getJavaType());
		assertEquals(Long.class, entityInfo.getIdType());
		assertEquals(new Long(123l), entityInfo.getId(new MyConfusedDomainEntity(123l)));
	}

	@Test
	public void testConfusedDomainEntityHavingStringId() {
		GemfireEntityInformation<MyConfusedDomainEntity, String> entityInfo = createEntityInformation(
			createPersistentEntity(MyConfusedDomainEntity.class));

		assertNotNull(entityInfo);
		assertEquals("MyConfusedDomainEntity", entityInfo.getRegionName());
		assertEquals(MyConfusedDomainEntity.class, entityInfo.getJavaType());
		//assertEquals(String.class, entityInfo.getIdType());
		assertTrue(Long.class.equals(entityInfo.getIdType()));
		assertEquals(123l, entityInfo.getId(new MyConfusedDomainEntity(123l)));
		assertEquals(248l, entityInfo.getId(new MyConfusedDomainEntity("248")));
	}

	@SuppressWarnings("unused")
	protected static class MyConfusedDomainEntity {

		@Id private Long id;

		protected MyConfusedDomainEntity() {
			this((Long) null);
		}

		protected MyConfusedDomainEntity(final Long id) {
			this.id = id;
		}

		protected MyConfusedDomainEntity(final String id) {
			setId(id);
		}

		@Id
		protected String getId() {
			return String.valueOf(id);
		}

		protected final void setId(String id) {
			try {
				this.id = Long.valueOf(id);
			}
			catch (NumberFormatException e) {
				this.id = null;
			}
		}
	}

}
