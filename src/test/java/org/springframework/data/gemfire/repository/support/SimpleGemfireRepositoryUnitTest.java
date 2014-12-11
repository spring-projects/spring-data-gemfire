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

package org.springframework.data.gemfire.repository.support;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.repository.Wrapper;
import org.springframework.data.gemfire.repository.sample.Animal;
import org.springframework.data.gemfire.test.support.CollectionUtils;
import org.springframework.data.repository.core.EntityInformation;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.CacheTransactionManager;
import com.gemstone.gemfire.cache.DataPolicy;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionAttributes;

/**
 * The SimpleGemfireRepositoryUnitTest class is a test suite of test cases testing the contract and functionality
 * of the SimpleGemfireRepository class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.GemfireTemplate
 * @see org.springframework.data.gemfire.repository.Wrapper
 * @see org.springframework.data.gemfire.repository.support.SimpleGemfireRepository
 * @since 1.4.5
 */
@SuppressWarnings("unchecked")
public class SimpleGemfireRepositoryUnitTest {

	protected Map<Long, Animal> asMap(Iterable<Animal> animals) {
		Map<Long, Animal> animalMap = new HashMap<Long, Animal>();
		for (Animal animal : animals) {
			animalMap.put(animal.getId(), animal);
		}
		return animalMap;
	}

	protected Animal createAnimal(final String name) {
		Animal animal = new Animal();
		animal.setName(name);
		return animal;
	}

	protected Animal createAnimal(final Long id, final String name) {
		Animal animal = createAnimal(name);
		animal.setId(id);
		return animal;
	}

	protected GemfireTemplate createGemfireTemplate(final Region<?, ?> region) {
		return new GemfireTemplate(region);
	}

	protected Cache mockCache(final String mockName, final boolean transactionExists) {
		Cache mockCache = mock(Cache.class, String.format("%1$s.MockCache", mockName));

		CacheTransactionManager mockCacheTransactionManager = mock(CacheTransactionManager.class, String.format(
			"%1$s.MockCacheTransactionManager", mockName));

		when(mockCache.getCacheTransactionManager()).thenReturn(mockCacheTransactionManager);
		when(mockCacheTransactionManager.exists()).thenReturn(transactionExists);

		return mockCache;
	}

	protected EntityInformation<Animal, Long> mockEntityInformation() {
		EntityInformation<Animal, Long> mockEntityInformation = mock(EntityInformation.class);

		doAnswer(new Answer<Long>() {
			private final AtomicLong idSequence = new AtomicLong(0l);
			@Override public Long answer(final InvocationOnMock invocation) throws Throwable {
				Animal argument = Animal.class.cast(invocation.getArguments()[0]);
				Long id = argument.getId();
				id = (id != null ? id : idSequence.incrementAndGet());
				argument.setId(id);
				return id;

			}
		}).when(mockEntityInformation).getId(any(Animal.class));

		return mockEntityInformation;
	}

	protected Region mockRegion(final String mockName, final Cache mockCache, final DataPolicy dataPolicy) {
		Region mockRegion = mock(Region.class, String.format("%1$s.MockRegion", mockName));

		when(mockRegion.getRegionService()).thenReturn(mockCache);

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class, String.format(
			"%1$s.MockRegionAttributes", mockName));

		when(mockRegion.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegionAttributes.getDataPolicy()).thenReturn(dataPolicy);

		return mockRegion;
	}

	@Test
	public void testSave() {
		Region<Long, Animal> mockRegion = mock(Region.class, "testSave");

		SimpleGemfireRepository<Animal, Long> repository = new SimpleGemfireRepository<Animal, Long>(
			createGemfireTemplate(mockRegion), mockEntityInformation());

		Animal dog = repository.save(createAnimal("dog"));

		assertNotNull(dog);
		assertEquals(1l, dog.getId().longValue());
		assertEquals("dog", dog.getName());

		verify(mockRegion, times(1)).put(eq(1l), eq(dog));
	}

	@Test
	public void testSaveEntities() {
		List<Animal> animals = new ArrayList<Animal>(3);

		animals.add(createAnimal("bird"));
		animals.add(createAnimal("cat"));
		animals.add(createAnimal("dog"));

		Region<Long, Animal> mockRegion = mock(Region.class, "testSaveAll");

		SimpleGemfireRepository<Animal, Long> repository = new SimpleGemfireRepository<Animal, Long>(
			createGemfireTemplate(mockRegion), mockEntityInformation());

		Iterable<Animal> savedAnimals = repository.save(animals);

		assertNotNull(savedAnimals);

		verify(mockRegion, times(1)).putAll(eq(asMap(savedAnimals)));
	}

	@Test
	public void testSaveWrapper() {
		Animal dog = createAnimal(1l, "dog");

		Wrapper dogWrapper = new Wrapper(dog, dog.getId());

		Region<Long, Animal> mockRegion = mock(Region.class, "testSaveWrapper");

		SimpleGemfireRepository<Animal, Long> repository = new SimpleGemfireRepository<Animal, Long>(
			createGemfireTemplate(mockRegion), mockEntityInformation());

		repository.save(dogWrapper);

		verify(mockRegion, times(1)).put(eq(dog.getId()), eq(dog));
	}

	@Test
	public void testExists() {
		final Animal dog = createAnimal(1l, "dog");

		Region<Long, Animal> mockRegion = mock(Region.class, "testFindOne");

		when(mockRegion.get(any(Long.class))).then(new Answer<Animal>() {
			@Override public Animal answer(final InvocationOnMock invocation) throws Throwable {
				return (dog.getId().equals(invocation.getArguments()[0]) ? dog : null);
			}
		});

		SimpleGemfireRepository<Animal, Long> repository = new SimpleGemfireRepository<Animal, Long>(
			createGemfireTemplate(mockRegion), mockEntityInformation());

		assertTrue(repository.exists(1l));
		assertFalse(repository.exists(10l));
	}

	@Test
	public void testFindOne() {
		final Animal dog = createAnimal(1l, "dog");

		Region<Long, Animal> mockRegion = mock(Region.class, "testFindOne");

		when(mockRegion.get(any(Long.class))).then(new Answer<Animal>() {
			@Override public Animal answer(final InvocationOnMock invocation) throws Throwable {
				return (dog.getId().equals(invocation.getArguments()[0]) ? dog : null);
			}
		});

		SimpleGemfireRepository<Animal, Long> repository = new SimpleGemfireRepository<Animal, Long>(
			createGemfireTemplate(mockRegion), mockEntityInformation());

		assertEquals(dog, repository.findOne(1l));
		assertNull(repository.findOne(10l));
	}

	@Test
	public void testFindAll() {
		long id = 0;

		final List<Animal> animals = new ArrayList<Animal>(3);

		animals.add(createAnimal(++id, "bird"));
		animals.add(createAnimal(++id, "cat"));
		animals.add(createAnimal(++id, "dog"));

		Region<Long, Animal> mockRegion = mock(Region.class, "testSaveAll");

		when(mockRegion.getAll(any(Collection.class))).then(new Answer<Map<Long, Animal>>() {
			@Override public Map<Long, Animal> answer(final InvocationOnMock invocation) throws Throwable {
				Collection<Long> keys = (Collection<Long>) invocation.getArguments()[0];
				Map<Long, Animal> result = new HashMap<Long, Animal>(keys.size());
				for (Animal animal : animals) {
					if (keys.contains(animal.getId())) {
						result.put(animal.getId(), animal);
					}
				}
				return result;
			}
		});

		SimpleGemfireRepository<Animal, Long> repository = new SimpleGemfireRepository<Animal, Long>(
			createGemfireTemplate(mockRegion), mockEntityInformation());

		Collection<Animal> animalsFound = repository.findAll(Arrays.asList(1l, 3l));

		assertNotNull(animalsFound);
		assertEquals(2, animalsFound.size());
		assertTrue(animalsFound.containsAll(CollectionUtils.subList(animals, 0, 2)));

		verify(mockRegion, times(1)).getAll(eq(Arrays.asList(1l, 3l)));
	}

	@Test
	public void testDeleteById() {
		Region<Long, Animal> mockRegion = mock(Region.class, "testDeleteById");

		SimpleGemfireRepository<Animal, Long> repository = new SimpleGemfireRepository<Animal, Long>(
			createGemfireTemplate(mockRegion), mockEntityInformation());

		repository.delete(1l);

		verify(mockRegion, times(1)).remove(eq(1l));
	}

	@Test
	public void testDeleteEntity() {
		Region<Long, Animal> mockRegion = mock(Region.class, "testDeleteEntity");

		SimpleGemfireRepository<Animal, Long> repository = new SimpleGemfireRepository<Animal, Long>(
			createGemfireTemplate(mockRegion), mockEntityInformation());

		repository.delete(createAnimal(1l, "dog"));

		verify(mockRegion, times(1)).remove(eq(1l));
	}

	@Test
	public void testDeleteEntities() {
		Region<Long, Animal> mockRegion = mock(Region.class, "testDeleteEntities");

		SimpleGemfireRepository<Animal, Long> repository = new SimpleGemfireRepository<Animal, Long>(
			createGemfireTemplate(mockRegion), mockEntityInformation());

		repository.delete(Arrays.asList(createAnimal(1l, "bird"), createAnimal(2l, "cat"), createAnimal(3l, "dog")));

		verify(mockRegion, times(1)).remove(eq(1l));
		verify(mockRegion, times(1)).remove(eq(2l));
		verify(mockRegion, times(1)).remove(eq(3l));
	}

	@Test
	public void testDeleteAllWithClear() {
		Cache mockCache = mockCache("testDeleteAllWithClear", false);

		Region<Long, Animal> mockRegion = mockRegion("testDeleteAllWithClear", mockCache, DataPolicy.REPLICATE);

		SimpleGemfireRepository<Animal, Long> gemfireRepository = new SimpleGemfireRepository<Animal, Long>(
			createGemfireTemplate(mockRegion), mockEntityInformation());

		gemfireRepository.deleteAll();

		verify(mockCache, times(1)).getCacheTransactionManager();
		verify(mockRegion, times(2)).getAttributes();
		verify(mockRegion, times(2)).getRegionService();
		verify(mockRegion, times(1)).clear();
	}

	@Test
	public void testDeleteAllWithKeysWhenClearThrowsUnsupportedOperationException() {
		Cache mockCache = mockCache("testDeleteAllWithKeysWhenClearThrowsUnsupportedOperationException", false);

		Region<Long, Animal> mockRegion = mockRegion("testDeleteAllWithKeysWhenClearThrowsUnsupportedOperationException",
			mockCache, DataPolicy.PERSISTENT_REPLICATE);

		doThrow(new UnsupportedOperationException("Not Implemented!")).when(mockRegion).clear();
		when(mockRegion.keySet()).thenReturn(new HashSet<Long>(Arrays.asList(1l, 2l, 3l)));

		SimpleGemfireRepository<Animal, Long> gemfireRepository = new SimpleGemfireRepository<Animal, Long>(
			createGemfireTemplate(mockRegion), mockEntityInformation());

		gemfireRepository.deleteAll();

		verify(mockCache, times(1)).getCacheTransactionManager();
		verify(mockRegion, times(2)).getAttributes();
		verify(mockRegion, times(2)).getRegionService();
		verify(mockRegion, times(1)).clear();
		verify(mockRegion, times(1)).remove(eq(1l));
		verify(mockRegion, times(1)).remove(eq(2l));
		verify(mockRegion, times(1)).remove(eq(3l));
	}

	@Test
	public void testDeleteAllWithKeysWhenPartitionRegion() {
		Cache mockCache = mockCache("testDeleteAllWithKeysWhenPartitionRegion", false);

		Region<Long, Animal> mockRegion = mockRegion("testDeleteAllWithKeysWhenPartitionRegion", mockCache,
			DataPolicy.PERSISTENT_PARTITION);

		when(mockRegion.keySet()).thenReturn(new HashSet<Long>(Arrays.asList(1l, 2l, 3l)));

		SimpleGemfireRepository<Animal, Long> gemfireRepository = new SimpleGemfireRepository<Animal, Long>(
			createGemfireTemplate(mockRegion), mockEntityInformation());

		gemfireRepository.deleteAll();

		verify(mockCache, times(0)).getCacheTransactionManager();
		verify(mockRegion, times(2)).getAttributes();
		verify(mockRegion, times(0)).getRegionService();
		verify(mockRegion, times(0)).clear();
		verify(mockRegion, times(1)).remove(eq(1l));
		verify(mockRegion, times(1)).remove(eq(2l));
		verify(mockRegion, times(1)).remove(eq(3l));
	}

	@Test
	public void testDeleteAllWithKeysWhenTransactionPresent() {
		Cache mockCache = mockCache("testDeleteAllWithKeysWhenTransactionPresent", true);

		Region<Long, Animal> mockRegion = mockRegion("testDeleteAllWithKeysWhenTransactionPresent", mockCache,
			DataPolicy.REPLICATE);

		when(mockRegion.keySet()).thenReturn(new HashSet<Long>(Arrays.asList(1l, 2l, 3l)));

		SimpleGemfireRepository<Animal, Long> gemfireRepository = new SimpleGemfireRepository<Animal, Long>(
			createGemfireTemplate(mockRegion), mockEntityInformation());

		gemfireRepository.deleteAll();

		verify(mockCache, times(1)).getCacheTransactionManager();
		verify(mockRegion, times(2)).getAttributes();
		verify(mockRegion, times(2)).getRegionService();
		verify(mockRegion, times(0)).clear();
		verify(mockRegion, times(1)).remove(eq(1l));
		verify(mockRegion, times(1)).remove(eq(2l));
		verify(mockRegion, times(1)).remove(eq(3l));
	}

}
