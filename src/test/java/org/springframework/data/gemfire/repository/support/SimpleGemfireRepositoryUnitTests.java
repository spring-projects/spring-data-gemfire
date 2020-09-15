/*
 * Copyright 2010-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.springframework.data.gemfire.repository.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.CacheTransactionManager;
import org.apache.geode.cache.DataPolicy;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.query.SelectResults;

import org.springframework.data.gemfire.GemfireTemplate;
import org.springframework.data.gemfire.repository.Wrapper;
import org.springframework.data.gemfire.repository.sample.Animal;
import org.springframework.data.repository.core.EntityInformation;

/**
 * Unit Tests for {@link SimpleGemfireRepository}.
 *
 * @author John Blum
 * @see java.util.function.Function
 * @see java.util.stream.Stream
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.Region
 * @see org.springframework.data.gemfire.GemfireTemplate
 * @see org.springframework.data.gemfire.repository.Wrapper
 * @see org.springframework.data.gemfire.repository.support.SimpleGemfireRepository
 * @see org.springframework.data.repository.core.EntityInformation
 * @since 1.4.5
 */
@SuppressWarnings({  "rawtypes", "unchecked" })
public class SimpleGemfireRepositoryUnitTests {

	protected Map<Long, Animal> asMap(Iterable<Animal> animals) {

		Map<Long, Animal> animalMap = new HashMap<>();

		for (Animal animal : animals) {
			animalMap.put(animal.getId(), animal);
		}

		return animalMap;
	}

	protected Animal newAnimal(String name) {

		Animal animal = new Animal();

		animal.setName(name);

		return animal;
	}

	protected Animal newAnimal(Long id, String name) {

		Animal animal = newAnimal(name);

		animal.setId(id);

		return animal;
	}

	protected GemfireTemplate newGemfireTemplate(Region<?, ?> region) {
		return new GemfireTemplate(region);
	}

	protected Cache mockCache(String name, boolean transactionExists) {

		Cache mockCache = mock(Cache.class, String.format("%s.MockCache", name));

		CacheTransactionManager mockCacheTransactionManager = mock(CacheTransactionManager.class,
			String.format("%s.MockCacheTransactionManager", name));

		when(mockCache.getCacheTransactionManager()).thenReturn(mockCacheTransactionManager);
		when(mockCacheTransactionManager.exists()).thenReturn(transactionExists);

		return mockCache;
	}

	protected EntityInformation<Animal, Long> mockEntityInformation() {

		EntityInformation<Animal, Long> mockEntityInformation = mock(EntityInformation.class);

		doAnswer(new Answer<Long>() {

			private final AtomicLong idSequence = new AtomicLong(0L);

			@Override
			public Long answer(InvocationOnMock invocation) {
				Animal argument = invocation.getArgument(0);
				argument.setId(resolveId(argument.getId()));
				return argument.getId();

			}

			private Long resolveId(Long id) {
				return (id != null ? id : idSequence.incrementAndGet());
			}

		}).when(mockEntityInformation).getRequiredId(any(Animal.class));

		return mockEntityInformation;
	}

	protected Region mockRegion() {
		return mockRegion("MockRegion");
	}

	protected Region mockRegion(String name) {

		Region mockRegion = mock(Region.class, String.format("%s.MockRegion", name));

		when(mockRegion.getName()).thenReturn(name);
		when(mockRegion.getFullPath()).thenReturn(String.format("%1$s%2$s", Region.SEPARATOR, name));

		return mockRegion;
	}

	protected Region mockRegion(String name, Cache mockCache, DataPolicy dataPolicy) {

		Region mockRegion = mockRegion(name);

		when(mockRegion.getRegionService()).thenReturn(mockCache);

		RegionAttributes mockRegionAttributes = mock(RegionAttributes.class,
			String.format("%s.MockRegionAttributes", name));

		when(mockRegion.getAttributes()).thenReturn(mockRegionAttributes);
		when(mockRegionAttributes.getDataPolicy()).thenReturn(dataPolicy);

		return mockRegion;
	}

	@Test
	public void constructsSimpleGemfireRepositorySuccessfully() {

		Region mockRegion = mock(Region.class);

		GemfireTemplate template = spy(new GemfireTemplate(mockRegion));

		EntityInformation mockEntityInformation = mock(EntityInformation.class);

		SimpleGemfireRepository repository = new SimpleGemfireRepository(template, mockEntityInformation);

		assertThat(repository).isNotNull();

		verifyZeroInteractions(template, mockRegion, mockEntityInformation);
	}

	@Test(expected = IllegalArgumentException.class)
	public void constructSimpleGemfireRepositoryWithNullTemplateThrowsIllegalArgumentException() {

		try {
			new SimpleGemfireRepository<>(null, mockEntityInformation());
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("GemfireTemplate must not be null");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void constructSimpleGemfireRepositoryWithNullEntityInformationThrowsIllegalArgumentException() {

		try {
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion()), null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("EntityInformation must not be null");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void saveEntitySuccessfully() {

		Animal cat = newAnimal(1L, "cat");

		Region<Long, Animal> mockRegion = mockRegion();

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion), mockEntityInformation());

		Animal dog = repository.save(newAnimal("dog"));

		assertThat(dog).isNotNull();
		assertThat(dog.getId().longValue()).isEqualTo(1L);
		assertThat(dog.getName()).isEqualTo("dog");

		verify(mockRegion, times(1)).put(eq(1L), eq(dog));
	}

	@Test
	public void saveEntitiesIsCorrect() {
		List<Animal> animals = new ArrayList<>(3);

		animals.add(newAnimal("bird"));
		animals.add(newAnimal("cat"));
		animals.add(newAnimal("dog"));

		Region<Long, Animal> mockRegion = mockRegion();

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion), mockEntityInformation());

		Iterable<Animal> savedAnimals = repository.saveAll(animals);

		assertThat(savedAnimals).isNotNull();

		verify(mockRegion, times(1)).putAll(eq(asMap(savedAnimals)));
	}

	@Test
	public void saveWrapperIsCorrect() {
		Animal dog = newAnimal(1L, "dog");

		Wrapper dogWrapper = new Wrapper(dog, dog.getId());

		Region<Long, Animal> mockRegion = mockRegion();

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion), mockEntityInformation());

		assertThat(repository.save(dogWrapper)).isEqualTo(dog);

		verify(mockRegion, times(1)).put(eq(1L), eq(dog));
		verifyNoMoreInteractions(mockRegion);
	}

	@Test
	public void countReturnsNumberOfRegionEntries() {

		SelectResults mockSelectResults = mock(SelectResults.class);

		Region mockRegion = mockRegion("Example");

		GemfireTemplate template = spy(newGemfireTemplate(mockRegion));

		doReturn(mockSelectResults).when(template).find(eq("SELECT count(*) FROM /Example"));
		when(mockSelectResults.iterator()).thenReturn(Collections.singletonList(21).iterator());

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository<>(template, mockEntityInformation());

		assertThat(repository).isNotNull();
		assertThat(repository.count()).isEqualTo(21L);

		verify(template, times(1)).getRegion();
		verify(mockRegion, times(1)).getFullPath();
		verify(template, times(1)).find(eq("SELECT count(*) FROM /Example"));
		verify(mockSelectResults, times(1)).iterator();
		verifyNoMoreInteractions(mockRegion, mockSelectResults, template);
	}

	@Test
	public void countWhenSelectResultsAreNullIsNullSafeAndReturnsZero() {

		Region mockRegion = mockRegion("Example");

		GemfireTemplate template = spy(newGemfireTemplate(mockRegion));

		doReturn(null).when(template).find(eq("SELECT count(*) FROM /Example"));

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository<>(template, mockEntityInformation());

		assertThat(repository).isNotNull();
		assertThat(repository.count()).isEqualTo(0L);

		verify(template, times(1)).getRegion();
		verify(mockRegion, times(1)).getFullPath();
		verify(template, times(1)).find(eq("SELECT count(*) FROM /Example"));
		verifyNoMoreInteractions(mockRegion, template);
	}

	@Test
	public void countWhenSelectResultsIteratorIsNullIsNullSafeAndReturnsZero() {

		SelectResults mockSelectResults = mock(SelectResults.class);

		Region mockRegion = mockRegion("Example");

		GemfireTemplate template = spy(newGemfireTemplate(mockRegion));

		doReturn(mockSelectResults).when(template).find(eq("SELECT count(*) FROM /Example"));
		doReturn(null).when(mockSelectResults).iterator();

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository<>(template, mockEntityInformation());

		assertThat(repository).isNotNull();
		assertThat(repository.count()).isEqualTo(0L);

		verify(template, times(1)).getRegion();
		verify(mockRegion, times(1)).getFullPath();
		verify(template, times(1)).find(eq("SELECT count(*) FROM /Example"));
		verify(mockSelectResults, times(1)).iterator();
		verifyNoMoreInteractions(mockRegion, mockSelectResults, template);
	}

	@Test
	public void countWhenSelectResultsIteratorIsEmptyReturnsZero() {

		SelectResults mockSelectResults = mock(SelectResults.class);

		Region mockRegion = mockRegion("Example");

		GemfireTemplate template = spy(newGemfireTemplate(mockRegion));

		doReturn(mockSelectResults).when(template).find(eq("SELECT count(*) FROM /Example"));
		doReturn(Collections.emptyIterator()).when(mockSelectResults).iterator();

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository<>(template, mockEntityInformation());

		assertThat(repository).isNotNull();
		assertThat(repository.count()).isEqualTo(0L);

		verify(template, times(1)).getRegion();
		verify(mockRegion, times(1)).getFullPath();
		verify(template, times(1)).find(eq("SELECT count(*) FROM /Example"));
		verify(mockSelectResults, times(1)).iterator();
		verifyNoMoreInteractions(mockRegion, mockSelectResults, template);
	}

	@Test
	public void existsIsCorrect() {
		Animal dog = newAnimal(1L, "dog");

		Region<Long, Animal> mockRegion = mockRegion();

		when(mockRegion.get(any(Long.class))).then(
			invocation -> (dog.getId().equals(invocation.getArguments()[0]) ? dog : null));

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion), mockEntityInformation());

		assertThat(repository.existsById(1L)).isTrue();
		assertThat(repository.existsById(10L)).isFalse();
	}

	@Test
	public void findOneIsCorrect() {
		Animal dog = newAnimal(1L, "dog");

		Region<Long, Animal> mockRegion = mockRegion();

		when(mockRegion.get(any(Long.class))).then(
			invocation -> (dog.getId().equals(invocation.getArguments()[0]) ? dog : null));

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion), mockEntityInformation());

		assertThat(repository.findById(1L).orElse(null)).isEqualTo(dog);
		assertThat(repository.findById(10L).isPresent()).isFalse();
	}

	@Test
	public void findAllIsCorrect() {
		Map<Long, Animal> animals =
			Stream.of(newAnimal(1L, "bird"), newAnimal(2L, "cat"), newAnimal(3L, "dog"))
				.collect(Collectors.toMap(Animal::getId, Function.identity()));

		Region<Long, Animal> mockRegion = mockRegion();

		when(mockRegion.getAll(any(Collection.class))).then(invocation -> {
			Collection<Long> keys = invocation.getArgument(0);

			return animals.values().stream().filter((animal -> keys.contains(animal.getId())))
				.collect(Collectors.toMap(Animal::getId, Function.identity()));
		});

		SimpleGemfireRepository<Animal, Long> repository = new SimpleGemfireRepository<>(
			newGemfireTemplate(mockRegion), mockEntityInformation());

		Collection<Animal> animalsFound = repository.findAllById(Arrays.asList(1L, 3L));

		assertThat(animalsFound).isNotNull();
		assertThat(animalsFound).hasSize(2);
		assertThat(animalsFound).contains(animals.get(1L), animals.get(3L));

		verify(mockRegion, times(1)).getAll(eq(Arrays.asList(1L, 3L)));
	}

	@Test
	public void findAllWithIdsReturnsNoMatches() {
		Region<Long, Animal> mockRegion = mockRegion();

		when(mockRegion.getAll(any(Collection.class))).then(invocation -> {
			Collection<Long> keys = invocation.getArgument(0);
			Map<Long, Animal> result = new HashMap<>(keys.size());

			for (Long key : keys) {
				result.put(key, null);
			}

			return result;
		});

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion), mockEntityInformation());

		Collection<Animal> animalsFound = repository.findAllById(Arrays.asList(1L, 2L, 3L));

		assertThat(animalsFound).isNotNull();
		assertThat(animalsFound).isEmpty();

		verify(mockRegion, times(1)).getAll(eq(Arrays.asList(1L, 2L, 3L)));
	}

	@Test
	public void findAllWithIdsReturnsPartialMatches() {
		Map<Long, Animal> animals =
			Stream.of(newAnimal(1L, "bird"), newAnimal(2L, "cat"), newAnimal(3L, "dog"))
				.collect(Collectors.toMap(Animal::getId, Function.identity()));

		Region<Long, Animal> mockRegion = mockRegion();

		when(mockRegion.getAll(any(Collection.class))).then(invocation -> {
			Collection<Long> keys = invocation.getArgument(0);
			Map<Long, Animal> result = new HashMap<>(keys.size());

			for (Long key : keys) {
				result.put(key, animals.get(key));
			}

			return result;
		});

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion), mockEntityInformation());

		Collection<Animal> animalsFound = repository.findAllById(Arrays.asList(0L, 1L, 2L, 4L));

		assertThat(animalsFound).isNotNull();
		assertThat(animalsFound).hasSize(2);
		assertThat(animalsFound).contains(animals.get(1L), animals.get(2L));

		verify(mockRegion, times(1)).getAll(eq(Arrays.asList(0L, 1L, 2L, 4L)));
	}

	@Test
	public void findAllByIdWithNullIterableIsNullSafe() {

		Region<Long, Animal> mockRegion = mockRegion();

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository(newGemfireTemplate(mockRegion), mockEntityInformation());

		Iterable<Animal> animals = repository.findAllById(null);

		assertThat(animals).isNotNull();
		assertThat(animals).isEmpty();

		verifyZeroInteractions(mockRegion);
	}

	@Test
	public void findByIdSuccessfully() {

		Animal dog = newAnimal(1L, "dog");

		Region<Long, Animal> mockRegion = mockRegion();

		doAnswer(invocation -> dog.getId().equals(invocation.getArgument(0)) ? dog : null)
			.when(mockRegion).get(any(Long.class));

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion), mockEntityInformation());

		assertThat(repository.findById(1L).orElse(null)).isEqualTo(dog);
		assertThat(repository.findById(2L).isPresent()).isFalse();
		assertThat(repository.findById(10L).isPresent()).isFalse();

		verify(mockRegion, times(1)).get(eq(1L));
		verify(mockRegion, times(1)).get(eq(2L));
		verify(mockRegion, times(1)).get(eq(10L));
		verifyNoMoreInteractions(mockRegion);
	}

	@Test
	public void findByIdWithNullIdIsNullSafe() {

		Region mockRegion = mockRegion();

		SimpleGemfireRepository repository =
			new SimpleGemfireRepository(newGemfireTemplate(mockRegion), mockEntityInformation());

		assertThat(repository.findById(null).isPresent()).isFalse();

		verifyZeroInteractions(mockRegion);
	}

	@Test
	public void deleteByIdSuccessfully() {

		Region<Long, Animal> mockRegion = mockRegion();

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion), mockEntityInformation());

		repository.deleteById(1L);

		verify(mockRegion, times(1)).remove(eq(1L));
	}

	@Test
	public void deleteEntityIsCorrect() {
		Region<Long, Animal> mockRegion = mockRegion();

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion), mockEntityInformation());

		repository.delete(newAnimal(1L, "dog"));

		verify(mockRegion, times(1)).remove(eq(1L));
	}

	@Test
	public void deleteEntitiesIsCorrect() {
		Region<Long, Animal> mockRegion = mockRegion();

		SimpleGemfireRepository<Animal, Long> repository =
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion), mockEntityInformation());

		repository.deleteAll(Arrays.asList(newAnimal(1L, "bird"), newAnimal(2L, "cat"),
			newAnimal(3L, "dog")));

		verify(mockRegion, times(1)).remove(eq(1L));
		verify(mockRegion, times(1)).remove(eq(2L));
		verify(mockRegion, times(1)).remove(eq(3L));
	}

	@Test
	public void deleteAllWithClear() {
		Cache mockCache = mockCache("MockCache", false);

		Region<Long, Animal> mockRegion = mockRegion("MockRegion", mockCache, DataPolicy.REPLICATE);

		SimpleGemfireRepository<Animal, Long> gemfireRepository =
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion), mockEntityInformation());

		gemfireRepository.deleteAll();

		verify(mockCache, times(1)).getCacheTransactionManager();
		verify(mockRegion, times(2)).getAttributes();
		verify(mockRegion, times(2)).getRegionService();
		verify(mockRegion, times(1)).clear();
	}

	@Test
	public void deleteAllWithKeysWhenClearThrowsException() {
		Cache mockCache = mockCache("MockCache", false);

		Region<Long, Animal> mockRegion = mockRegion("MockRegion", mockCache, DataPolicy.PERSISTENT_REPLICATE);

		Set<Long> keys = new HashSet<>(Arrays.asList(1L, 2L, 3L));

		doThrow(new UnsupportedOperationException("Not Implemented!")).when(mockRegion).clear();
		when(mockRegion.keySet()).thenReturn(keys);

		SimpleGemfireRepository<Animal, Long> gemfireRepository =
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion), mockEntityInformation());

		gemfireRepository.deleteAll();

		verify(mockCache, times(1)).getCacheTransactionManager();
		verify(mockRegion, times(2)).getAttributes();
		verify(mockRegion, times(2)).getRegionService();
		verify(mockRegion, times(1)).clear();
		verify(mockRegion, times(1)).removeAll(eq(keys));
	}

	@Test
	public void deleteAllWithKeysWhenPartitionRegion() {
		Cache mockCache = mockCache("MockCache", false);

		Region<Long, Animal> mockRegion = mockRegion("MockRegion", mockCache, DataPolicy.PERSISTENT_PARTITION);

		Set<Long> keys = new HashSet<>(Arrays.asList(1L, 2L, 3L));

		when(mockRegion.keySet()).thenReturn(keys);

		SimpleGemfireRepository<Animal, Long> gemfireRepository =
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion), mockEntityInformation());

		gemfireRepository.deleteAll();

		verify(mockCache, times(0)).getCacheTransactionManager();
		verify(mockRegion, times(2)).getAttributes();
		verify(mockRegion, times(0)).getRegionService();
		verify(mockRegion, times(0)).clear();
		verify(mockRegion, times(1)).removeAll(eq(keys));
	}

	@Test
	public void deleteAllWithKeysWhenTransactionPresent() {
		Cache mockCache = mockCache("MockCache", true);

		Region<Long, Animal> mockRegion = mockRegion("MockRegion", mockCache, DataPolicy.REPLICATE);

		Set<Long> keys = new HashSet<>(Arrays.asList(1L, 2L, 3L));

		when(mockRegion.keySet()).thenReturn(keys);

		SimpleGemfireRepository<Animal, Long> gemfireRepository =
			new SimpleGemfireRepository<>(newGemfireTemplate(mockRegion), mockEntityInformation());

		gemfireRepository.deleteAll();

		verify(mockCache, times(1)).getCacheTransactionManager();
		verify(mockRegion, times(2)).getAttributes();
		verify(mockRegion, times(2)).getRegionService();
		verify(mockRegion, times(0)).clear();
		verify(mockRegion, times(1)).removeAll(eq(keys));
	}
}
