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

package org.springframework.data.gemfire.util;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import org.junit.Test;

import org.springframework.data.gemfire.test.support.MapBuilder;

/**
 * Unit tests for {@link CollectionUtils}.
 *
 * @author John Blum
 * @see java.lang.Iterable
 * @see java.util.Collection
 * @see java.util.Collections
 * @see java.util.Enumeration
 * @see java.util.Iterator
 * @see java.util.List
 * @see java.util.Map
 * @see java.util.Set
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.util.CollectionUtils
 * @since 1.7.0
 */
public class CollectionUtilsUnitTests {

	@Test
	public void addAllIterableElementsToList() {

		List<Integer> target = new ArrayList<>(Arrays.asList(1, 2, 3));
		Set<Integer> source = new HashSet<>(Arrays.asList(1, 2, 3));

		target = CollectionUtils.addAll(target, source);

		assertThat(target).isNotNull();
		assertThat(target.size()).isEqualTo(6);
		assertThat(target).isEqualTo(Arrays.asList(1, 2, 3, 1, 2, 3));
	}

	@Test
	public void addAllIterableElementsToSet() {

		Set<Integer> target = new HashSet<>(Arrays.asList(1, 2, 3));
		Set<Integer> source = new HashSet<>(Arrays.asList(1, 2, 3, 4, 5));

		target = CollectionUtils.addAll(target, source);

		assertThat(target).isNotNull();
		assertThat(target.size()).isEqualTo(5);
		assertThat(target).contains(1, 2, 3, 4, 5);
	}

	@Test(expected = IllegalArgumentException.class)
	public void addIterableToNullCollection() {

		try {
			CollectionUtils.addAll(null, Collections.emptySet());
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("Collection is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void addEmptyIterableToCollection() {

		Collection<Integer> target = new ArrayList<>(Arrays.asList(1, 2, 3));

		target = CollectionUtils.addAll(target, Collections.emptyList());

		assertThat(target).isNotNull();
		assertThat(target.size()).isEqualTo(3);
		assertThat(target).contains(1, 2, 3);
	}

	@Test
	public void addNullIterableToCollection() {

		Collection<Integer> target = new ArrayList<>(Arrays.asList(1, 2, 3));

		target = CollectionUtils.addAll(target, null);

		assertThat(target).isNotNull();
		assertThat(target.size()).isEqualTo(3);
		assertThat(target).contains(1, 2, 3);
	}

	@Test
	public void asSetContainsAllArrayElements() {

		Object[] elements = { "a", "b", "c" };

		Set<?> set = CollectionUtils.asSet(elements);

		assertThat(set).isNotNull();
		assertThat(set.size()).isEqualTo(elements.length);
		assertThat(set.containsAll(Arrays.asList(elements))).isTrue();
	}

	@Test
	public void asSetContainsUniqueArrayElements() {

		Object[] elements = { 1, 2, 1 };

		Set<?> set = CollectionUtils.asSet(elements);

		assertThat(set).isNotNull();
		assertThat(set.size()).isEqualTo(2);
		assertThat(set.containsAll(Arrays.asList(elements))).isTrue();
	}

	@Test(expected = UnsupportedOperationException.class)
	public void asSetReturnsUnmodifiableSet() {

		Set<Integer> set = CollectionUtils.asSet(1, 2, 3);

		assertThat(set).isNotNull();
		assertThat(set.size()).isEqualTo(3);

		try {
			set.add(4);
			set.remove(1);
			set.remove(2);
		}
		catch (UnsupportedOperationException e) {
			assertThat(set.size()).isEqualTo(3);
			throw e;
		}
	}

	@Test
	public void containsAnyWithCollectionAndArrayIsTrue() {
		assertThat(CollectionUtils.containsAny(Arrays.asList(1, 2, 3), 1, 2)).isTrue();
	}

	@Test
	public void containsAnyWithCollectionAndArrayIsFalse() {
		assertThat(CollectionUtils.containsAny(Arrays.asList(1, 2, 3), 4)).isFalse();
	}

	@Test
	public void containsAnyWithCollectionAndNullArrayIsFalse() {
		assertThat(CollectionUtils.containsAny(Arrays.asList(1, 2, 3), (Object[]) null));
	}

	@Test
	public void containsAnyWithNullCollectionAndArrayIsFalse() {
		assertThat(CollectionUtils.containsAny(null, 1)).isFalse();
	}

	@Test
	public void containsAnyWithNullCollectionAndNullArrayIsFalse() {
		assertThat(CollectionUtils.containsAny(null, (Object[]) null)).isFalse();
	}

	@Test
	public void emptyIterableReturnsEmptyIterable() {

		Iterable<?> iterable = CollectionUtils.emptyIterable();

		assertThat(iterable).isNotNull();
		assertThat(iterable.iterator()).isNotNull();
		assertThat(iterable.iterator().hasNext()).isFalse();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void iterableOfEnumeration() {

		Enumeration<Object> mockEnumeration = mock(Enumeration.class, "MockEnumeration");

		when(mockEnumeration.hasMoreElements()).thenReturn(true).thenReturn(true).thenReturn(true).thenReturn(false);
		when(mockEnumeration.nextElement()).thenReturn(1).thenReturn(2).thenReturn(3)
			.thenThrow(new NoSuchElementException("Enumeration exhausted"));

		Iterable<Object> iterable = CollectionUtils.iterable(mockEnumeration);

		assertThat(iterable).isNotNull();
		//assertThat(iterable).containsExactly(1, 2, 3);
		LinkedHashSet<Object> set = StreamSupport.stream(iterable.spliterator(), false).collect(Collectors.toCollection(LinkedHashSet::new));
		assertThat(set)
			.containsExactly(1, 2, 3);

		verify(mockEnumeration, times(4)).hasMoreElements();
		verify(mockEnumeration, times(3)).nextElement();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void iterableOfSingleEnumeration() {

		Enumeration<Object> mockEnumeration = mock(Enumeration.class);

		when(mockEnumeration.hasMoreElements()).thenReturn(true).thenReturn(false);
		when(mockEnumeration.nextElement()).thenReturn(1)
			.thenThrow(new NoSuchElementException("Enumeration exhausted"));

		Iterable<Object> iterable = CollectionUtils.iterable(mockEnumeration);

		assertThat(iterable).isNotNull();
		//assertThat(iterable).containsExactly(1);
		assertThat(StreamSupport.stream(iterable.spliterator(), false).collect(Collectors.toSet()))
			.containsExactly(1);

		verify(mockEnumeration, times(2)).hasMoreElements();
		verify(mockEnumeration, times(1)).nextElement();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void iterableOfNullEnumeration() {

		Iterable<?> iterable = CollectionUtils.iterable((Enumeration<?>) null);

		assertThat(iterable).isNotNull();
		assertThat(iterable).isEmpty();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void iterableOfIterator() {

		Iterator<Object> mockIterator = mock(Iterator.class, "MockIterator");

		when(mockIterator.hasNext()).thenReturn(true).thenReturn(true).thenReturn(true).thenReturn(false);
		when(mockIterator.next()).thenReturn(1).thenReturn(2).thenReturn(3)
			.thenThrow(new NoSuchElementException("Iterator exhausted"));

		Iterable<Object> iterable = CollectionUtils.iterable(mockIterator);

		assertThat(iterable).isNotNull();

		Set<Object> set = new LinkedHashSet<>();

		iterable.forEach(set::add);

		assertThat(set).hasSize(3);
		assertThat(set).containsExactly(1, 2, 3);

		verify(mockIterator, times(4)).hasNext();
		verify(mockIterator, times(3)).next();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void iterableOfSingleIterator() {

		Iterator<Object> mockIterator = mock(Iterator.class, "MockIterator");

		when(mockIterator.hasNext()).thenReturn(true).thenReturn(false);
		when(mockIterator.next()).thenReturn(1).thenThrow(new NoSuchElementException("Iterator exhausted"));

		Iterable<Object> iterable = CollectionUtils.iterable(mockIterator);

		assertThat(iterable).isNotNull();

		Set<Object> set = new HashSet<>();

		iterable.forEach(set::add);

		assertThat(set).hasSize(1);
		assertThat(set).containsExactly(1);

		verify(mockIterator, times(2)).hasNext();
		verify(mockIterator, times(1)).next();
	}

	@Test
	public void iterableOfNullIterator() {

		Iterable<?> iterable = CollectionUtils.iterable((Iterator<?>) null);

		assertThat(iterable).isNotNull();
		assertThat(iterable).isEmpty();
	}

	@Test
	public void nullSafeCollectionWithNonNullCollection() {

		Collection<?> mockCollection = mock(Collection.class);

		assertThat(CollectionUtils.nullSafeCollection(mockCollection)).isSameAs(mockCollection);
	}

	@Test
	public void nullSafeCollectionWithNullCollection() {

		Collection collection = CollectionUtils.nullSafeCollection(null);

		assertThat(collection).isNotNull();
		assertThat(collection.isEmpty()).isTrue();
	}

	@Test
	public void nullSafeEnumerationWithNonNullEnumeration() {

		Enumeration<?> mockEnumeration = mock(Enumeration.class);

		assertThat(CollectionUtils.nullSafeEnumeration(mockEnumeration)).isSameAs(mockEnumeration);
	}

	@Test
	public void nullSafeEnumerationWithNullEnumeration() {

		Enumeration<?> enumeration = CollectionUtils.nullSafeEnumeration(null);

		assertThat(enumeration).isNotNull();
		assertThat(enumeration.hasMoreElements()).isFalse();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void nullSafeIterableWithNonNullIterable() {

		Iterable<Object> mockIterable = mock(Iterable.class);

		assertThat(CollectionUtils.nullSafeIterable(mockIterable)).isSameAs(mockIterable);
	}

	@Test
	public void nullSafeIterableWithNullIterable() {

		Iterable<Object> iterable = CollectionUtils.nullSafeIterable(null);

		assertThat(iterable).isNotNull();
		assertThat(iterable.iterator()).isNotNull();
		assertThat(iterable.iterator().hasNext()).isFalse();
	}

	@SuppressWarnings("all")
	@Test(expected = IllegalStateException.class)
	public void nullSafeIterableIterator() {

		Iterable<Object> iterable = CollectionUtils.nullSafeIterable(null);

		assertThat(iterable).isNotNull();

		Iterator<Object> iterator = iterable.iterator();

		assertThat(iterator).isNotNull();
		assertThat(iterator.hasNext()).isFalse();

		try {
			iterator.next();
		}
		catch (NoSuchElementException ignore) {
			assertThat(ignore.getCause()).isNull();

			try {
				iterator.remove();
			}
			catch (IllegalStateException expected) {
				assertThat(expected.getCause()).isNull();

				throw expected;
			}
		}
	}

	@Test
	public void nullSafeIterableWithNonNullNonEmptyIterableReturnsIterable() {

		Iterable<Object> iterable = Collections.singleton(1);
		Iterable<Object> defaultIterable = Collections.singleton(2);

		assertThat(CollectionUtils.nullSafeIterable(iterable, defaultIterable)).isSameAs(iterable);
	}

	@Test
	public void nullSafeIterableWithEmptyIterableReturnsDefault() {

		Iterable<Object> iterable = Collections.emptySet();
		Iterable<Object> defaultIterable = Collections.singleton(2);

		assertThat(CollectionUtils.nullSafeIterable(iterable, defaultIterable)).isSameAs(defaultIterable);
	}

	@Test
	public void nullSafeIterableWithNullIterableReturnsDefault() {

		Iterable<?> defaultIterable = Collections.singleton(2);

		assertThat(CollectionUtils.nullSafeIterable(null, defaultIterable)).isSameAs(defaultIterable);
	}

	@Test
	public void nullSafeIterableWithNullIterableAndNullDefaultReturnsNull() {
		assertThat(CollectionUtils.nullSafeIterable((Iterable<?>) null, null)).isNull();
	}

	@Test
	public void nullSafeIteratorWithNonNullIterator() {

		Iterator<?> mockIterator = mock(Iterator.class);

		assertThat(CollectionUtils.nullSafeIterator(mockIterator)).isSameAs(mockIterator);
	}

	@Test
	public void nullSafeIteratorWithNullIterator() {

		Iterator<?> iterator = CollectionUtils.nullSafeIterator(null);

		assertThat(iterator).isNotNull();
		assertThat(iterator).isExhausted();
	}

	@Test
	public void nullSafeListWithNonNullList() {

		List<?> mockList = mock(List.class);

		assertThat(CollectionUtils.nullSafeList(mockList)).isSameAs(mockList);
	}

	@Test
	public void nullSafeListWithNullList() {

		List<?> list = CollectionUtils.nullSafeList(null);

		assertThat(list).isNotNull();
		assertThat(list.isEmpty()).isTrue();
	}

	@Test
	public void nullSafeMapWithNonNullMap() {

		Map<?, ?> mockMap = mock(Map.class);

		assertThat(CollectionUtils.nullSafeMap(mockMap)).isSameAs(mockMap);
	}

	@Test
	public void nullSafeMapWithNullMap() {

		Map<?, ?> map = CollectionUtils.nullSafeMap(null);

		assertThat(map).isNotNull();
		assertThat(map.isEmpty()).isTrue();
	}

	@Test
	public void nullSafeSetWithNonNullSet() {

		Set<?> mockSet = mock(Set.class);

		assertThat(CollectionUtils.nullSafeSet(mockSet)).isSameAs(mockSet);
	}

	@Test
	public void nullSafeSetWithNullSet() {

		Set<?> set = CollectionUtils.nullSafeSet(null);

		assertThat(set).isNotNull();
		assertThat(set.isEmpty()).isTrue();
	}

	@Test
	public void nullSafeIsEmptyCollectionWithNonNulNonEmptyCollectionReturnsFalse() {
		assertThat(CollectionUtils.nullSafeIsEmpty(Collections.singleton(1))).isFalse();
		assertThat(CollectionUtils.nullSafeIsEmpty(Collections.singletonList(1))).isFalse();
	}

	@Test
	public void nullSafeIsEmptyCollectionWithEmptyCollectionReturnsTrue() {
		assertThat(CollectionUtils.isEmpty(Collections.emptyList())).isTrue();
		assertThat(CollectionUtils.isEmpty(Collections.emptySet())).isTrue();
	}

	@Test
	public void nullSafeIsEmptyCollectionWithNullCollectionReturnsTrue() {
		assertThat(CollectionUtils.isEmpty((Collection<?>) null)).isTrue();
	}

	@Test
	public void nullSafeIsEmptyMapWithNonNullNonEmptyMapReturnsFalse() {
		assertThat(CollectionUtils.isEmpty(Collections.singletonMap("key", "value"))).isFalse();
	}

	@Test
	public void nullSafeIsEmptyMapWithEmptyMapReturnsTrue() {
		assertThat(CollectionUtils.isEmpty(Collections.emptyMap())).isTrue();
	}

	@Test
	public void nullSafeIsEmptyMapWithNullMapReturnsTrue() {
		assertThat(CollectionUtils.isEmpty((Map<?, ?>) null)).isTrue();
	}

	@Test
	public void nullSafeCollectionSizeWithNonNullNonEmptyCollectionReturnsSize() {
		assertThat(CollectionUtils.nullSafeSize(Collections.singleton(1))).isEqualTo(1);
		assertThat(CollectionUtils.nullSafeSize(Collections.singletonList(1))).isEqualTo(1);
	}

	@Test
	public void nullSafeCollectionSizeWithEmptyCollectionReturnsZero() {
		assertThat(CollectionUtils.nullSafeSize(Collections.emptyList())).isZero();
		assertThat(CollectionUtils.nullSafeSize(Collections.emptySet())).isZero();
	}

	@Test
	public void nullSafeCollectionSizeWithNullCollectionReturnsZero() {
		assertThat(CollectionUtils.nullSafeSize((Collection<?>) null)).isZero();
	}

	@Test
	public void nullSafeMapSizeWithNonNullNonEmptyMapReturnsSize() {
		assertThat(CollectionUtils.nullSafeSize(Collections.singletonMap("key", "value"))).isEqualTo(1);
	}

	@Test
	public void nullSafeMapSizeWithEmptyMapReturnsZero() {
		assertThat(CollectionUtils.nullSafeSize(Collections.emptyMap())).isZero();
	}

	@Test
	public void nullSafeMapSizeWithNullMapReturnsZero() {
		assertThat(CollectionUtils.nullSafeSize((Map<?, ?>) null)).isZero();
	}

	@Test
	public void sortIsSuccessful() {

		List<Integer> list = new ArrayList<>(Arrays.asList(2, 3, 1));
		List<Integer> sortedList = CollectionUtils.sort(list);

		assertThat(sortedList).isSameAs(list);
		assertThat(sortedList).isEqualTo(Arrays.asList(1, 2, 3));
	}

	@Test(expected = IllegalArgumentException.class)
	public void sortWithNullListThrowsIllegalArgumentException() {

		try {
			CollectionUtils.sort(null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("List is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void subListOfListWithIndexesReturnsSubList() {

		List<Integer> list = Arrays.asList(0, 1, 2, 3);
		List<Integer> subList = CollectionUtils.subList(list, 1, 3);

		assertThat(subList).isNotNull();
		assertThat(subList).isNotSameAs(list);
		assertThat(subList.size()).isEqualTo(2);
		assertThat(subList).containsAll(Arrays.asList(1, 3));
	}

	@Test(expected = IndexOutOfBoundsException.class)
	public void subListOfListWithInvalidIndexThrowsIndexOutOfBoundsException() {
		CollectionUtils.subList(Arrays.asList(0, 1, 2), 1, 3);
	}

	@Test
	public void subListOfListWithNoIndexesReturnsEmptyList() {

		List<Integer> subList = CollectionUtils.subList(Arrays.asList(0, 1, 2));

		assertThat(subList).isNotNull();
		assertThat(subList.isEmpty()).isTrue();
	}

	@Test(expected = IllegalArgumentException.class)
	public void subListOfNullListThrowsIllegalArgumentException() {

		try {
			CollectionUtils.subList(null, 1, 2, 3);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("List is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void toStringOfMultiEntryMap() {

		Map<String, String> map = MapBuilder.<String, String>newMapBuilder()
			.put("keyOne", "valueOne")
			.put("keyTwo", "valueTwo")
			.build();

		String mapString = CollectionUtils.toString(map);

		assertThat(mapString).isNotNull();
		assertThat(mapString).isEqualTo("{\n\tkeyOne = valueOne,\n\tkeyTwo = valueTwo\n}");
	}

	@Test
	public void toStringOfSingleEntryMap() {

		String mapString = CollectionUtils.toString(Collections.singletonMap("key", "value"));

		assertThat(mapString).isNotNull();
		assertThat(mapString).isEqualTo("{\n\tkey = value\n}");
	}

	@Test
	public void toStringOfEmptyMap() {

		String mapString = CollectionUtils.toString(Collections.emptyMap());

		assertThat(mapString).isNotNull();
		assertThat(mapString).isEqualTo("{\n\n}");
	}

	@Test
	public void toStringOfNullMap() {

		String mapString = CollectionUtils.toString(null);

		assertThat(mapString).isNotNull();
		assertThat(mapString).isEqualTo("{\n\n}");
	}
}
