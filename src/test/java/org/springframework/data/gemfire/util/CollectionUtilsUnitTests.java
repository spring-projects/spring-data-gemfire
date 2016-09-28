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

package org.springframework.data.gemfire.util;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link CollectionUtils}.
 *
 * @author John Blum
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

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Test
	public void asSetContainsAllArrayElements() {
		Object[] elements = { "a", "b", "c" };

		Set<?> set = CollectionUtils.asSet(elements);

		assertThat(set).isNotNull();
		assertThat(set.size()).isEqualTo(elements.length);
		assertThat(set).containsAll(Arrays.asList(elements));
	}

	@Test
	public void asSetContainsUniqueArrayElements() {
		Object[] elements = { 1, 2, 1 };

		Set<?> set = CollectionUtils.asSet(elements);

		assertThat(set).isNotNull();
		assertThat(set.size()).isEqualTo(2);
		assertThat(set).containsAll(Arrays.asList(elements));
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
	@SuppressWarnings("unchecked")
	public void iterableEnumeration() {
		Enumeration<String> mockEnumeration = mock(Enumeration.class, "MockEnumeration");

		when(mockEnumeration.hasMoreElements()).thenReturn(true).thenReturn(true).thenReturn(true).thenReturn(false);
		when(mockEnumeration.nextElement()).thenReturn("zero").thenReturn("one").thenReturn("two")
			.thenThrow(new NoSuchElementException("Enumeration exhausted"));

		Iterable<String> iterable = CollectionUtils.iterable(mockEnumeration);

		assertThat(iterable).isNotNull();

		List<String> actualList = new ArrayList<String>(3);

		for (String element : iterable) {
			actualList.add(element);
		}

		assertThat(actualList).isEqualTo(Arrays.asList("zero", "one", "two"));

		verify(mockEnumeration, times(4)).hasMoreElements();
		verify(mockEnumeration, times(3)).nextElement();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void iterableIterator() {
		Iterator<String> mockIterator = mock(Iterator.class, "MockIterator");

		when(mockIterator.hasNext()).thenReturn(true).thenReturn(true).thenReturn(true).thenReturn(false);
		when(mockIterator.next()).thenReturn("zero").thenReturn("one").thenReturn("two")
			.thenThrow(new NoSuchElementException("Iterator exhausted"));

		Iterable<String> iterable = CollectionUtils.iterable(mockIterator);

		assertThat(iterable).isNotNull();

		List<String> actualList = new ArrayList<String>(3);

		for (String element : iterable) {
			actualList.add(element);
		}

		assertThat(actualList).containsAll(Arrays.asList("zero", "one", "two"));

		verify(mockIterator, times(4)).hasNext();
		verify(mockIterator, times(3)).next();
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

	@Test(expected = UnsupportedOperationException.class)
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
			assertThat(ignore.getMessage()).isEqualTo("No more elements");
			assertThat(ignore.getCause()).isNull();

			try {
				iterator.remove();
			}
			catch (UnsupportedOperationException expected) {
				assertThat(expected.getMessage()).isEqualTo("Operation not supported");
				assertThat(expected.getCause()).isNull();

				throw expected;
			}
		}
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
	public void sortIsSuccessful() {
		List<Integer> list = new ArrayList<Integer>(Arrays.asList(2, 3, 1));
		List<Integer> sortedList = CollectionUtils.sort(list);

		assertThat(sortedList).isSameAs(list);
		assertThat(sortedList).isEqualTo(Arrays.asList(1, 2, 3));
	}

	@Test
	public void subListFromListWithIndexesReturnsSubList() {
		List<Integer> list = Arrays.asList(0, 1, 2, 3);
		List<Integer> subList = CollectionUtils.subList(list, 1, 3);

		assertThat(subList).isNotNull();
		assertThat(subList).isNotSameAs(list);
		assertThat(subList.size()).isEqualTo(2);
		assertThat(subList).containsAll(Arrays.asList(1, 3));
	}

	@Test
	public void subListFromListWithNoIndexesReturnsEmptyList() {
		List<Integer> subList = CollectionUtils.subList(Arrays.asList(0, 1, 2));

		assertThat(subList).isNotNull();
		assertThat(subList.isEmpty()).isTrue();
	}

	@Test(expected = IndexOutOfBoundsException.class)
	public void subListFromListWithInvalidIndexThrowsIndexOutOfBoundsException() {
		CollectionUtils.subList(Arrays.asList(0, 1, 2), 1, 3);
	}

	@Test
	public void subListWithNullSourceListThrowsIllegalArgumentException() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("List must not be null");

		CollectionUtils.subList(null, 1, 2, 3);
	}
}
