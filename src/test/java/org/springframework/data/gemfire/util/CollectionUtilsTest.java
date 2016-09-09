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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
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

import org.junit.Test;

/**
 * Test suite of test cases testing the contract and functionality of the {@link CollectionUtils} class.
 *
 * @author John Blum
 * @see java.util.Collection
 * @see java.util.Collections
 * @see java.util.List
 * @see java.util.Enumeration
 * @see java.util.Iterator
 * @see java.util.Map
 * @see java.util.Set
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.util.CollectionUtils
 * @since 1.7.0
 */
public class CollectionUtilsTest {

	@Test
	public void asSetContainsAllArrayElements() {
		Object[] elements = { "a", "b", "c" };

		Set<?> set = CollectionUtils.asSet(elements);

		assertThat(set, is(notNullValue(Set.class)));
		assertThat(set.size(), is(equalTo(elements.length)));
		assertThat(set.containsAll(Arrays.asList(elements)), is(true));
	}

	@Test
	public void asSetContainsUniqueArrayElements() {
		Object[] elements = { 1, 2, 1 };

		Set<?> set = CollectionUtils.asSet(elements);

		assertThat(set, is(notNullValue(Set.class)));
		assertThat(set.size(), is(equalTo(2)));
		assertThat(set.containsAll(Arrays.asList(elements)), is(true));
	}

	@Test(expected = UnsupportedOperationException.class)
	public void asSetReturnsUnmodifiableSet() {
		Set<Integer> set = CollectionUtils.asSet(1, 2, 3);

		assertThat(set, is(notNullValue(Set.class)));
		assertThat(set.size(), is(equalTo(3)));

		try {
			set.add(4);
			set.remove(1);
			set.remove(2);
		}
		catch (UnsupportedOperationException e) {
			assertThat(set.size(), is(equalTo(3)));
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

		assertThat(iterable, is(notNullValue()));

		List<String> actualList = new ArrayList<String>(3);

		for (String element : iterable) {
			actualList.add(element);
		}

		assertThat(actualList, is(equalTo(Arrays.asList("zero", "one", "two"))));

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

		assertThat(iterable, is(notNullValue()));

		List<String> actualList = new ArrayList<String>(3);

		for (String element : iterable) {
			actualList.add(element);
		}

		assertThat(actualList, is(equalTo(Arrays.asList("zero", "one", "two"))));

		verify(mockIterator, times(4)).hasNext();
		verify(mockIterator, times(3)).next();
	}

	@Test
	public void nullSafeCollectionWithNonNullCollection() {
		Collection<?> mockCollection = mock(Collection.class);

		assertSame(mockCollection, CollectionUtils.nullSafeCollection(mockCollection));
	}

	@Test
	public void nullSafeCollectionWithNullCollection() {
		Collection collection = CollectionUtils.nullSafeCollection(null);

		assertThat(collection, is(notNullValue(Collection.class)));
		assertThat(collection.isEmpty(), is(true));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void nullSafeIterableWithNonNullIterable() {
		Iterable<Object> mockIterable = mock(Iterable.class);

		assertThat(CollectionUtils.nullSafeIterable(mockIterable), is(sameInstance(mockIterable)));
	}

	@Test
	public void nullSafeIterableWithNullIterable() {
		Iterable<Object> iterable = CollectionUtils.nullSafeIterable(null);

		assertThat(iterable, is(not(nullValue(Iterable.class))));
		assertThat(iterable.iterator(), is(not(nullValue(Iterator.class))));
	}

	@Test(expected = UnsupportedOperationException.class)
	public void nullSafeIterableIterator() {
		Iterator<Object> iterator = CollectionUtils.nullSafeIterable(null).iterator();

		assertThat(iterator, is(not(nullValue())));
		assertThat(iterator.hasNext(), is(equalTo(false)));

		try {
			iterator.next();
		}
		catch (NoSuchElementException ignore) {
			assertThat(ignore.getMessage(), is(equalTo("No more elements")));
			assertThat(ignore.getCause(), is(nullValue()));

			try {
				iterator.remove();
			}
			catch (UnsupportedOperationException expected) {
				assertThat(expected.getMessage(), is(equalTo("Operation not supported")));
				assertThat(expected.getCause(), is(nullValue()));
				throw expected;
			}
		}
	}

	@Test
	public void nullSafeListWithNonNullList() {
		List<?> mockList = mock(List.class);

		assertSame(mockList, CollectionUtils.nullSafeList(mockList));
	}

	@Test
	public void nullSafeListWithNullList() {
		List<?> list = CollectionUtils.nullSafeList(null);

		assertThat(list, is(notNullValue(List.class)));
		assertThat(list.isEmpty(), is(true));
	}

	@Test
	public void nullSafeMapWithNonNullMap() {
		Map<?, ?> mockMap = mock(Map.class);

		assertSame(mockMap, CollectionUtils.nullSafeMap(mockMap));
	}

	@Test
	public void nullSafeMapWithNullMap() {
		Map<?, ?> map = CollectionUtils.nullSafeMap(null);

		assertThat(map, is(notNullValue(Map.class)));
		assertThat(map.isEmpty(), is(true));
	}

	@Test
	public void nullSafeSetWithNonNullSet() {
		Set<?> mockSet = mock(Set.class);

		assertSame(mockSet, CollectionUtils.nullSafeSet(mockSet));
	}

	@Test
	public void nullSafeSetWithNullSet() {
		Set<?> set = CollectionUtils.nullSafeSet(null);

		assertThat(set, is(notNullValue(Set.class)));
		assertThat(set.isEmpty(), is(true));
	}
}
