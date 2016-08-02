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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
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
import java.util.NoSuchElementException;

import org.junit.Test;

/**
 * The CollectionUtilsTest class is a test suite of test cases testing the contract and functionality
 * of the CollectionUtils class.
 *
 * @author John Blum
 * @see java.util.Collection
 * @see java.util.Enumeration
 * @see java.util.Iterator
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.util.CollectionUtils
 * @since 1.7.0
 */
public class CollectionUtilsTest {

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
		List<?> mockList = mock(List.class);

		assertSame(mockList, CollectionUtils.nullSafeCollection(mockList));
	}

	@Test
	public void nullSafeCollectionWithNullCollection() {
		Collection collection = CollectionUtils.nullSafeCollection(null);

		assertNotNull(collection);
		assertTrue(collection.isEmpty());
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

		assertThat(iterable, is(not(nullValue())));
		assertThat(iterable.iterator(), is(not(nullValue())));
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
}
