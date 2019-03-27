/*
 * Copyright 2010-2013 the original author or authors.
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

package org.springframework.data.gemfire.repository.query;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import org.junit.Test;

import com.gemstone.gemfire.cache.query.SelectResults;
import com.gemstone.gemfire.cache.query.internal.ResultsBag;

/**
 * The SpringBasedGemfireRepositoryQueryTest class is a test suite of test cases testing the contract and functionality
 * of the StringBasedGemfireRepositoryQuery class.
 * <p/>
 * @author John Blum
 * @see org.mockito.Mockito
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.repository.query.StringBasedGemfireRepositoryQuery
 * @since 1.4.0
 */
public class StringBasedGemfireRepositoryQueryTest {

	private final StringBasedGemfireRepositoryQuery repositoryQuery = new StringBasedGemfireRepositoryQuery();

	@Test
	public void testToCollectionWithSelectResults() {
		SelectResults mockSelectResults = mock(SelectResults.class, "testToCollectionWithSelectResults.SelectResults");
		List<String> expectedList = Arrays.asList("one", "two", "three");

		when(mockSelectResults.asList()).thenReturn(expectedList);

		Collection<?> actualList = repositoryQuery.toCollection(mockSelectResults);

		assertSame(expectedList, actualList);
	}

	@Test
	public void testToCollectionWithResultsBag() {
		ResultsBag mockResultsBag = mock(ResultsBag.class, "testToCollectionWithResultsBag.ResultsBag");
		List<String> expectedList = Arrays.asList("a", "b", "c");

		when(mockResultsBag.asList()).thenReturn(expectedList);

		Collection<?> actualList = repositoryQuery.toCollection(mockResultsBag);

		assertSame(expectedList, actualList);
	}

	@Test
	public void testToCollectionWithCollection() {
		List<String> expectedList = Arrays.asList("x", "y", "z");
		Collection<?> actualList = repositoryQuery.toCollection(expectedList);

		assertSame(expectedList, actualList);
	}

	@Test
	public void testToCollectionWithArray() {
		Object[] array = { 1, 2, 3 };
		Collection<?> list = repositoryQuery.toCollection(array);

		assertNotNull(list);
		assertNotSame(array, list);
		assertTrue(list instanceof List);
		assertEquals(array.length, list.size());
		assertTrue(list.containsAll(Arrays.asList(array)));
	}

	@Test
	public void testToCollectionWithSingleObject() {
		Collection<?> list = repositoryQuery.toCollection("test");

		assertTrue(list instanceof List);
		assertFalse(list.isEmpty());
		assertEquals(1, list.size());
		assertEquals("test", ((List) list).get(0));
	}

	@Test
	public void testToCollectionWithNull() {
		Collection<?> list = repositoryQuery.toCollection(null);

		assertNotNull(list);
		assertTrue(list.isEmpty());
	}

}
