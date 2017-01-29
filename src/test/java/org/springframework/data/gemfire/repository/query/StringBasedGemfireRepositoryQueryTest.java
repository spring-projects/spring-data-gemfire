/*
 * Copyright 2010-2018 the original author or authors.
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

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.apache.geode.cache.query.SelectResults;
import org.apache.geode.cache.query.internal.ResultsBag;
import org.junit.Test;

/**
 * The SpringBasedGemfireRepositoryQueryTest class is a test suite of test cases testing the contract and functionality
 * of the StringBasedGemfireRepositoryQuery class.
 *
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

	@Test
	public void applyAllQueryAnnotationExtensions() {
		GemfireQueryMethod mockQueryMethod = mock(GemfireQueryMethod.class, "MockGemfireQueryMethod");

		when(mockQueryMethod.hasHint()).thenReturn(true);
		when(mockQueryMethod.getHints()).thenReturn(Arrays.asList("IdIdx", "NameIdx").toArray(new String[2]));
		when(mockQueryMethod.hasImport()).thenReturn(true);
		when(mockQueryMethod.getImport()).thenReturn("org.example.domain.Type");
		when(mockQueryMethod.hasLimit()).thenReturn(true);
		when(mockQueryMethod.getLimit()).thenReturn(10);
		when(mockQueryMethod.hasTrace()).thenReturn(true);

		QueryString queryString = new QueryString("SELECT * FROM /Example");

		assertThat(queryString.toString(), is(equalTo("SELECT * FROM /Example")));

		StringBasedGemfireRepositoryQuery repositoryQuery = new StringBasedGemfireRepositoryQuery();

		QueryString actualQueryString = repositoryQuery.applyQueryAnnotationExtensions(mockQueryMethod, queryString);

		assertThat(actualQueryString, is(notNullValue()));
		assertThat(actualQueryString, is(not(sameInstance(queryString))));
		assertThat(actualQueryString.toString(), is(equalTo(
			"<TRACE> <HINT 'IdIdx', 'NameIdx'> IMPORT org.example.domain.Type; SELECT * FROM /Example LIMIT 10")));

		verify(mockQueryMethod, times(1)).hasHint();
		verify(mockQueryMethod, times(1)).getHints();
		verify(mockQueryMethod, times(1)).hasImport();
		verify(mockQueryMethod, times(1)).getImport();
		verify(mockQueryMethod, times(1)).hasLimit();
		verify(mockQueryMethod, times(1)).getLimit();
		verify(mockQueryMethod, times(1)).hasTrace();
	}

	@Test
	public void applyHintLimitAndTraceQueryAnnotationExtensionsWithExistingHintAndLimit() {
		GemfireQueryMethod mockQueryMethod = mock(GemfireQueryMethod.class, "MockGemfireQueryMethod");

		when(mockQueryMethod.hasHint()).thenReturn(true);
		when(mockQueryMethod.getHints()).thenReturn(Collections.singletonList("FirstNameIdx").toArray(new String[1]));
		when(mockQueryMethod.hasImport()).thenReturn(false);
		when(mockQueryMethod.hasLimit()).thenReturn(true);
		when(mockQueryMethod.getLimit()).thenReturn(50);
		when(mockQueryMethod.hasTrace()).thenReturn(true);

		QueryString queryString = new QueryString("<HINT 'LastNameIdx'> SELECT * FROM /Example LIMIT 25");

		assertThat(queryString.toString(), is(equalTo("<HINT 'LastNameIdx'> SELECT * FROM /Example LIMIT 25")));

		StringBasedGemfireRepositoryQuery repositoryQuery = new StringBasedGemfireRepositoryQuery();

		QueryString actualQueryString = repositoryQuery.applyQueryAnnotationExtensions(mockQueryMethod, queryString);

		assertThat(actualQueryString, is(notNullValue()));
		assertThat(actualQueryString, is(not(sameInstance(queryString))));
		assertThat(actualQueryString.toString(), is(equalTo(
			"<TRACE> <HINT 'LastNameIdx'> SELECT * FROM /Example LIMIT 25")));

		verify(mockQueryMethod, times(1)).hasHint();
		verify(mockQueryMethod, never()).getHints();
		verify(mockQueryMethod, times(1)).hasImport();
		verify(mockQueryMethod, never()).getImport();
		verify(mockQueryMethod, times(1)).hasLimit();
		verify(mockQueryMethod, never()).getLimit();
		verify(mockQueryMethod, times(1)).hasTrace();
	}

	@Test
	public void applyImportAndTraceQueryAnnotationExtensionsWithExistingTrace() {
		GemfireQueryMethod mockQueryMethod = mock(GemfireQueryMethod.class, "MockGemfireQueryMethod");

		when(mockQueryMethod.hasHint()).thenReturn(false);
		when(mockQueryMethod.hasImport()).thenReturn(true);
		when(mockQueryMethod.getImport()).thenReturn("org.example.domain.Type");
		when(mockQueryMethod.hasLimit()).thenReturn(false);
		when(mockQueryMethod.hasTrace()).thenReturn(true);

		QueryString queryString = new QueryString("<TRACE> SELECT * FROM /Example");

		assertThat(queryString.toString(), is(equalTo("<TRACE> SELECT * FROM /Example")));

		StringBasedGemfireRepositoryQuery repositoryQuery = new StringBasedGemfireRepositoryQuery();

		QueryString actualQueryString = repositoryQuery.applyQueryAnnotationExtensions(mockQueryMethod, queryString);

		assertThat(actualQueryString, is(notNullValue()));
		assertThat(actualQueryString, is(not(sameInstance(queryString))));
		assertThat(actualQueryString.toString(), is(equalTo(
			"IMPORT org.example.domain.Type; <TRACE> SELECT * FROM /Example")));

		verify(mockQueryMethod, times(1)).hasHint();
		verify(mockQueryMethod, never()).getHints();
		verify(mockQueryMethod, times(1)).hasImport();
		verify(mockQueryMethod, times(1)).getImport();
		verify(mockQueryMethod, times(1)).hasLimit();
		verify(mockQueryMethod, never()).getLimit();
		verify(mockQueryMethod, times(1)).hasTrace();
	}

}
