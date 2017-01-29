/*
 * Copyright 2016-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.springframework.data.gemfire.search.lucene;

import static java.util.Arrays.asList;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.isA;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.search.lucene.LuceneAccessor.LuceneQueryExecutor;

import java.util.Collection;
import java.util.List;

import org.apache.geode.cache.lucene.LuceneQuery;
import org.apache.geode.cache.lucene.LuceneQueryException;
import org.apache.geode.cache.lucene.LuceneQueryFactory;
import org.apache.geode.cache.lucene.LuceneQueryProvider;
import org.apache.geode.cache.lucene.LuceneResultStruct;
import org.apache.geode.cache.lucene.LuceneService;
import org.apache.geode.cache.lucene.PageableLuceneQueryResults;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link LuceneTemplate}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.search.lucene.LuceneTemplate
 * @see org.apache.geode.cache.lucene.LuceneQuery
 * @see org.apache.geode.cache.lucene.LuceneQueryFactory
 * @see org.apache.geode.cache.lucene.LuceneQueryProvider
 * @see org.apache.geode.cache.lucene.LuceneResultStruct
 * @see org.apache.geode.cache.lucene.LuceneService
 * @see org.apache.geode.cache.lucene.PageableLuceneQueryResults
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class LuceneTemplateUnitTests {

	@Mock
	private LuceneQuery<Object, Object> mockLuceneQuery;

	@Mock
	private LuceneQueryFactory mockLuceneQueryFactory;

	@Mock
	private LuceneQueryProvider mockLuceneQueryProvider;

	@Mock
	private LuceneResultStruct<Object, Object> mockLuceneResultStructOne;

	@Mock
	private LuceneResultStruct<Object, Object> mockLuceneResultStructTwo;

	@Mock
	private LuceneService mockLuceneService;

	@Mock
	private PageableLuceneQueryResults<Object, Object> mockPageableLuceneQueryResults;

	@Spy
	private LuceneTemplate luceneTemplate;

	@Before
	@SuppressWarnings("deprecation")
	public void setup() {
		luceneTemplate.setLuceneService(mockLuceneService);

		when(mockLuceneService.createLuceneQueryFactory()).thenReturn(mockLuceneQueryFactory);
		when(mockLuceneQueryFactory.setPageSize(anyInt())).thenReturn(mockLuceneQueryFactory);
		when(mockLuceneQueryFactory.setLimit(anyInt())).thenReturn(mockLuceneQueryFactory);
	}

	@Test
	@SuppressWarnings({ "deprecation", "unchecked" })
	public void stringQueryReturnsList() throws LuceneQueryException {
		when(mockLuceneQueryFactory.create(eq("TestIndex"), eq("/Example"), anyString(), anyString()))
			.thenReturn(mockLuceneQuery);
		when(mockLuceneQuery.findResults()).thenReturn(asList(mockLuceneResultStructOne, mockLuceneResultStructTwo));

		doReturn("TestIndex").when(luceneTemplate).resolveIndexName();
		doReturn("/Example").when(luceneTemplate).resolveRegionPath();

		List<LuceneResultStruct<Object, Object>> results =  luceneTemplate.query(
			"title : Up Shit Creek Without a Paddle", "title", 100);

		assertThat(results).isNotNull();
		assertThat(results).hasSize(2);
		assertThat(results).containsAll(asList(mockLuceneResultStructOne, mockLuceneResultStructTwo));

		verify(luceneTemplate, times(1)).resolveIndexName();
		verify(luceneTemplate, times(1)).resolveRegionPath();
		verify(luceneTemplate, times(1)).doFind(isA(LuceneQueryExecutor.class),
			eq("title : Up Shit Creek Without a Paddle"), eq("/Example"), eq("TestIndex"));
		verify(mockLuceneService, times(1)).createLuceneQueryFactory();
		verify(mockLuceneQueryFactory, times(1)).setLimit(eq(100));
		verify(mockLuceneQueryFactory, times(1)).setPageSize(eq(LuceneOperations.DEFAULT_PAGE_SIZE));
		verify(mockLuceneQueryFactory, times(1)).create(eq("TestIndex"),
			eq("/Example"), eq("title : Up Shit Creek Without a Paddle"), eq("title"));
		verify(mockLuceneQuery, times(1)).findResults();
	}

	@Test
	@SuppressWarnings({ "deprecation", "unchecked" })
	public void stringQueryWithPageSizeReturnsPageableResults() throws LuceneQueryException {
		when(mockLuceneQueryFactory.create(eq("TestIndex"), eq("/Example"), anyString(), anyString()))
			.thenReturn(mockLuceneQuery);
		when(mockLuceneQuery.findPages()).thenReturn(mockPageableLuceneQueryResults);

		doReturn("TestIndex").when(luceneTemplate).resolveIndexName();
		doReturn("/Example").when(luceneTemplate).resolveRegionPath();

		PageableLuceneQueryResults<Object, Object> results =  luceneTemplate.query(
			"title : Up Shit Creek Without a Paddle", "title", 100, 20);

		assertThat(results).isSameAs(mockPageableLuceneQueryResults);

		verify(luceneTemplate, times(1)).resolveIndexName();
		verify(luceneTemplate, times(1)).resolveRegionPath();
		verify(luceneTemplate, times(1)).doFind(isA(LuceneQueryExecutor.class),
			eq("title : Up Shit Creek Without a Paddle"), eq("/Example"), eq("TestIndex"));
		verify(mockLuceneService, times(1)).createLuceneQueryFactory();
		verify(mockLuceneQueryFactory, times(1)).setLimit(eq(100));
		verify(mockLuceneQueryFactory, times(1)).setPageSize(eq(20));
		verify(mockLuceneQueryFactory, times(1)).create(eq("TestIndex"),
			eq("/Example"), eq("title : Up Shit Creek Without a Paddle"), eq("title"));
		verify(mockLuceneQuery, times(1)).findPages();
	}

	@Test
	@SuppressWarnings({ "deprecation", "unchecked" })
	public void queryProviderQueryReturnsList() throws LuceneQueryException {
		when(mockLuceneQueryFactory.create(eq("TestIndex"), eq("/Example"),
			any(LuceneQueryProvider.class))).thenReturn(mockLuceneQuery);
		when(mockLuceneQuery.findResults()).thenReturn(asList(mockLuceneResultStructOne, mockLuceneResultStructTwo));

		doReturn("TestIndex").when(luceneTemplate).resolveIndexName();
		doReturn("/Example").when(luceneTemplate).resolveRegionPath();

		List<LuceneResultStruct<Object, Object>> results =
			luceneTemplate.query(mockLuceneQueryProvider, 100);

		assertThat(results).isNotNull();
		assertThat(results).hasSize(2);
		assertThat(results).containsAll(asList(mockLuceneResultStructOne, mockLuceneResultStructTwo));

		verify(luceneTemplate, times(1)).resolveIndexName();
		verify(luceneTemplate, times(1)).resolveRegionPath();
		verify(luceneTemplate, times(1)).doFind(isA(LuceneQueryExecutor.class),
			eq(mockLuceneQueryProvider), eq("/Example"), eq("TestIndex"));
		verify(mockLuceneService, times(1)).createLuceneQueryFactory();
		verify(mockLuceneQueryFactory, times(1)).setLimit(eq(100));
		verify(mockLuceneQueryFactory, times(1)).setPageSize(eq(LuceneOperations.DEFAULT_PAGE_SIZE));
		verify(mockLuceneQueryFactory, times(1)).create(eq("TestIndex"),
			eq("/Example"), eq(mockLuceneQueryProvider));
		verify(mockLuceneQuery, times(1)).findResults();
	}

	@Test
	@SuppressWarnings({ "deprecation", "unchecked" })
	public void queryProviderQueryWithPageSizeReturnsPageableResults() throws LuceneQueryException {
		when(mockLuceneQueryFactory.create(eq("TestIndex"), eq("/Example"),
			any(LuceneQueryProvider.class))).thenReturn(mockLuceneQuery);
		when(mockLuceneQuery.findPages()).thenReturn(mockPageableLuceneQueryResults);

		doReturn("TestIndex").when(luceneTemplate).resolveIndexName();
		doReturn("/Example").when(luceneTemplate).resolveRegionPath();

		PageableLuceneQueryResults<Object, Object> results =  luceneTemplate.query(mockLuceneQueryProvider,
			100, 20);

		assertThat(results).isSameAs(mockPageableLuceneQueryResults);

		verify(luceneTemplate, times(1)).resolveIndexName();
		verify(luceneTemplate, times(1)).resolveRegionPath();
		verify(luceneTemplate, times(1)).doFind(isA(LuceneQueryExecutor.class),
			eq(mockLuceneQueryProvider), eq("/Example"), eq("TestIndex"));
		verify(mockLuceneService, times(1)).createLuceneQueryFactory();
		verify(mockLuceneQueryFactory, times(1)).setLimit(eq(100));
		verify(mockLuceneQueryFactory, times(1)).setPageSize(eq(20));
		verify(mockLuceneQueryFactory, times(1)).create(eq("TestIndex"),
			eq("/Example"), eq(mockLuceneQueryProvider));
		verify(mockLuceneQuery, times(1)).findPages();
	}

	@Test
	@SuppressWarnings({ "deprecation", "unchecked" })
	public void stringQueryForKeysReturnsKeys() throws LuceneQueryException {
		when(mockLuceneQueryFactory.create(eq("TestIndex"), eq("/Example"), anyString(), anyString()))
			.thenReturn(mockLuceneQuery);
		when(mockLuceneQuery.findKeys()).thenReturn(asList("keyOne", "keyTwo"));

		doReturn("TestIndex").when(luceneTemplate).resolveIndexName();
		doReturn("/Example").when(luceneTemplate).resolveRegionPath();

		Collection<String> keys =  luceneTemplate.queryForKeys(
			"title : Up Shit Creek Without a Paddle", "title", 100);

		assertThat(keys).isNotNull();
		assertThat(keys).hasSize(2);
		assertThat(keys).containsAll(asList("keyOne", "keyTwo"));

		verify(luceneTemplate, times(1)).resolveIndexName();
		verify(luceneTemplate, times(1)).resolveRegionPath();
		verify(luceneTemplate, times(1)).doFind(isA(LuceneQueryExecutor.class),
			eq("title : Up Shit Creek Without a Paddle"), eq("/Example"), eq("TestIndex"));
		verify(mockLuceneService, times(1)).createLuceneQueryFactory();
		verify(mockLuceneQueryFactory, times(1)).setLimit(eq(100));
		verify(mockLuceneQueryFactory, times(1)).setPageSize(eq(LuceneOperations.DEFAULT_PAGE_SIZE));
		verify(mockLuceneQueryFactory, times(1)).create(eq("TestIndex"),
			eq("/Example"), eq("title : Up Shit Creek Without a Paddle"), eq("title"));
		verify(mockLuceneQuery, times(1)).findKeys();
	}

	@Test
	@SuppressWarnings({ "deprecation", "unchecked" })
	public void queryProviderQueryForKeysReturnsKeys() throws LuceneQueryException {
		when(mockLuceneQueryFactory.create(eq("TestIndex"), eq("/Example"),
			any(LuceneQueryProvider.class))).thenReturn(mockLuceneQuery);
		when(mockLuceneQuery.findKeys()).thenReturn(asList("keyOne", "keyTwo"));

		doReturn("TestIndex").when(luceneTemplate).resolveIndexName();
		doReturn("/Example").when(luceneTemplate).resolveRegionPath();

		Collection<String> keys =  luceneTemplate.queryForKeys(mockLuceneQueryProvider, 100);

		assertThat(keys).isNotNull();
		assertThat(keys).hasSize(2);
		assertThat(keys).containsAll(asList("keyOne", "keyTwo"));

		verify(luceneTemplate, times(1)).resolveIndexName();
		verify(luceneTemplate, times(1)).resolveRegionPath();
		verify(luceneTemplate, times(1)).doFind(isA(LuceneQueryExecutor.class),
			eq(mockLuceneQueryProvider), eq("/Example"), eq("TestIndex"));
		verify(mockLuceneService, times(1)).createLuceneQueryFactory();
		verify(mockLuceneQueryFactory, times(1)).setLimit(eq(100));
		verify(mockLuceneQueryFactory, times(1)).setPageSize(eq(LuceneOperations.DEFAULT_PAGE_SIZE));
		verify(mockLuceneQueryFactory, times(1)).create(eq("TestIndex"),
			eq("/Example"), eq(mockLuceneQueryProvider));
		verify(mockLuceneQuery, times(1)).findKeys();
	}

	@Test
	@SuppressWarnings({ "deprecation", "unchecked" })
	public void stringQueryForValuesReturnsValues() throws LuceneQueryException {
		when(mockLuceneQueryFactory.create(eq("TestIndex"), eq("/Example"), anyString(), anyString()))
			.thenReturn(mockLuceneQuery);
		when(mockLuceneQuery.findValues()).thenReturn(asList("valueOne", "valueTwo"));

		doReturn("TestIndex").when(luceneTemplate).resolveIndexName();
		doReturn("/Example").when(luceneTemplate).resolveRegionPath();

		Collection<String> keys =  luceneTemplate.queryForValues(
			"title : Up Shit Creek Without a Paddle", "title", 100);

		assertThat(keys).isNotNull();
		assertThat(keys).hasSize(2);
		assertThat(keys).containsAll(asList("valueOne", "valueTwo"));

		verify(luceneTemplate, times(1)).resolveIndexName();
		verify(luceneTemplate, times(1)).resolveRegionPath();
		verify(luceneTemplate, times(1)).doFind(isA(LuceneQueryExecutor.class),
			eq("title : Up Shit Creek Without a Paddle"), eq("/Example"), eq("TestIndex"));
		verify(mockLuceneService, times(1)).createLuceneQueryFactory();
		verify(mockLuceneQueryFactory, times(1)).setLimit(eq(100));
		verify(mockLuceneQueryFactory, times(1)).setPageSize(eq(LuceneOperations.DEFAULT_PAGE_SIZE));
		verify(mockLuceneQueryFactory, times(1)).create(eq("TestIndex"),
			eq("/Example"), eq("title : Up Shit Creek Without a Paddle"), eq("title"));
		verify(mockLuceneQuery, times(1)).findValues();
	}

	@Test
	@SuppressWarnings({ "deprecation", "unchecked" })
	public void queryProviderQueryForValuesReturnsValues() throws LuceneQueryException {
		when(mockLuceneQueryFactory.create(eq("TestIndex"), eq("/Example"),
			any(LuceneQueryProvider.class))).thenReturn(mockLuceneQuery);
		when(mockLuceneQuery.findValues()).thenReturn(asList("valueOne", "valueTwo"));

		doReturn("TestIndex").when(luceneTemplate).resolveIndexName();
		doReturn("/Example").when(luceneTemplate).resolveRegionPath();

		Collection<String> keys =  luceneTemplate.queryForValues(mockLuceneQueryProvider, 100);

		assertThat(keys).isNotNull();
		assertThat(keys).hasSize(2);
		assertThat(keys).containsAll(asList("valueOne", "valueTwo"));

		verify(luceneTemplate, times(1)).resolveIndexName();
		verify(luceneTemplate, times(1)).resolveRegionPath();
		verify(luceneTemplate, times(1)).doFind(isA(LuceneQueryExecutor.class),
			eq(mockLuceneQueryProvider), eq("/Example"), eq("TestIndex"));
		verify(mockLuceneService, times(1)).createLuceneQueryFactory();
		verify(mockLuceneQueryFactory, times(1)).setLimit(eq(100));
		verify(mockLuceneQueryFactory, times(1)).setPageSize(eq(LuceneOperations.DEFAULT_PAGE_SIZE));
		verify(mockLuceneQueryFactory, times(1)).create(eq("TestIndex"),
			eq("/Example"), eq(mockLuceneQueryProvider));
		verify(mockLuceneQuery, times(1)).findValues();
	}
}
