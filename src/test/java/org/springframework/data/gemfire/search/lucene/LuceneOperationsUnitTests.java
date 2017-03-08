/*
 * Copyright 2016 the original author or authors.
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

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.anyVararg;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.geode.cache.lucene.LuceneQueryProvider;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

/**
 * Unit tests for the {@link LuceneOperations} interface.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.search.lucene.LuceneOperations
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class LuceneOperationsUnitTests {

	@Mock
	private LuceneQueryProvider mockLuceneQueryProvider;

	@Mock
	private TestLuceneOperations mockLuceneOperations;

	@Test
	public void stringQueryCallsQueryWithResultLimit() {
		when(mockLuceneOperations.query(anyString(), anyString(), anyVararg())).thenCallRealMethod();

		mockLuceneOperations.query("title : Up Shit Creek Without a Paddle", "title",
			"content");

		verify(mockLuceneOperations, times(1)).query(
			eq("title : Up Shit Creek Without a Paddle"), eq("title"),
				eq(LuceneOperations.DEFAULT_RESULT_LIMIT), eq("content"));
	}

	@Test
	public void queryProviderQueryCallsQueryWithResultLimit() {
		when(mockLuceneOperations.query(any(LuceneQueryProvider.class), anyVararg())).thenCallRealMethod();

		mockLuceneOperations.query(mockLuceneQueryProvider, "content");

		verify(mockLuceneOperations, times(1)).query(eq(mockLuceneQueryProvider),
			eq(LuceneOperations.DEFAULT_RESULT_LIMIT), eq("content"));
	}

	@Test
	public void stringQueryForKeysCallsQueryForKeysWithResultLimit() {
		when(mockLuceneOperations.queryForKeys(anyString(), anyString())).thenCallRealMethod();

		mockLuceneOperations.queryForKeys("title : Up Shit Creek Without a Paddle", "title");

		verify(mockLuceneOperations, times(1)).queryForKeys(
			eq("title : Up Shit Creek Without a Paddle"), eq("title"),
				eq(LuceneOperations.DEFAULT_RESULT_LIMIT));
	}

	@Test
	public void queryProviderQueryForKeysCallsQueryForKeysWithResultLimit() {
		when(mockLuceneOperations.queryForKeys(any(LuceneQueryProvider.class))).thenCallRealMethod();

		mockLuceneOperations.queryForKeys(mockLuceneQueryProvider);

		verify(mockLuceneOperations, times(1)).queryForKeys(
			eq(mockLuceneQueryProvider), eq(LuceneOperations.DEFAULT_RESULT_LIMIT));
	}

	@Test
	public void stringQueryForValuesCallsQueryForValuesWithResultLimit() {
		when(mockLuceneOperations.queryForValues(anyString(), anyString())).thenCallRealMethod();

		mockLuceneOperations.queryForValues("title : Up Shit Creek Without a Paddle", "title");

		verify(mockLuceneOperations, times(1)).queryForValues(
			eq("title : Up Shit Creek Without a Paddle"), eq("title"),
				eq(LuceneOperations.DEFAULT_RESULT_LIMIT));
	}

	@Test
	public void queryProviderQueryForValuesCallsQueryForValuesWithResultLimit() {
		when(mockLuceneOperations.queryForValues(any(LuceneQueryProvider.class))).thenCallRealMethod();

		mockLuceneOperations.queryForValues(mockLuceneQueryProvider);

		verify(mockLuceneOperations, times(1)).queryForValues(
			eq(mockLuceneQueryProvider), eq(LuceneOperations.DEFAULT_RESULT_LIMIT));
	}

	abstract class TestLuceneOperations implements LuceneOperations {
	}
}
