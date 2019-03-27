/*
 * Copyright 2016-2019 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
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
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.geode.cache.lucene.LuceneQueryProvider;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link ProjectingLuceneOperations}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.search.lucene.ProjectingLuceneOperations
 * @since 1.1.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ProjectingLuceneOperationsUnitTests {

	@Mock
	private LuceneQueryProvider mockQueryProvider;

	@Mock
	private TestProjectingLuceneOperations projectingLuceneOperations;

	@Test
	@SuppressWarnings("unchecked")
	public void stringQueryCallsQueryWithResultLimit() {
		when(projectingLuceneOperations.query(anyString(), anyString(), any(Class.class))).thenCallRealMethod();

		projectingLuceneOperations.query("title : Up Shit Creek Without A Paddle",
			"title", Book.class);

		verify(projectingLuceneOperations, times(1))
			.query(eq("title : Up Shit Creek Without A Paddle"), eq("title"),
				eq(ProjectingLuceneOperations.DEFAULT_RESULT_LIMIT), eq(Book.class));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void queryProviderQueryCallsQueryWithResultLimit() {
		when(projectingLuceneOperations.query(any(LuceneQueryProvider.class), any(Class.class)))
			.thenCallRealMethod();

		projectingLuceneOperations.query(mockQueryProvider, Book.class);

		verify(projectingLuceneOperations, times(1))
			.query(eq(mockQueryProvider), eq(ProjectingLuceneOperations.DEFAULT_RESULT_LIMIT), eq(Book.class));
	}

	static class Book {}

	abstract class TestProjectingLuceneOperations implements ProjectingLuceneOperations {
	}
}
