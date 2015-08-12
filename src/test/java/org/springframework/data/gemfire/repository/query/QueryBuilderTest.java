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

package org.springframework.data.gemfire.repository.query;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.repository.query.parser.PartTree;

/**
 * The QueryBuilderTest class is a test suite of test cases testing the contract and functionality of the QueryBuilder
 * class.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.repository.query.QueryBuilder
 * @since 1.7.0
 */
public class QueryBuilderTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	@Test
	public void createQueryBuilderNonDistinct() {
		GemfirePersistentEntity<?> mockPersistentEntity = mock(GemfirePersistentEntity.class, "MockGemfirePersistentEntity");
		PartTree mockPartTree = mock(PartTree.class, "MockPartTree");

		when(mockPersistentEntity.getRegionName()).thenReturn("Example");
		when(mockPartTree.isDistinct()).thenReturn(false);

		QueryBuilder queryBuilder = new QueryBuilder(mockPersistentEntity, mockPartTree);

		assertThat(queryBuilder.toString(), is(equalTo("SELECT * FROM /Example x")));

		verify(mockPersistentEntity, times(1)).getRegionName();
		verify(mockPartTree, times(1)).isDistinct();
	}

	@Test
	public void createQueryBuilderWithDistinct() {
		GemfirePersistentEntity<?> mockPersistentEntity = mock(GemfirePersistentEntity.class, "MockGemfirePersistentEntity");
		PartTree mockPartTree = mock(PartTree.class, "MockPartTree");

		when(mockPersistentEntity.getRegionName()).thenReturn("Example");
		when(mockPartTree.isDistinct()).thenReturn(true);

		QueryBuilder queryBuilder = new QueryBuilder(mockPersistentEntity, mockPartTree);

		assertThat(queryBuilder.toString(), is(equalTo("SELECT DISTINCT * FROM /Example x")));

		verify(mockPersistentEntity, times(1)).getRegionName();
		verify(mockPartTree, times(1)).isDistinct();
	}

	@Test
	public void createQueryBuilderWithNullQueryString() {
		expectedException.expect(IllegalArgumentException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage(is(equalTo("The OQL Query must be specified")));

		new QueryBuilder(null);
	}

	@Test
	public void createWithPredicate() {
		Predicate mockPredicate = mock(Predicate.class, "MockPredicate");

		when(mockPredicate.toString(eq(QueryBuilder.DEFAULT_ALIAS))).thenReturn("x.id = 1");

		QueryBuilder queryBuilder = new QueryBuilder(String.format("SELECT * FROM /Example %s",
			QueryBuilder.DEFAULT_ALIAS));

		QueryString queryString = queryBuilder.create(mockPredicate);

		assertThat(queryString, is(notNullValue()));
		assertThat(queryString.toString(), is(equalTo("SELECT * FROM /Example x WHERE x.id = 1")));

		verify(mockPredicate, times(1)).toString(eq(QueryBuilder.DEFAULT_ALIAS));
	}

}
