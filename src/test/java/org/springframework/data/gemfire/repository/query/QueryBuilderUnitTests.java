/*
 * Copyright 2010-2019 the original author or authors.
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;

import org.springframework.data.gemfire.mapping.GemfirePersistentEntity;
import org.springframework.data.repository.query.parser.PartTree;

/**
 * Unit tests for {@link QueryBuilder} class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.repository.query.QueryBuilder
 * @since 1.7.0
 */
public class QueryBuilderUnitTests {

	@Test
	public void createQueryBuilderWithDistinctQuery() {

		GemfirePersistentEntity<?> mockPersistentEntity = mock(GemfirePersistentEntity.class);

		PartTree mockPartTree = mock(PartTree.class);

		when(mockPersistentEntity.getRegionName()).thenReturn("Example");
		when(mockPartTree.isDistinct()).thenReturn(true);

		QueryBuilder queryBuilder = new QueryBuilder(mockPersistentEntity, mockPartTree);

		assertThat(queryBuilder.toString()).isEqualTo("SELECT DISTINCT * FROM /Example x");

		verify(mockPersistentEntity, times(1)).getRegionName();
		verify(mockPartTree, times(1)).isDistinct();
	}

	@Test
	public void createQueryBuilderWithNonDistinctQuery() {

		GemfirePersistentEntity<?> mockPersistentEntity = mock(GemfirePersistentEntity.class);

		PartTree mockPartTree = mock(PartTree.class);

		when(mockPersistentEntity.getRegionName()).thenReturn("Example");
		when(mockPartTree.isDistinct()).thenReturn(false);

		QueryBuilder queryBuilder = new QueryBuilder(mockPersistentEntity, mockPartTree);

		assertThat(queryBuilder.toString()).isEqualTo("SELECT * FROM /Example x");

		verify(mockPersistentEntity, times(1)).getRegionName();
		verify(mockPartTree, times(1)).isDistinct();
	}

	@Test(expected = IllegalArgumentException.class)
	public void createQueryBuilderWithNullQueryString() {

		try {
			new QueryBuilder(null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessage("Query is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	@SuppressWarnings("all")
	public void createWithPredicate() {

		Predicate mockPredicate = mock(Predicate.class);

		when(mockPredicate.toString(eq(QueryBuilder.DEFAULT_ALIAS))).thenReturn("x.id = 1");

		QueryBuilder queryBuilder =
			new QueryBuilder(String.format("SELECT * FROM /Example %s", QueryBuilder.DEFAULT_ALIAS));

		QueryString queryString = queryBuilder.create(mockPredicate);

		assertThat(queryString).isNotNull();
		assertThat(queryString.toString()).isEqualTo("SELECT * FROM /Example x WHERE x.id = 1");

		verify(mockPredicate, times(1)).toString(eq(QueryBuilder.DEFAULT_ALIAS));
	}

	@Test
	public void createWithNullPredicate() {

		QueryBuilder queryBuilder = new QueryBuilder("SELECT * FROM /Example");

		QueryString queryString = queryBuilder.create(null);

		assertThat(queryString).isNotNull();
		assertThat(queryString.toString()).isEqualTo("SELECT * FROM /Example");
	}
}
