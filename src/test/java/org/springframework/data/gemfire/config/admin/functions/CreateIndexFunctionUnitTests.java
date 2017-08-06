/*
 * Copyright 2017 the original author or authors.
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
 */

package org.springframework.data.gemfire.config.admin.functions;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.QueryException;
import org.apache.geode.cache.query.QueryService;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.data.gemfire.IndexType;
import org.springframework.data.gemfire.config.schema.definitions.IndexDefinition;

/**
 * Unit tests for {@link CreateIndexFunction}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.query.Index
 * @see org.apache.geode.cache.query.QueryService
 * @see org.springframework.data.gemfire.config.schema.definitions.IndexDefinition
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class CreateIndexFunctionUnitTests {

	@Mock
	private Cache mockCache;

	private CreateIndexFunction createIndexFunction;

	@Mock
	private Index mockIndex;

	@Mock
	private QueryService mockQueryService;

	@Before
	public void setup() {

		this.createIndexFunction = spy(new CreateIndexFunction());

		doReturn(this.mockCache).when(this.createIndexFunction).resolveCache();
		when(this.mockCache.getQueryService()).thenReturn(this.mockQueryService);
		when(this.mockIndex.getName()).thenReturn("MockIndex");
	}

	@Test
	public void createsFunctionalIndex() throws QueryException {

		when(this.mockIndex.getIndexedExpression()).thenReturn("age");
		when(this.mockIndex.getFromClause()).thenReturn("/Customers");
		when(this.mockIndex.getType()).thenReturn(IndexType.FUNCTIONAL.getGemfireIndexType());
		when(this.mockQueryService.getIndexes()).thenReturn(null);

		IndexDefinition indexDefinition = IndexDefinition.from(this.mockIndex);

		assertThat(this.createIndexFunction.createIndex(indexDefinition)).isTrue();

		verify(this.mockCache, times(2)).getQueryService();
		verify(this.mockQueryService, times(1)).getIndexes();
		verify(this.mockQueryService, times(1))
			.createIndex(eq("MockIndex"), eq("age"), eq("/Customers"));
	}

	@Test
	public void createsHashIndex() throws QueryException {

		Index mockIndexTwo = mock(Index.class, "MockIndexTwo");

		when(mockIndexTwo.getName()).thenReturn("MockIndexTwo");
		when(this.mockIndex.getIndexedExpression()).thenReturn("name");
		when(this.mockIndex.getFromClause()).thenReturn("/Customers");
		when(this.mockIndex.getType()).thenReturn(IndexType.HASH.getGemfireIndexType());
		when(this.mockQueryService.getIndexes()).thenReturn(Collections.singleton(mockIndexTwo));

		IndexDefinition indexDefinition = IndexDefinition.from(this.mockIndex);

		assertThat(this.createIndexFunction.createIndex(indexDefinition)).isTrue();

		verify(this.mockCache, times(2)).getQueryService();
		verify(this.mockQueryService, times(1)).getIndexes();
		verify(mockIndexTwo, times(1)).getName();
		verify(this.mockQueryService, times(1))
			.createHashIndex(eq("MockIndex"), eq("name"), eq("/Customers"));
	}

	@Test
	public void createsKeyIndex() throws QueryException {

		when(this.mockIndex.getIndexedExpression()).thenReturn("id");
		when(this.mockIndex.getFromClause()).thenReturn("/Customers");
		when(this.mockIndex.getType()).thenReturn(IndexType.PRIMARY_KEY.getGemfireIndexType());
		when(this.mockQueryService.getIndexes()).thenReturn(Collections.emptyList());

		IndexDefinition indexDefinition = IndexDefinition.from(this.mockIndex);

		assertThat(this.createIndexFunction.createIndex(indexDefinition)).isTrue();

		verify(this.mockCache, times(2)).getQueryService();
		verify(this.mockQueryService, times(1)).getIndexes();
		verify(this.mockQueryService, times(1))
			.createKeyIndex(eq("MockIndex"), eq("id"), eq("/Customers"));
	}

	@Test
	public void doesNotCreateIndexWhenIndexAlreadyExists() throws QueryException {

		when(this.mockQueryService.getIndexes()).thenReturn(Collections.singleton(this.mockIndex));

		IndexDefinition indexDefinition = IndexDefinition.from(this.mockIndex);

		assertThat(this.createIndexFunction.createIndex(indexDefinition)).isFalse();

		verify(this.mockCache, times(1)).getQueryService();
		verify(this.mockQueryService, times(1)).getIndexes();
		verify(this.mockQueryService, never()).createIndex(anyString(), anyString(), anyString());
		verify(this.mockQueryService, never()).createIndex(anyString(), anyString(), anyString(), anyString());
		verify(this.mockQueryService, never()).createHashIndex(anyString(), anyString(), anyString());
		verify(this.mockQueryService, never()).createHashIndex(anyString(), anyString(), anyString(), anyString());
		verify(this.mockQueryService, never()).createKeyIndex(anyString(), anyString(), anyString());
	}
}
