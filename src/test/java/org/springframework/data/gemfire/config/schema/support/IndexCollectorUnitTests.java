/*
 * Copyright 2017-2018 the original author or authors.
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

package org.springframework.data.gemfire.config.schema.support;

import static java.util.Arrays.stream;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.util.CollectionUtils.asSet;

import java.util.Collections;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.QueryService;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ApplicationContext;

/**
 * Unit tests for {@link IndexCollector}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class IndexCollectorUnitTests {

	@Mock
	private ApplicationContext mockApplicationContext;

	@Mock
	private GemFireCache mockCache;

	private IndexCollector indexCollector = new IndexCollector();

	@Mock
	private QueryService mockQueryService;

	@Before
	public void setup() {
		when(this.mockCache.getQueryService()).thenReturn(this.mockQueryService);
	}

	private Index mockIndex(String name) {

		Index mockIndex = mock(Index.class, name);

		when(mockIndex.getName()).thenReturn(name);

		return mockIndex;
	}

	private Map<String, Index> asMap(Index... indexes) {
		return stream(indexes).collect(Collectors.toMap(Index::getName, Function.identity()));
	}

	@Test
	public void collectIndexesFromApplicationContext() {

		Index mockIndexOne = mockIndex("MockIndexOne");
		Index mockIndexTwo = mockIndex("MockIndexTwo");

		Map<String, Index> indexBeans = asMap(mockIndexOne, mockIndexTwo);

		when(this.mockApplicationContext.getBeansOfType(eq(Index.class))).thenReturn(indexBeans);

		Set<Index> indexes = this.indexCollector.collectFrom(this.mockApplicationContext);

		assertThat(indexes).isNotNull();
		assertThat(indexes).hasSize(2);
		assertThat(indexes).containsAll(asSet(mockIndexOne, mockIndexTwo));

		verify(this.mockApplicationContext, times(1)).getBeansOfType(eq(Index.class));
	}

	@Test
	public void collectIndexesFromApplicationContextWhenNoBeansOfTypeIndexExist() {

		when(this.mockApplicationContext.getBeansOfType(eq(Index.class))).thenReturn(Collections.emptyMap());

		Set<Index> indexes = this.indexCollector.collectFrom(this.mockApplicationContext);

		assertThat(indexes).isNotNull();
		assertThat(indexes).isEmpty();

		verify(this.mockApplicationContext, times(1)).getBeansOfType(eq(Index.class));
	}

	@Test
	public void collectIndexesFromGemFireCache() {

		Index mockIndexOne = mockIndex("MockIndexOne");
		Index mockIndexTwo = mockIndex("MockIndexTwo");

		when(this.mockQueryService.getIndexes()).thenReturn(asSet(mockIndexOne, mockIndexTwo));

		Set<Index> indexes = this.indexCollector.collectFrom(this.mockCache);

		assertThat(indexes).isNotNull();
		assertThat(indexes).hasSize(2);
		assertThat(indexes).containsAll(asSet(mockIndexOne, mockIndexTwo));

		verify(this.mockCache, times(1)).getQueryService();
		verify(this.mockQueryService, times(1)).getIndexes();
	}

	@Test
	public void collectIndexesFromGemFireCacheWhenNoIndexesExist() {

		when(this.mockQueryService.getIndexes()).thenReturn(Collections.emptySet());

		Set<Index> indexes = this.indexCollector.collectFrom(this.mockCache);

		assertThat(indexes).isNotNull();
		assertThat(indexes).isEmpty();

		verify(this.mockCache, times(1)).getQueryService();
		verify(this.mockQueryService, times(1)).getIndexes();
	}
}
