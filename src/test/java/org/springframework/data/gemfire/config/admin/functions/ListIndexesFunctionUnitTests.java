/*
 * Copyright 2017-2020 the original author or authors.
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
 */

package org.springframework.data.gemfire.config.admin.functions;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Arrays;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.QueryService;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit tests for {@link ListIndexesFunction}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.query.Index
 * @see org.apache.geode.cache.query.QueryService
 * @see org.springframework.data.gemfire.config.admin.functions.ListIndexesFunction
 * @since 2.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ListIndexesFunctionUnitTests {

	@Mock
	private Cache mockCache;

	@Mock
	private Index mockIndexOne;

	@Mock
	private Index mockIndexTwo;

	private ListIndexesFunction listIndexesFunction;

	@Mock
	private QueryService mockQueryService;

	@Before
	public void setup() {

		this.listIndexesFunction = spy(new ListIndexesFunction());

		doReturn(this.mockCache).when(this.listIndexesFunction).resolveCache();
		when(this.mockCache.getQueryService()).thenReturn(this.mockQueryService);
		when(this.mockIndexOne.getName()).thenReturn("MockIndexOne");
		when(this.mockIndexTwo.getName()).thenReturn("MockIndexTwo");
	}

	@Test
	public void listIndexesReturnsIndexNames() {

		when(this.mockQueryService.getIndexes()).thenReturn(Arrays.asList(this.mockIndexOne, this.mockIndexTwo));

		assertThat(this.listIndexesFunction.listIndexes()).contains("MockIndexOne", "MockIndexTwo");

		verify(this.listIndexesFunction, times(1)).resolveCache();
		verify(this.mockCache, times(1)).getQueryService();
		verify(this.mockQueryService, times(1)).getIndexes();
		verify(this.mockIndexOne, times(1)).getName();
		verify(this.mockIndexTwo, times(1)).getName();
	}

	@Test
	public void listIndexesReturnsEmptySetWhenCacheIsNull() {

		doReturn(null).when(this.listIndexesFunction).resolveCache();

		assertThat(this.listIndexesFunction.listIndexes()).isEmpty();

		verify(this.listIndexesFunction, times(1)).resolveCache();
	}

	@Test
	public void listIndexesReturnsEmptySetWhenQueryServiceIsNull() {

		when(this.mockCache.getQueryService()).thenReturn(null);

		assertThat(this.listIndexesFunction.listIndexes()).isEmpty();

		verify(this.listIndexesFunction, times(1)).resolveCache();
		verify(this.mockCache, times(1)).getQueryService();
	}

	@Test
	public void listIndexesReturnsEmptySetWhenQueryServiceGetIndexesIsNull() {

		when(this.mockQueryService.getIndexes()).thenReturn(null);

		assertThat(this.listIndexesFunction.listIndexes()).isEmpty();

		verify(this.listIndexesFunction, times(1)).resolveCache();
		verify(this.mockCache, times(1)).getQueryService();
		verify(this.mockQueryService, times(1)).getIndexes();
	}
}
