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

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isA;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Matchers.anyInt;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.data.gemfire.search.lucene.LuceneAccessor.LuceneQueryExecutor;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.lucene.LuceneQueryException;
import org.apache.geode.cache.lucene.LuceneQueryFactory;
import org.apache.geode.cache.lucene.LuceneService;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.dao.DataRetrievalFailureException;
import org.springframework.data.gemfire.search.lucene.support.LuceneAccessorSupport;

/**
 * Unit tests for {@link LuceneAccessor}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.search.lucene.LuceneAccessor
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class LuceneAccessorUnitTests {

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Mock
	private GemFireCache mockCache;

	// SUT
	private LuceneAccessor luceneAccessor;

	@Mock
	private LuceneIndex mockLuceneIndex;

	@Mock
	private LuceneQueryFactory mockLuceneQueryFactory;

	@Mock
	private LuceneService mockLuceneService;

	@Mock
	private Region<?, ?> mockRegion;

	@Before
	public void setup() {
		luceneAccessor = spy(new LuceneAccessorSupport() { });
	}

	@Test
	public void afterPropertiesSetInitializesLuceneAccessorProperly() throws Exception {
		assertThat(luceneAccessor.getCache()).isNull();
		assertThat(luceneAccessor.getLuceneService()).isNull();
		assertThat(luceneAccessor.getIndexName()).isNullOrEmpty();
		assertThat(luceneAccessor.getRegion()).isNull();
		assertThat(luceneAccessor.getRegionPath()).isNullOrEmpty();

		doReturn(mockCache).when(luceneAccessor).resolveCache();
		doReturn(mockLuceneService).when(luceneAccessor).resolveLuceneService();
		doReturn("TestIndex").when(luceneAccessor).resolveIndexName();
		doReturn("/Example").when(luceneAccessor).resolveRegionPath();

		luceneAccessor.afterPropertiesSet();

		assertThat(luceneAccessor.getCache()).isEqualTo(mockCache);
		assertThat(luceneAccessor.getLuceneService()).isEqualTo(mockLuceneService);
		assertThat(luceneAccessor.getIndexName()).isEqualTo("TestIndex");
		assertThat(luceneAccessor.getRegion()).isNull();
		assertThat(luceneAccessor.getRegionPath()).isEqualTo("/Example");

		verify(luceneAccessor, times(1)).resolveCache();
		verify(luceneAccessor, times(1)).resolveLuceneService();
		verify(luceneAccessor, times(1)).resolveIndexName();
		verify(luceneAccessor, times(1)).resolveRegionPath();
	}

	@Test
	public void createLuceneQueryFactory() {
		doReturn(mockLuceneService).when(luceneAccessor).resolveLuceneService();
		when(mockLuceneService.createLuceneQueryFactory()).thenReturn(mockLuceneQueryFactory);

		assertThat(luceneAccessor.createLuceneQueryFactory()).isEqualTo(mockLuceneQueryFactory);

		verify(luceneAccessor).resolveLuceneService();
		verify(mockLuceneService, times(1)).createLuceneQueryFactory();
	}

	@Test
	@SuppressWarnings("deprecation")
	public void createLuceneQueryFactoryWithResultLimit() {
		doReturn(mockLuceneService).when(luceneAccessor).resolveLuceneService();
		when(mockLuceneService.createLuceneQueryFactory()).thenReturn(mockLuceneQueryFactory);
		when(mockLuceneQueryFactory.setPageSize(anyInt())).thenReturn(mockLuceneQueryFactory);
		when(mockLuceneQueryFactory.setLimit(anyInt())).thenReturn(mockLuceneQueryFactory);

		assertThat(luceneAccessor.createLuceneQueryFactory(1000)).isEqualTo(mockLuceneQueryFactory);

		verify(luceneAccessor).resolveLuceneService();
		verify(mockLuceneService, times(1)).createLuceneQueryFactory();
		verify(mockLuceneQueryFactory, times(1)).setLimit(eq(1000));
		verify(mockLuceneQueryFactory, times(1))
			.setPageSize(eq(LuceneAccessor.DEFAULT_PAGE_SIZE));
	}

	@Test
	@SuppressWarnings("deprecation")
	public void createLuceneQueryFactoryWithResultLimitAndPageSize() {
		doReturn(mockLuceneService).when(luceneAccessor).resolveLuceneService();
		when(mockLuceneService.createLuceneQueryFactory()).thenReturn(mockLuceneQueryFactory);
		when(mockLuceneQueryFactory.setPageSize(anyInt())).thenReturn(mockLuceneQueryFactory);
		when(mockLuceneQueryFactory.setLimit(anyInt())).thenReturn(mockLuceneQueryFactory);

		assertThat(luceneAccessor.createLuceneQueryFactory(1000, 20))
			.isEqualTo(mockLuceneQueryFactory);

		verify(luceneAccessor).resolveLuceneService();
		verify(mockLuceneService, times(1)).createLuceneQueryFactory();
		verify(mockLuceneQueryFactory, times(1)).setLimit(eq(1000));
		verify(mockLuceneQueryFactory, times(1)).setPageSize(eq(20));
	}

	@Test
	public void resolveCacheReturnsConfiguredCache() {
		luceneAccessor.setCache(mockCache);

		assertThat(luceneAccessor.getCache()).isSameAs(mockCache);
		assertThat(luceneAccessor.resolveCache()).isSameAs(mockCache);
	}

	@Test
	public void resolveLuceneServiceReturnsConfiguredLuceneService() {
		luceneAccessor.setLuceneService(mockLuceneService);

		assertThat(luceneAccessor.getLuceneService()).isSameAs(mockLuceneService);
		assertThat(luceneAccessor.resolveLuceneService()).isSameAs(mockLuceneService);
	}

	@Test
	public void resolveLuceneServiceLooksUpLuceneService() {
		doReturn(mockCache).when(luceneAccessor).resolveCache();
		doReturn(mockLuceneService).when(luceneAccessor).resolveLuceneService(eq(mockCache));

		assertThat(luceneAccessor.getLuceneService()).isNull();
		assertThat(luceneAccessor.resolveLuceneService()).isSameAs(mockLuceneService);

		verify(luceneAccessor, times(1)).resolveCache();
		verify(luceneAccessor, times(1)).resolveLuceneService(eq(mockCache));
	}

	@Test
	public void resolveLuceneServiceThrowsIllegalArgumentExceptionWhenCacheIsNull() {
		exception.expect(IllegalArgumentException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("Cache reference was not properly configured");

		luceneAccessor.resolveLuceneService(null);
	}

	@Test
	@SuppressWarnings("all")
	public void resolveIndexNameReturnsConfiguredIndexName() {
		luceneAccessor.setIndexName("TestIndex");

		assertThat(luceneAccessor.getIndexName()).isEqualTo("TestIndex");
		assertThat(luceneAccessor.resolveIndexName()).isEqualTo("TestIndex");

		verify(luceneAccessor, never()).getLuceneIndex();
	}

	@Test
	@SuppressWarnings("all")
	public void resolveIndexNameReturnsLuceneIndexName() {
		luceneAccessor.setLuceneIndex(mockLuceneIndex);

		when(mockLuceneIndex.getName()).thenReturn("MockIndex");

		assertThat(luceneAccessor.getIndexName()).isNullOrEmpty();
		assertThat(luceneAccessor.resolveIndexName()).isEqualTo("MockIndex");

		verify(luceneAccessor, times(1)).getLuceneIndex();
		verify(mockLuceneIndex, times(1)).getName();
	}

	@Test
	@SuppressWarnings("all")
	public void resolveIndexNameThrowsIllegalStateExceptionWhenIndexNameIsUnresolvable() {
		assertThat(luceneAccessor.getIndexName()).isNullOrEmpty();
		assertThat(luceneAccessor.getLuceneIndex()).isNull();

		exception.expect(IllegalStateException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("The name of the Lucene Index could not be resolved");

		luceneAccessor.resolveIndexName();
	}

	@Test
	public void resolveRegionPathReturnsConfiguredRegionPath() {
		luceneAccessor.setRegionPath("/Example");

		assertThat(luceneAccessor.getRegionPath()).isEqualTo("/Example");
		assertThat(luceneAccessor.resolveRegionPath()).isEqualTo("/Example");
	}

	@Test
	@SuppressWarnings("all")
	public void resolveRegionPathReturnsRegionFullPath() {
		when(mockRegion.getFullPath()).thenReturn("/Example");

		luceneAccessor.setRegion(mockRegion);

		assertThat(luceneAccessor.resolveRegionPath()).isEqualTo("/Example");

		verify(luceneAccessor, times(1)).getRegionPath();
		verify(luceneAccessor, times(1)).getRegion();
		verify(mockRegion, times(1)).getFullPath();
	}

	@Test
	public void resolveRegionPathThrowsIllegalStatueExceptionWhenRegionPathIsUnresolvable() {
		assertThat(luceneAccessor.getRegion()).isNull();
		assertThat(luceneAccessor.getRegionPath()).isNullOrEmpty();

		exception.expect(IllegalStateException.class);
		exception.expectCause(is(nullValue(Throwable.class)));
		exception.expectMessage("Region path could not be resolved");

		luceneAccessor.resolveRegionPath();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void doFind() throws LuceneQueryException {
		LuceneQueryExecutor<String> mockQueryExecutor = mock(LuceneQueryExecutor.class);

		when(mockQueryExecutor.execute()).thenReturn("test");

		assertThat(luceneAccessor.doFind(mockQueryExecutor, "title : 'Up Shit Creek Without a Paddle",
			"/Example", "ExampleIndex")).isEqualTo("test");

		verify(mockQueryExecutor, times(1)).execute();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void doFindHandlesLuceneQueryException() throws LuceneQueryException {
		LuceneQueryExecutor<String> mockQueryExecutor = mock(LuceneQueryExecutor.class);

		when(mockQueryExecutor.execute()).thenThrow(new LuceneQueryException("test"));

		try {
			exception.expect(DataRetrievalFailureException.class);
			exception.expectCause(isA(LuceneQueryException.class));
			exception.expectMessage(containsString(
				"Failed to execute Lucene Query [title : Up Shit Creek Without a Paddle] on Region [/Example] with Lucene Index [ExampleIndex]"));

			luceneAccessor.doFind(mockQueryExecutor, "title : Up Shit Creek Without a Paddle",
				"/Example", "ExampleIndex");
		}
		finally {
			verify(mockQueryExecutor, times(1)).execute();
		}
	}

	@Test
	public void luceneAccessorInitializedCorrectly() {
		luceneAccessor.setCache(mockCache);
		luceneAccessor.setIndexName("ExampleIndex");
		luceneAccessor.setLuceneIndex(mockLuceneIndex);
		luceneAccessor.setLuceneService(mockLuceneService);
		luceneAccessor.setRegion(mockRegion);
		luceneAccessor.setRegionPath("/Example");

		assertThat(luceneAccessor.getCache()).isSameAs(mockCache);
		assertThat(luceneAccessor.getIndexName()).isEqualTo("ExampleIndex");
		assertThat(luceneAccessor.getLuceneIndex()).isSameAs(mockLuceneIndex);
		assertThat(luceneAccessor.getLuceneService()).isSameAs(mockLuceneService);
		assertThat(luceneAccessor.getRegion()).isSameAs(mockRegion);
		assertThat(luceneAccessor.getRegionPath()).isEqualTo("/Example");
	}
}
