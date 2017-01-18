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

package org.springframework.data.gemfire;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.isA;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.sameInstance;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.IndexExistsException;
import org.apache.geode.cache.query.IndexInvalidException;
import org.apache.geode.cache.query.IndexNameConflictException;
import org.apache.geode.cache.query.IndexStatistics;
import org.apache.geode.cache.query.QueryService;
import org.junit.After;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.data.gemfire.config.xml.GemfireConstants;
import org.springframework.data.util.ReflectionUtils;

/**
 * Unit tests for {@link IndexFactoryBean}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.query.Index
 * @see org.springframework.data.gemfire.IndexFactoryBean
 * @since 1.5.2
 */
public class IndexFactoryBeanTest {

	@Rule
	public ExpectedException expectedException = ExpectedException.none();

	private Cache mockCache = mock(Cache.class, "IndexFactoryBeanTest.MockCache");

	private IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

	private QueryService mockQueryService = mock(QueryService.class, "IndexFactoryBeanTest.MockQueryService");

	protected IndexFactoryBean newIndexFactoryBean() {
		IndexFactoryBean indexFactoryBean = new IndexFactoryBean() {
			@Override QueryService lookupQueryService() {
				return mockQueryService;
			}
		};

		indexFactoryBean.setCache(mockCache);

		return indexFactoryBean;
	}

	@After
	public void tearDown() {
		indexFactoryBean.setBeanFactory(null);
		indexFactoryBean.setCache(null);
		indexFactoryBean.setDefine(false);
	}

	@Test
	public void afterPropertiesSetIsSuccessful() throws Exception {
		BeanFactory mockBeanFactory = mock(BeanFactory.class, "testAfterPropertiesSetIsSuccessful.MockBeanFactory");

		Cache mockCache = mock(Cache.class, "testAfterPropertiesSetIsSuccessful.MockCache");

		Index mockIndex = mock(Index.class, "testAfterPropertiesSetIsSuccessful.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "testAfterPropertiesSetIsSuccessful.MockQueryService");

		when(mockBeanFactory.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE))).thenReturn(false);
		when(mockCache.getQueryService()).thenReturn(mockQueryService);
		when(mockQueryService.createKeyIndex(eq("TestKeyIndex"), eq("id"), eq("/Example"))).thenReturn(mockIndex);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setBeanFactory(mockBeanFactory);
		indexFactoryBean.setCache(mockCache);
		indexFactoryBean.setDefine(false);
		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setName("TestKeyIndex");
		indexFactoryBean.setType("key");
		indexFactoryBean.afterPropertiesSet();

		assertEquals(mockIndex, indexFactoryBean.getObject());
		assertSame(mockIndex, indexFactoryBean.getObject()); // assert Index really is a 'Singleton'
		assertTrue(Index.class.isAssignableFrom(indexFactoryBean.getObjectType()));
		assertTrue(indexFactoryBean.isSingleton());

		verify(mockBeanFactory, times(1)).containsBean(
			eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE));
		verify(mockCache, times(1)).getQueryService();
		verify(mockQueryService, times(1)).createKeyIndex(eq("TestKeyIndex"), eq("id"), eq("/Example"));
	}

	@Test(expected = IllegalArgumentException.class)
	public void afterPropertiesSetWithNullCache() throws Exception {
		try {
			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();
			indexFactoryBean.setName("TestIndex");
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Cache is required", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void afterPropertiesSetWithNullQueryService() throws Exception {
		try {
			IndexFactoryBean indexFactoryBean = new IndexFactoryBean() {
				@Override QueryService lookupQueryService() {
					return null;
				}
			};

			indexFactoryBean.setCache(mockCache);
			indexFactoryBean.setName("TestIndex");
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("QueryService is required to create an Index", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void afterPropertiesSetWithUnspecifiedExpression() throws Exception {
		try {
			IndexFactoryBean indexFactoryBean = newIndexFactoryBean();
			indexFactoryBean.setName("TestIndex");
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Index expression is required", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void afterPropertiesSetWithUnspecifiedFromClause() throws Exception {
		expectedException.expect(IllegalArgumentException.class);
		expectedException.expectCause(is(nullValue(Throwable.class)));
		expectedException.expectMessage("Index from clause is required");

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setExpression("id");
		indexFactoryBean.setName("TestIndex");
		indexFactoryBean.afterPropertiesSet();
	}

	@Test(expected = IllegalArgumentException.class)
	public void afterPropertiesSetWithUnspecifiedIndexName() throws Exception {
		try {
			IndexFactoryBean indexFactoryBean = newIndexFactoryBean();
			indexFactoryBean.setExpression("id");
			indexFactoryBean.setFrom("/Example");
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Index name is required", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void afterPropertiesSetUsingImportsWithInvalidType() throws Exception {
		try {
			IndexFactoryBean indexFactoryBean = newIndexFactoryBean();
			indexFactoryBean.setExpression("id");
			indexFactoryBean.setFrom("/Example");
			indexFactoryBean.setImports("org.example.DomainType");
			indexFactoryBean.setName("TestIndex");
			indexFactoryBean.setType("PriMary_Key");
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("imports are not supported with a KEY Index", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void doLookupQueryService() {
		QueryService mockQueryServiceOne = mock(QueryService.class, "testLookupQueryService.MockQueryService.One");
		QueryService mockQueryServiceTwo = mock(QueryService.class, "testLookupQueryService.MockQueryService.Two");

		Cache mockCache = mock(Cache.class, "testDoLookupQueryService.MockCache");

		when(mockCache.getQueryService()).thenReturn(mockQueryServiceTwo);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setCache(mockCache);
		indexFactoryBean.setQueryService(mockQueryServiceOne);

		assertSame(mockQueryServiceOne, indexFactoryBean.doLookupQueryService());

		verify(mockCache, never()).getQueryService();
	}

	@Test
	public void doLookupQueryServiceOnClientCache() {
		ClientCache mockClientCache = mock(ClientCache.class, "testDoLookupQueryServiceOnClientCache.MockClientCache");

		QueryService mockQueryService = mock(QueryService.class, "testDoLookupQueryServiceOnClientCache.MockQueryService");

		when(mockClientCache.getLocalQueryService()).thenReturn(mockQueryService);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setCache(mockClientCache);
		indexFactoryBean.setQueryService(null);

		assertSame(mockQueryService, indexFactoryBean.doLookupQueryService());

		verify(mockClientCache, times(1)).getLocalQueryService();
		verify(mockClientCache, never()).getQueryService();
	}

	@Test
	public void doLookupQueryServiceOnPeerCache() {
		Cache mockCache = mock(Cache.class, "testDoLookupQueryServiceOnPeerCache.MockCache");

		QueryService mockQueryService = mock(QueryService.class, "testDoLookupQueryServiceOnPeerCache.MockQueryService");

		when(mockCache.getQueryService()).thenReturn(mockQueryService);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setCache(mockCache);
		indexFactoryBean.setQueryService(null);

		assertSame(mockQueryService, indexFactoryBean.doLookupQueryService());

		verify(mockCache, times(1)).getQueryService();
	}

	@Test
	public void lookupQueryServiceFromBeanFactory() {
		BeanFactory mockBeanFactory = mock(BeanFactory.class, "testLookupQueryServiceFromBeanFactory.MockBeanFactory");

		QueryService mockQueryService = mock(QueryService.class, "testLookupQueryServiceFromBeanFactory.MockQueryService");

		when(mockBeanFactory.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE)))
			.thenReturn(true);
		when(mockBeanFactory.getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE),
			eq(QueryService.class))).thenReturn(mockQueryService);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setBeanFactory(mockBeanFactory);

		QueryService actualQueryService = indexFactoryBean.lookupQueryService();

		assertSame(mockQueryService, actualQueryService);

		verify(mockBeanFactory, times(1)).containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE));
		verify(mockBeanFactory, times(1)).getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE),
			eq(QueryService.class));
	}

	@Test
	public void lookupQueryServiceFromCache() {
		BeanFactory mockBeanFactory = mock(BeanFactory.class, "testLookupQueryServiceFromCache.MockBeanFactory");

		when(mockBeanFactory.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE)))
			.thenReturn(false);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setBeanFactory(mockBeanFactory);
		indexFactoryBean.setDefine(false);
		indexFactoryBean.setQueryService(mockQueryService);

		QueryService actualQueryService = indexFactoryBean.lookupQueryService();

		assertSame(mockQueryService, actualQueryService);

		verify(mockBeanFactory, times(1)).containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE));
		verify(mockBeanFactory, never()).getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE),
			eq(QueryService.class));
	}

	@Test
	public void registerQueryServiceBeanWhenIndexIsCreated() {
		ConfigurableBeanFactory mockBeanFactory = mock(ConfigurableBeanFactory.class,
			"testRegisterQueryServiceBeanWhenIndexIsCreated.MockBeanFactory");

		QueryService mockQueryService = mock(QueryService.class, "testRegisterQueryServiceBeanWhenIndexIsCreated.MockQueryService");

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setBeanFactory(mockBeanFactory);
		indexFactoryBean.setDefine(false);

		assertSame(mockQueryService, indexFactoryBean.registerQueryServiceBean(mockQueryService));

		verify(mockBeanFactory, never()).registerSingleton(
			eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE), same(mockQueryService));
	}

	@Test
	public void registerQueryServiceBeanWhenIndexIsDefined() {
		ConfigurableBeanFactory mockBeanFactory = mock(ConfigurableBeanFactory.class,
			"testRegisterQueryServiceBeanWhenIndexIsDefined.MockBeanFactory");

		QueryService mockQueryService = mock(QueryService.class, "testRegisterQueryServiceBeanWhenIndexIsDefined.MockQueryService");

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setBeanFactory(mockBeanFactory);
		indexFactoryBean.setDefine(true);

		assertSame(mockQueryService, indexFactoryBean.registerQueryServiceBean(mockQueryService));

		verify(mockBeanFactory, times(1)).registerSingleton(
			eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE), same(mockQueryService));
	}

	@Test
	public void createIndexReturningNewKeyIndex() throws Exception {
		Index mockIndex = mock(Index.class, "createIndexReturningNewKeyIndex.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "createIndexReturningNewKeyIndex.MockQueryService");

		when(mockQueryService.getIndexes()).thenReturn(Collections.<Index>emptyList());
		when(mockQueryService.createKeyIndex(eq("KeyIndex"), eq("id"), eq("/Example"))).thenReturn(mockIndex);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setName("KeyIndex");
		indexFactoryBean.setOverride(false);
		indexFactoryBean.setType("key");

		Index actualIndex = indexFactoryBean.createIndex(mockQueryService, "KeyIndex");

		assertSame(mockIndex, actualIndex);

		verify(mockQueryService, times(1)).createKeyIndex(eq("KeyIndex"), eq("id"), eq("/Example"));
	}

	@Test
	public void createIndexReturningNewHashIndex() throws Exception {
		Index mockIndex = mock(Index.class, "createIndexReturningNewHashIndex.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "createIndexReturningNewHashIndex.MockQueryService");

		when(mockQueryService.getIndexes()).thenReturn(Collections.<Index>emptyList());
		when(mockQueryService.createHashIndex(eq("HashIndex"), eq("name"), eq("/Animals"), eq("org.example.Dog")))
			.thenReturn(mockIndex);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setExpression("name");
		indexFactoryBean.setFrom("/Animals");
		indexFactoryBean.setImports("org.example.Dog");
		indexFactoryBean.setName("HashIndex");
		indexFactoryBean.setOverride(false);
		indexFactoryBean.setType("HasH");

		Index actualIndex = indexFactoryBean.createIndex(mockQueryService, "HashIndex");

		assertSame(mockIndex, actualIndex);

		verify(mockQueryService, times(1)).createHashIndex(eq("HashIndex"), eq("name"), eq("/Animals"),
			eq("org.example.Dog"));
	}

	@Test
	public void createIndexReturningNewFunctionalIndex() throws Exception {
		Index mockIndex = mock(Index.class, "createIndexReturningNewFunctionalIndex.MockIndex");

		QueryService mockQueryService = mock(QueryService.class,
			"createIndexReturningNewFunctionalIndex.MockQueryService");

		when(mockQueryService.getIndexes()).thenReturn(Collections.<Index>emptyList());
		when(mockQueryService.createIndex(eq("FunctionalIndex"), eq("someField"), eq("/Example")))
			.thenReturn(mockIndex);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setExpression("someField");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setImports("  ");
		indexFactoryBean.setName("FunctionalIndex");
		indexFactoryBean.setOverride(false);
		indexFactoryBean.setType((String) null);

		Index actualIndex = indexFactoryBean.createIndex(mockQueryService, "FunctionalIndex");

		assertSame(mockIndex, actualIndex);

		verify(mockQueryService, times(1)).createIndex(eq("FunctionalIndex"), eq("someField"), eq("/Example"));
	}

	@Test
	public void createIndexOverridesExistingIndex() throws Exception {
		Index mockExistingIndex = mock(Index.class, "createIndexOverridesExistingIndex.MockExistingIndex");
		Index mockOverridingIndex = mock(Index.class, "createIndexOverridesExistingIndex.MockOverridingIndex");

		QueryService mockQueryService = mock(QueryService.class, "createIndexOverridesExistingIndex.MockQueryService");

		when(mockExistingIndex.getName()).thenReturn("ExistingIndex");
		when(mockOverridingIndex.getName()).thenReturn("OverridingIndex");
		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockExistingIndex));
		when(mockQueryService.createHashIndex(eq("ExistingIndex"), eq("someField"), eq("/Example"),
			eq("example.DomainType"))).thenReturn(mockOverridingIndex);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setExpression("someField");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setImports("example.DomainType");
		indexFactoryBean.setName("OverridingIndex");
		indexFactoryBean.setType("HASH");

		Index actualIndex = indexFactoryBean.createIndex(mockQueryService, "ExistingIndex");

		assertSame(mockOverridingIndex, actualIndex);

		verifyZeroInteractions(mockOverridingIndex);
		verify(mockExistingIndex, times(1)).getName();
		verify(mockQueryService, times(1)).removeIndex(same(mockExistingIndex));
		verify(mockQueryService, times(1)).createHashIndex(eq("ExistingIndex"), eq("someField"), eq("/Example"),
			eq("example.DomainType"));
	}

	@Test
	public void createIndexReturnsExistingIndex() throws Exception {
		Index mockExistingIndex = mock(Index.class, "createIndexReturnsExistingIndex.MockExistingIndex");
		Index mockNewIndex = mock(Index.class, "createIndexReturnsExistingIndex.MockNewIndex");

		QueryService mockQueryService = mock(QueryService.class, "createIndexReturnsExistingIndex.MockQueryService");

		when(mockExistingIndex.getName()).thenReturn("ExistingIndex");
		when(mockNewIndex.getName()).thenReturn("NewIndex");
		when(mockQueryService.getIndexes()).thenReturn(Arrays.asList(mockExistingIndex, mockNewIndex));

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setName("ExistingIndex");
		indexFactoryBean.setOverride(false);

		Index actualIndex = indexFactoryBean.createIndex(mockQueryService, "ExistingIndex");

		assertSame(mockExistingIndex, actualIndex);

		verify(mockExistingIndex, times(1)).getName();
		verify(mockNewIndex, never()).getName();
		verify(mockQueryService, times(1)).getIndexes();
	}

	@Test
	public void createIndexThrowsIndexNameConflictExceptionOnOverride() throws Exception {
		Index mockExistingIndex = mock(Index.class,
			"createIndexThrowsIndexNameConflictExceptionOnOverride.MockExistingIndex");

		QueryService mockQueryService = mock(QueryService.class,
			"createIndexThrowsIndexNameConflictExceptionOnOverride.MockQueryService");

		when(mockExistingIndex.getName()).thenReturn("ExistingIndex");
		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockExistingIndex));
		when(mockQueryService.createIndex(any(String.class), any(String.class), any(String.class)))
			.thenThrow(new IndexNameConflictException("TEST"));

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setExpression("someField");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setName("ExistingIndex");
		indexFactoryBean.setType("Functional");

		try {
			expectedException.expect(GemfireIndexException.class);
			expectedException.expectCause(isA(IndexNameConflictException.class));
			expectedException.expectMessage(
				"Failed to remove the existing Index on override before re-creating Index with name (ExistingIndex)");

			indexFactoryBean.createIndex(mockQueryService, "ExistingIndex");
		}
		finally {
			verify(mockExistingIndex, times(1)).getName();
			verify(mockQueryService, times(1)).getIndexes();
			verify(mockQueryService, times(1)).createIndex(eq("ExistingIndex"), eq("someField"), eq("/Example"));
		}
	}

	@Test
	public void createIndexThrowsIndexExistsException() throws Exception {
		QueryService mockQueryService = mock(QueryService.class,
			"createIndexThrowsIndexExistsException.MockQueryService");

		when(mockQueryService.getIndexes()).thenReturn(null);
		when(mockQueryService.createKeyIndex(eq("NewIndex"), eq("someField"), eq("/Example")))
			.thenThrow(new IndexExistsException("TEST"));

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setExpression("someField");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setName("NewIndex");
		indexFactoryBean.setOverride(true);
		indexFactoryBean.setType("PRIMARY_KEY");

		try {
			expectedException.expect(GemfireIndexException.class);
			expectedException.expectCause(isA(IndexExistsException.class));
			expectedException.expectMessage(
				"An Index with a different name having the same definition as this Index (NewIndex) already exists");

			indexFactoryBean.createIndex(mockQueryService, "NewIndex");
		}
		finally {
			verify(mockQueryService, times(1)).getIndexes();
			verify(mockQueryService, never()).removeIndex(any(Index.class));
			verify(mockQueryService, times(1)).createKeyIndex(eq("NewIndex"), eq("someField"), eq("/Example"));
		}
	}

	@Test
	public void createIndexAddsExistingIndexOnAnyException() throws Exception {
		final Index mockExistingIndex = mock(Index.class, "createIndexAddsExistingIndexOnAnyException.MockExistingIndex");
		final Index mockIndexTwo = mock(Index.class, "createIndexAddsExistingIndexOnException.MockIndexTwo");

		final List<Index> indexes = new ArrayList<Index>(1);

		indexes.add(mockIndexTwo);

		QueryService mockQueryService = mock(QueryService.class,
			"createIndexAddsExistingIndexOnAnyException.MockQueryService");

		when(mockExistingIndex.getName()).thenReturn("ExistingIndex");
		when(mockIndexTwo.getName()).thenReturn("NewIndex");

		when(mockQueryService.getIndexes()).then(new Answer<Collection<Index>>() {
			private boolean called = false;

			private synchronized boolean wasCalledOnce() {
				boolean localCalled = this.called;
				this.called = true;
				return localCalled;
			}

			@Override
			public Collection<Index> answer(final InvocationOnMock invocationOnMock) throws Throwable {
				return (wasCalledOnce() ? indexes : Collections.singletonList(mockExistingIndex));
			}
		});

		when(mockQueryService.createIndex(eq("ExistingIndex"), eq("someField"), eq("/Example")))
			.thenThrow(new RuntimeException("TEST"));

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setExpression("someField");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setName("ExistingIndex");
		indexFactoryBean.setOverride(true);
		indexFactoryBean.setType("FUNCTIONAL");

		assertEquals(1, indexes.size());
		assertFalse(indexes.contains(mockExistingIndex));

		Index actualIndex = indexFactoryBean.createIndex(mockQueryService, "ExistingIndex");

		assertThat(actualIndex, is(sameInstance(mockExistingIndex)));
		assertThat(indexes.size(), is(equalTo(2)));
		assertThat(indexes.contains(mockExistingIndex), is(true));
		assertThat(indexes.contains(mockIndexTwo), is(true));

		verify(mockQueryService, times(3)).getIndexes();
		verify(mockQueryService, times(1)).removeIndex(same(mockExistingIndex));
		verify(mockQueryService, times(1)).createIndex(eq("ExistingIndex"), eq("someField"), eq("/Example"));
	}

	@Test(expected = RuntimeException.class)
	public void createIndexThrowsExceptionOnAbsoluteFailure() throws Exception {
		Index mockExistingIndex = mock(Index.class, "createIndexThrowsExceptionOnAbsoluteFailure.MockIndex.Existing");

		QueryService mockQueryService = mock(QueryService.class,
			"createIndexThrowsExceptionOnAbsoluteFailure.MockQueryService");

		when(mockExistingIndex.getName()).thenReturn("ExistingIndex");
		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockExistingIndex));
		when(mockQueryService.createIndex(eq("ExistingIndex"), eq("someField"), eq("/Example")))
			.thenThrow(new RuntimeException("test"));

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setExpression("someField");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setName("ExistingIndex");

		try {
			indexFactoryBean.createIndex(mockQueryService, "ExistingIndex");
		}
		catch (RuntimeException expected) {
			assertEquals("test", expected.getMessage());
			throw expected;
		}
		finally {
			verify(mockQueryService, times(2)).getIndexes();
			verify(mockQueryService, times(1)).removeIndex(same(mockExistingIndex));
			verify(mockQueryService, times(1)).createIndex(eq("ExistingIndex"), eq("someField"), eq("/Example"));
		}
	}

	@Test
	public void createFunctionalIndex() throws Exception {
		Index mockIndex = mock(Index.class, "testCreateFunctionalIndex.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "testCreateFunctionalIndex.MockQueryService");

		when(mockQueryService.createIndex(eq("FunctionalIndex"), eq("purchaseDate"), eq("/Orders")))
			.thenReturn(mockIndex);

		Index actualIndex = indexFactoryBean.createFunctionalIndex(mockQueryService, "FunctionalIndex",
			"purchaseDate", "/Orders", null);

		assertSame(mockIndex, actualIndex);

		verify(mockQueryService, times(1)).createIndex(eq("FunctionalIndex"), eq("purchaseDate"), eq("/Orders"));
		verify(mockQueryService, never()).defineIndex(anyString(), anyString(), anyString());
	}

	@Test
	public void createFunctionalIndexWithImports() throws Exception {
		Index mockIndex = mock(Index.class, "testCreateFunctionalIndexWithImports.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "testCreateFunctionalIndexWithImports.MockQueryService");

		when(mockQueryService.createIndex(eq("FunctionalIndexWithImports"), eq("purchaseDate"), eq("/Orders"),
			eq("org.example.Order"))).thenReturn(mockIndex);

		Index actualIndex = indexFactoryBean.createFunctionalIndex(mockQueryService, "FunctionalIndexWithImports",
			"purchaseDate", "/Orders", "org.example.Order");

		assertSame(mockIndex, actualIndex);

		verify(mockQueryService, times(1)).createIndex(eq("FunctionalIndexWithImports"), eq("purchaseDate"),
			eq("/Orders"), eq("org.example.Order"));
		verify(mockQueryService, never()).defineIndex(anyString(), anyString(), anyString(), anyString());
	}

	@Test
	public void defineFunctionalIndex() throws Exception {
		QueryService mockQueryService = mock(QueryService.class, "testDefineFunctionalIndex.MockQueryService");

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				return null;
			}
		}).when(mockQueryService).defineIndex(eq("FunctionalIndex"), eq("purchaseDate"), eq("/Orders"));

		indexFactoryBean.setDefine(true);

		Index actualIndex = indexFactoryBean.createFunctionalIndex(mockQueryService, "FunctionalIndex",
			"purchaseDate", "/Orders", null);

		assertTrue(actualIndex instanceof IndexFactoryBean.IndexWrapper);
		assertSame(mockQueryService, ((IndexFactoryBean.IndexWrapper) actualIndex).getQueryService());
		assertEquals("FunctionalIndex", ((IndexFactoryBean.IndexWrapper) actualIndex).getIndexName());

		verify(mockQueryService, never()).createIndex(anyString(), anyString(), anyString());
		verify(mockQueryService, times(1)).defineIndex(eq("FunctionalIndex"), eq("purchaseDate"), eq("/Orders"));
	}

	@Test
	public void defineFunctionalIndexWithImports() throws Exception {
		QueryService mockQueryService = mock(QueryService.class, "testDefineFunctionalIndexWithImports.MockQueryService");

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				return null;
			}
		}).when(mockQueryService).defineIndex(eq("FunctionalIndex"), eq("purchaseDate"), eq("/Orders"),
			eq("org.example.Order"));

		indexFactoryBean.setDefine(true);

		Index actualIndex = indexFactoryBean.createFunctionalIndex(mockQueryService, "FunctionalIndexWithImports",
			"purchaseDate", "/Orders", "org.example.Order");

		assertTrue(actualIndex instanceof IndexFactoryBean.IndexWrapper);
		assertSame(mockQueryService, ((IndexFactoryBean.IndexWrapper) actualIndex).getQueryService());
		assertEquals("FunctionalIndexWithImports", ((IndexFactoryBean.IndexWrapper) actualIndex).getIndexName());

		verify(mockQueryService, never()).createIndex(anyString(), anyString(), anyString(), anyString());
		verify(mockQueryService, times(1)).defineIndex(eq("FunctionalIndexWithImports"), eq("purchaseDate"),
			eq("/Orders"), eq("org.example.Order"));
	}

	@Test
	public void createHashIndex() throws Exception {
		Index mockIndex = mock(Index.class, "testCreateHashIndex.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "testCreateHashIndex.MockQueryService");

		when(mockQueryService.createHashIndex(eq("HashIndex"), eq("name"), eq("/People"))).thenReturn(mockIndex);

		Index actualIndex = indexFactoryBean.createHashIndex(mockQueryService, "HashIndex", "name", "/People", "  ");

		assertSame(mockIndex, actualIndex);

		verify(mockQueryService, times(1)).createHashIndex(eq("HashIndex"), eq("name"), eq("/People"));
		verify(mockQueryService, never()).defineHashIndex(anyString(), anyString(), anyString());
	}

	@Test
	public void createHashIndexWithImports() throws Exception {
		Index mockIndex = mock(Index.class, "testCreateHashIndexWithImports.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "testCreateHashIndexWithImports.MockQueryService");

		when(mockQueryService.createHashIndex(eq("HashIndexWithImports"), eq("name"), eq("/People"),
			eq("org.example.Person"))).thenReturn(mockIndex);

		Index actualIndex = indexFactoryBean.createHashIndex(mockQueryService, "HashIndexWithImports",
			"name", "/People", "org.example.Person");

		assertSame(mockIndex, actualIndex);

		verify(mockQueryService, times(1)).createHashIndex(eq("HashIndexWithImports"), eq("name"), eq("/People"),
			eq("org.example.Person"));
		verify(mockQueryService, never()).defineHashIndex(anyString(), anyString(), anyString(), anyString());
	}

	@Test
	public void defineHashIndex() throws Exception {
		QueryService mockQueryService = mock(QueryService.class, "testDefineHashIndex.MockQueryService");

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				return null;
			}
		}).when(mockQueryService).defineHashIndex(eq("HashIndex"), eq("name"), eq("/People"));

		indexFactoryBean.setDefine(true);

		Index actualIndex = indexFactoryBean.createHashIndex(mockQueryService, "HashIndex",
			"name", "/People", "  ");

		assertTrue(actualIndex instanceof IndexFactoryBean.IndexWrapper);
		assertSame(mockQueryService, ((IndexFactoryBean.IndexWrapper) actualIndex).getQueryService());
		assertEquals("HashIndex", ((IndexFactoryBean.IndexWrapper) actualIndex).getIndexName());

		verify(mockQueryService, never()).createHashIndex(anyString(), anyString(), anyString());
		verify(mockQueryService, times(1)).defineHashIndex(eq("HashIndex"), eq("name"), eq("/People"));
	}

	@Test
	public void defineHashIndexWithImports() throws Exception {
		QueryService mockQueryService = mock(QueryService.class, "testDefineHashIndexWithImports.MockQueryService");

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				return null;
			}
		}).when(mockQueryService).defineHashIndex(eq("HashIndexWithImports"), eq("name"), eq("/People"),
			eq("org.example.Person"));

		indexFactoryBean.setDefine(true);

		Index actualIndex = indexFactoryBean.createHashIndex(mockQueryService, "HashIndexWithImports",
			"name", "/People", "org.example.Person");

		assertTrue(actualIndex instanceof IndexFactoryBean.IndexWrapper);
		assertSame(mockQueryService, ((IndexFactoryBean.IndexWrapper) actualIndex).getQueryService());
		assertEquals("HashIndexWithImports", ((IndexFactoryBean.IndexWrapper) actualIndex).getIndexName());

		verify(mockQueryService, never()).createHashIndex(anyString(), anyString(), anyString(), anyString());
		verify(mockQueryService, times(1)).defineHashIndex(eq("HashIndexWithImports"), eq("name"), eq("/People"),
			eq("org.example.Person"));
	}

	@Test
	public void createKeyIndex() throws Exception {
		QueryService mockQueryService = mock(QueryService.class, "testCreateKeyIndex.MockQueryService");

		Index mockIndex = mock(Index.class, "testCreateKeyIndex.MockKeyIndex");

		when(mockQueryService.createKeyIndex(eq("KeyIndex"), eq("id"), eq("/Example"))).thenReturn(mockIndex);

		indexFactoryBean.setDefine(false);

		Index actualIndex = indexFactoryBean.createKeyIndex(mockQueryService, "KeyIndex", "id", "/Example");

		assertSame(mockIndex, actualIndex);

		verify(mockQueryService, times(1)).createKeyIndex(eq("KeyIndex"), eq("id"), eq("/Example"));
		verify(mockQueryService, never()).defineKeyIndex(anyString(), anyString(), anyString());
	}

	@Test
	public void defineKeyIndex() throws Exception {
		QueryService mockQueryService = mock(QueryService.class, "testDefineKeyIndex.MockQueryService");

		indexFactoryBean.setDefine(true);

		Index actualIndex = indexFactoryBean.createKeyIndex(mockQueryService, "KeyIndex", "id", "/Example");

		assertTrue(actualIndex instanceof IndexFactoryBean.IndexWrapper);

		IndexFactoryBean.IndexWrapper indexWrapper = (IndexFactoryBean.IndexWrapper) actualIndex;

		assertSame(mockQueryService, indexWrapper.getQueryService());
		assertEquals("KeyIndex", indexWrapper.getIndexName());

		verify(mockQueryService, never()).createKeyIndex(anyString(), anyString(), anyString());
		verify(mockQueryService, times(1)).defineKeyIndex(eq("KeyIndex"), eq("id"), eq("/Example"));
	}

	@Test
	public void getExistingIndex() {
		Index mockIndexOne = mock(Index.class, "testGetExistingIndex.MockIndex.One");
		Index mockIndexTwo = mock(Index.class, "testGetExistingIndex.MockIndex.Two");
		Index mockIndexThree = mock(Index.class, "testGetExistingIndex.MockIndex.Two");

		QueryService mockQueryService = mock(QueryService.class, "testGetExistingIndex.MockQueryService");

		when(mockIndexOne.getName()).thenReturn("PrimaryIndex");
		when(mockIndexTwo.getName()).thenReturn("SecondaryIndex");
		when(mockIndexThree.getName()).thenReturn("TernaryIndex");
		when(mockQueryService.getIndexes()).thenReturn(Arrays.asList(mockIndexOne, mockIndexTwo, mockIndexThree));

		assertNull(indexFactoryBean.getExistingIndex(mockQueryService, null));
		assertNull(indexFactoryBean.getExistingIndex(mockQueryService, ""));
		assertNull(indexFactoryBean.getExistingIndex(mockQueryService, "  "));
		assertNull(indexFactoryBean.getExistingIndex(mockQueryService, "Primary Index"));
		assertNull(indexFactoryBean.getExistingIndex(mockQueryService, "Secondary_Index"));
		assertNull(indexFactoryBean.getExistingIndex(mockQueryService, "QuadIndex"));
		assertSame(mockIndexOne, indexFactoryBean.getExistingIndex(mockQueryService, "PRIMARYINDEX"));
		assertSame(mockIndexTwo, indexFactoryBean.getExistingIndex(mockQueryService, "SecondaryIndex"));
		assertSame(mockIndexThree, indexFactoryBean.getExistingIndex(mockQueryService, "ternaryindex"));
	}

	@Test
	public void getObjectLooksUpIndex() throws Exception {
		QueryService mockQueryService = mock(QueryService.class, "testGetObjectLooksUpIndex.MockQueryService");

		when(mockQueryService.getIndexes()).then(new Answer<Collection<Index>>() {
			private final AtomicInteger counter = new AtomicInteger(1);
			@Override public Collection<Index> answer(final InvocationOnMock invocation) throws Throwable {
				Index mockIndex = mock(Index.class, "getObjectLooksUpIndex.MockIndex");
				when(mockIndex.getName()).thenReturn(String.format("MockIndex%1$s", counter.getAndIncrement()));
				return Collections.singletonList(mockIndex);
			}
		});

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setQueryService(mockQueryService);
		indexFactoryBean.setName("MockIndex1");
		ReflectionUtils.setField(IndexFactoryBean.class.getDeclaredField("indexName"), indexFactoryBean, "MockIndex1");

		Index actualIndex = indexFactoryBean.getObject();

		assertNotNull(actualIndex);
		assertEquals("MockIndex1", actualIndex.getName());

		Index sameIndex = indexFactoryBean.getObject();

		assertSame(actualIndex, sameIndex);
	}

	@Test
	public void indexFactoryBeanCreatesSingleIndex() {
		assertTrue(indexFactoryBean.isSingleton());
	}

	@Test
	public void defineMultipleIndexesWithSeparateIndexFactoryBeansSameSpringContext() throws Exception {
		ConfigurableBeanFactory mockBeanFactory = mock(ConfigurableBeanFactory.class,
			"testDefineMultipleIndexesWithSeparateIndexFactoryBeansSameSpringContext.MockBeanFactory");

		Cache mockCacheOne = mock(Cache.class, "testDefineMultipleIndexesWithSeparateIndexFactoryBeansSameSpringContext.MockCacheOne");
		Cache mockCacheTwo = mock(Cache.class, "testDefineMultipleIndexesWithSeparateIndexFactoryBeansSameSpringContext.MockCacheTwo");

		QueryService mockQueryServiceOne = mock(QueryService.class,
			"testDefineMultipleIndexesWithSeparateIndexFactoryBeansSameSpringContext.MockQueryServiceOne");

		QueryService mockQueryServiceTwo = mock(QueryService.class,
			"testDefineMultipleIndexesWithSeparateIndexFactoryBeansSameSpringContext.MockQueryServiceTwo");

		final AtomicReference<QueryService> queryServiceReference = new AtomicReference<QueryService>(null);

		doAnswer(new Answer<Boolean>() {
			@Override public Boolean answer(final InvocationOnMock invocation) throws Throwable {
				return (queryServiceReference.get() != null);
			}
		}).when(mockBeanFactory).containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE));

		doAnswer(new Answer<QueryService>() {
			@Override public QueryService answer(final InvocationOnMock invocation) throws Throwable {
				return queryServiceReference.get();
			}
		}).when(mockBeanFactory).getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE),
			eq(QueryService.class));

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				assertEquals(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE,
					invocation.getArgument(0));
				queryServiceReference.compareAndSet(null, invocation.getArgument(1));
				return null;
			}
		}).when(mockBeanFactory).registerSingleton(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE),
			any(QueryService.class));

		when(mockCacheOne.getQueryService()).thenReturn(mockQueryServiceOne);
		when(mockCacheTwo.getQueryService()).thenReturn(mockQueryServiceTwo);

		IndexFactoryBean indexFactoryBeanOne = new IndexFactoryBean();

		indexFactoryBeanOne.setBeanFactory(mockBeanFactory);
		indexFactoryBeanOne.setCache(mockCacheOne);
		indexFactoryBeanOne.setDefine(true);
		indexFactoryBeanOne.setExpression("id");
		indexFactoryBeanOne.setFrom("/People");
		indexFactoryBeanOne.setName("PersonIdIndex");
		indexFactoryBeanOne.setType("Key");
		indexFactoryBeanOne.afterPropertiesSet();

		IndexFactoryBean indexFactoryBeanTwo = new IndexFactoryBean();

		indexFactoryBeanTwo.setBeanFactory(mockBeanFactory);
		indexFactoryBeanTwo.setCache(mockCacheTwo);
		indexFactoryBeanTwo.setDefine(true);
		indexFactoryBeanTwo.setExpression("purchaseDate");
		indexFactoryBeanTwo.setFrom("/Orders");
		indexFactoryBeanTwo.setImports("org.example.Order");
		indexFactoryBeanTwo.setName("PurchaseDateIndex");
		indexFactoryBeanTwo.setType("HASH");
		indexFactoryBeanTwo.afterPropertiesSet();

		verify(mockBeanFactory, times(2)).containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE));
		verify(mockBeanFactory, times(1)).getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE),
			eq(QueryService.class));
		verify(mockBeanFactory, times(1)).registerSingleton(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE),
			same(mockQueryServiceOne));
		verify(mockCacheOne, times(1)).getQueryService();
		verify(mockCacheTwo, never()).getQueryService();
		verify(mockQueryServiceOne, times(1)).defineKeyIndex(eq("PersonIdIndex"), eq("id"), eq("/People"));
		verify(mockQueryServiceOne, times(1)).defineHashIndex(eq("PurchaseDateIndex"), eq("purchaseDate"), eq("/Orders"),
			eq("org.example.Order"));
		verify(mockQueryServiceTwo, never()).defineHashIndex(anyString(), anyString(), anyString());
		verify(mockQueryServiceTwo, never()).defineHashIndex(anyString(), anyString(), anyString());
		verify(mockQueryServiceTwo, never()).defineIndex(anyString(), anyString(), anyString());
		verify(mockQueryServiceTwo, never()).defineIndex(anyString(), anyString(), anyString(), anyString());
		verify(mockQueryServiceTwo, never()).defineKeyIndex(anyString(), anyString(), anyString());
	}

	@Test
	@SuppressWarnings("deprecation")
	public void indexWrapperDelegation() {
		Index mockIndex = mock(Index.class, "indexWrapperDelegation.MockIndex");

		IndexStatistics mockIndexStats = mock(IndexStatistics.class, "indexWrapperDelegation.MockIndexStats");

		QueryService mockQueryService = mock(QueryService.class, "indexWrapperDelegation.MockQueryService");

		when(mockIndex.getCanonicalizedFromClause()).thenReturn("/Example");
		when(mockIndex.getCanonicalizedIndexedExpression()).thenReturn("ID");
		when(mockIndex.getCanonicalizedProjectionAttributes()).thenReturn("identifier");
		when(mockIndex.getFromClause()).thenReturn("Example");
		when(mockIndex.getIndexedExpression()).thenReturn("id");
		when(mockIndex.getName()).thenReturn("MockIndex");
		when(mockIndex.getProjectionAttributes()).thenReturn("id");
		when(mockIndex.getStatistics()).thenReturn(mockIndexStats);
		when(mockIndex.getType()).thenReturn(org.apache.geode.cache.query.IndexType.HASH);
		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockIndex));

		IndexFactoryBean.IndexWrapper indexWrapper = new IndexFactoryBean.IndexWrapper(mockQueryService, "MockIndex");

		assertNotNull(indexWrapper);
		assertEquals("MockIndex", indexWrapper.getIndexName());
		assertSame(mockQueryService, indexWrapper.getQueryService());

		Index actualIndex = indexWrapper.getIndex();

		assertSame(mockIndex, actualIndex);
		assertEquals("/Example", indexWrapper.getCanonicalizedFromClause());
		assertEquals("ID", indexWrapper.getCanonicalizedIndexedExpression());
		assertEquals("identifier", indexWrapper.getCanonicalizedProjectionAttributes());
		assertEquals("Example", indexWrapper.getFromClause());
		assertEquals("id", indexWrapper.getIndexedExpression());
		assertEquals("MockIndex", indexWrapper.getName());
		assertEquals("id", indexWrapper.getProjectionAttributes());
		assertSame(mockIndexStats, indexWrapper.getStatistics());
		assertEquals(org.apache.geode.cache.query.IndexType.HASH, indexWrapper.getType());

		Index sameIndex = indexWrapper.getIndex();

		assertSame(actualIndex, sameIndex);
	}

	@Test(expected = IllegalArgumentException.class)
	public void createIndexWrapperWithNullQueryService() {
		try {
			new IndexFactoryBean.IndexWrapper(null, "TestIndex");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("QueryService must not be null", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void createIndexWrapperWithUnspecifiedIndexName() {
		try {
			new IndexFactoryBean.IndexWrapper(mock(QueryService.class), "  ");
		}
		catch (IllegalArgumentException expected) {
			assertEquals("The name of the Index must be specified!", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = GemfireIndexException.class)
	public void indexWrapperGetIndexWhenIndexNotFound() {
		QueryService mockQueryService = mock(QueryService.class, "indexWrapperGetIndexWhenIndexNotFound.MockQueryService");

		when(mockQueryService.getIndexes()).thenReturn(Collections.<Index>emptyList());

		IndexFactoryBean.IndexWrapper indexWrapper = new IndexFactoryBean.IndexWrapper(mockQueryService, "NonExistingIndex");

		assertNotNull(indexWrapper);
		assertEquals("NonExistingIndex", indexWrapper.getIndexName());
		assertSame(mockQueryService, indexWrapper.getQueryService());

		try {
			indexWrapper.getIndex();
		}
		catch (GemfireIndexException expected) {
			assertThat(expected.getMessage(), startsWith("index with name (NonExistingIndex) was not found"));
			assertTrue(expected.getCause() instanceof IndexInvalidException);
			throw expected;
		}
	}
}
