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

import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.same;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.After;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.data.util.ReflectionUtils;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.query.Index;
import com.gemstone.gemfire.cache.query.IndexExistsException;
import com.gemstone.gemfire.cache.query.IndexInvalidException;
import com.gemstone.gemfire.cache.query.IndexStatistics;
import com.gemstone.gemfire.cache.query.QueryService;

/**
 * The IndexFactoryBeanTest class is a test suite of test cases testing the contract and functionality
 * of the IndexFactoryBean class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.springframework.data.gemfire.IndexFactoryBean
 * @see com.gemstone.gemfire.cache.Cache
 * @see com.gemstone.gemfire.cache.client.ClientCache
 * @see com.gemstone.gemfire.cache.query.Index
 * @see com.gemstone.gemfire.cache.query.QueryService
 * @since 1.5.2
 */
public class IndexFactoryBeanTest {

	private IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

	@After
	public void tearDown() {
		indexFactoryBean.setDefine(false);
	}

	@Test
	public void afterPropertiesSetIsSuccessful() throws Exception {
		Cache mockCache = mock(Cache.class, "afterPropertiesSet.MockCache");

		QueryService mockQueryService = mock(QueryService.class, "afterPropertiesSet.MockQueryService");

		when(mockCache.getQueryService()).thenReturn(mockQueryService);

		final Index mockIndex = mock(Index.class, "afterPropertiesSet.MockIndex");

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean() {
			@Override Index createIndex(final QueryService queryService, final String indexName) throws Exception {
				return mockIndex;
			}
		};

		indexFactoryBean.setCache(mockCache);
		indexFactoryBean.setName("TestIndex");
		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setType("key");
		indexFactoryBean.afterPropertiesSet();

		assertEquals(mockIndex, indexFactoryBean.getObject());
		assertEquals(mockIndex, indexFactoryBean.getObject()); // assert Index really is a 'Singleton'
		assertTrue(Index.class.isAssignableFrom(indexFactoryBean.getObjectType()));
		assertTrue(indexFactoryBean.isSingleton());
	}

	@Test(expected = IllegalArgumentException.class)
	public void afterPropertiesSetWithNullCache() throws Exception {
		try {
			new IndexFactoryBean().afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("The GemFire Cache reference must not be null!", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void afterPropertiesSetWithNullQueryService() throws Exception {
		try {
			Cache mockCache = mock(Cache.class, "afterPropertiesSetWithNullQueryService.MockCache");

			when(mockCache.getQueryService()).thenReturn(null);

			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

			indexFactoryBean.setCache(mockCache);
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("QueryService is required to create an Index!", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void afterPropertiesSetWithUnspecifiedExpression() throws Exception {
		try {
			Cache mockCache = mock(Cache.class, "afterPropertiesSetWithUnspecifiedExpression.MockCache");

			QueryService mockQueryService = mock(QueryService.class,
				"afterPropertiesSetWithUnspecifiedExpression.MockQueryService");

			when(mockCache.getQueryService()).thenReturn(mockQueryService);

			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

			indexFactoryBean.setCache(mockCache);
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Index 'expression' is required!", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void afterPropertiesSetWithUnspecifiedFromClause() throws Exception {
		try {
			Cache mockCache = mock(Cache.class, "afterPropertiesSetWithUnspecifiedFromClause.MockCache");

			QueryService mockQueryService = mock(QueryService.class,
				"afterPropertiesSetWithUnspecifiedFromClause.MockQueryService");

			when(mockCache.getQueryService()).thenReturn(mockQueryService);

			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

			indexFactoryBean.setCache(mockCache);
			indexFactoryBean.setExpression("id");
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("Index 'from' clause (a Region's full-path) is required!", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void afterPropertiesSetWithUnspecifiedIndexName() throws Exception {
		try {
			Cache mockCache = mock(Cache.class, "afterPropertiesSetWithUnspecifiedIndexName.MockCache");

			QueryService mockQueryService = mock(QueryService.class,
				"afterPropertiesSetWithUnspecifiedIndexName.MockQueryService");

			when(mockCache.getQueryService()).thenReturn(mockQueryService);

			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

			indexFactoryBean.setCache(mockCache);
			indexFactoryBean.setExpression("id");
			indexFactoryBean.setFrom("/Example");
			indexFactoryBean.setImports("org.example.DomainType");
			indexFactoryBean.setType("hash");
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("The Index bean id or name is required!", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void afterPropertiesSetWithInvalidTypeUsingImports() throws Exception {
		try {
			Cache mockCache = mock(Cache.class, "afterPropertiesSetWithInvalidTypeUsingImports.MockCache");

			QueryService mockQueryService = mock(QueryService.class,
				"afterPropertiesSetWithInvalidTypeUsingImports.MockQueryService");

			when(mockCache.getQueryService()).thenReturn(mockQueryService);

			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

			indexFactoryBean.setCache(mockCache);
			indexFactoryBean.setExpression("id");
			indexFactoryBean.setFrom("/Example");
			indexFactoryBean.setImports("org.example.DomainType");
			indexFactoryBean.setType("PriMary_Key");
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("The 'imports' property is not supported for a Key Index.", expected.getMessage());
			throw expected;
		}
	}

	@Test
	public void testLookupQueryService() {
		QueryService mockQueryServiceOne = mock(QueryService.class, "testLookupQueryService.MockQueryService.One");
		QueryService mockQueryServiceTwo = mock(QueryService.class, "testLookupQueryService.MockQueryService.Two");

		Cache mockCache = mock(Cache.class, "testLookupQueryService.MockCache");

		when(mockCache.getQueryService()).thenReturn(mockQueryServiceTwo);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setCache(mockCache);
		indexFactoryBean.setQueryService(mockQueryServiceOne);

		assertSame(mockQueryServiceOne, indexFactoryBean.lookupQueryService());

		verify(mockCache, never()).getQueryService();
	}

	@Test
	public void lookupQueryServiceOnClientCache() {
		ClientCache mockClientCache = mock(ClientCache.class, "lookupQueryServiceOnClientCache.MockClientCache");

		QueryService mockQueryService = mock(QueryService.class, "lookupQueryServiceOnClientCache.MockQueryService");

		when(mockClientCache.getLocalQueryService()).thenReturn(mockQueryService);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setCache(mockClientCache);

		assertSame(mockQueryService, indexFactoryBean.lookupQueryService());

		verify(mockClientCache, times(1)).getLocalQueryService();
	}

	@Test
	public void lookupQueryServiceOnPeerCache() {
		Cache mockCache = mock(Cache.class, "lookupQueryServiceOnPeerCache.MockCache");

		QueryService mockQueryService = mock(QueryService.class, "lookupQueryServiceOnPeerCache.MockQueryService");

		when(mockCache.getQueryService()).thenReturn(mockQueryService);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setCache(mockCache);
		indexFactoryBean.setQueryService(null);

		assertSame(mockQueryService, indexFactoryBean.lookupQueryService());

		verify(mockCache, times(1)).getQueryService();
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
	public void createIndexReturnsExistingIndex() throws Exception {
		Index mockExistingIndex = mock(Index.class, "createIndexReturnsExistingIndex.MockIndex.Existing");
		Index mockIndexTwo = mock(Index.class, "createIndexReturnsExistingIndex.MockIndex.Two");

		QueryService mockQueryService = mock(QueryService.class, "createIndexReturnsExistingIndex.MockQueryService");

		when(mockExistingIndex.getName()).thenReturn("ExistingIndex");
		when(mockIndexTwo.getName()).thenReturn("IndexTwo");
		when(mockQueryService.getIndexes()).thenReturn(Arrays.asList(mockExistingIndex, mockIndexTwo));

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setName("ExistingIndex");
		indexFactoryBean.setOverride(false);

		Index actualIndex = indexFactoryBean.createIndex(mockQueryService, "ExistingIndex");

		assertSame(mockExistingIndex, actualIndex);
	}

	@Test
	public void createIndexReturnsExistingIndexAndThrowsIndexExistsExceptionOnCreate() throws Exception {
		Index mockExistingIndex = mock(Index.class,
			"createIndexReturnsExistingIndexAndThrowsIndexExistsExceptionOnCreate.MockIndex.Existing");

		QueryService mockQueryService = mock(QueryService.class,
			"createIndexReturnsExistingIndexAndThrowsIndexExistsExceptionOnCreate.MockQueryService");

		when(mockExistingIndex.getName()).thenReturn("ExistingIndex");
		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockExistingIndex));
		when(mockQueryService.createKeyIndex(eq("ExistingIndex"), eq("id"), eq("/Example")))
			.thenThrow(new IndexExistsException("test"));

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setName("ExistingIndex");
		indexFactoryBean.setOverride(true);
		indexFactoryBean.setType("PRIMARY_KEY");

		Index actualIndex = indexFactoryBean.createIndex(mockQueryService, "ExistingIndex");

		assertSame(mockExistingIndex, actualIndex);

		verify(mockExistingIndex, times(2)).getName();
		verify(mockQueryService, times(2)).getIndexes();
		verify(mockQueryService, times(1)).removeIndex(same(mockExistingIndex));
		verify(mockQueryService, times(1)).createKeyIndex(eq("ExistingIndex"), eq("id"), eq("/Example"));
	}

	@Test
	public void createIndexOverridesExistingIndex() throws Exception {
		Index mockExistingIndex = mock(Index.class, "createIndexOverridesExistingIndex.MockIndex.Existing");
		Index mockOverridingIndex = mock(Index.class, "createIndexOverridesExistingIndex.MockIndex.Overriding");

		QueryService mockQueryService = mock(QueryService.class, "createIndexOverridesExistingIndex.MockQueryService");

		when(mockExistingIndex.getName()).thenReturn("ExistingIndex");
		when(mockOverridingIndex.getName()).thenReturn("OverridingIndex");
		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockExistingIndex));
		when(mockQueryService.createHashIndex(eq("ExistingIndex"), eq("someField"), eq("/Example"),
			eq("org.example.DomainType"))).thenReturn(mockOverridingIndex);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setExpression("someField");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setImports("org.example.DomainType");
		indexFactoryBean.setName("OverridingIndex");
		indexFactoryBean.setType("HASH");

		Index actualIndex = indexFactoryBean.createIndex(mockQueryService, "ExistingIndex");

		assertSame(mockOverridingIndex, actualIndex);

		verify(mockQueryService, times(1)).getIndexes();
		verify(mockQueryService, times(1)).removeIndex(same(mockExistingIndex));
		verify(mockQueryService, times(1)).createHashIndex(eq("ExistingIndex"), eq("someField"), eq("/Example"),
			eq("org.example.DomainType"));
	}

	@Test
	public void createIndexAddsExistingIndexOnException() throws Exception {
		final Index mockExistingIndex = mock(Index.class, "createIndexAddsExistingIndexOnException.MockIndex.Existing");
		final Index mockIndexTwo = mock(Index.class, "createIndexAddsExistingIndexOnException.MockIndex.Two");

		final List<Index> indexes = new ArrayList<Index>(1);

		indexes.add(mockIndexTwo);

		QueryService mockQueryService = mock(QueryService.class,
			"createIndexAddsExistingIndexOnException.MockQueryService");

		when(mockExistingIndex.getName()).thenReturn("ExistingIndex");
		when(mockIndexTwo.getName()).thenReturn("IndexTwo");

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
			.thenThrow(new RuntimeException("test"));

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setExpression("someField");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setName("ExistingIndex");
		indexFactoryBean.setOverride(true);
		indexFactoryBean.setType("FUNCTIONAL");

		assertEquals(1, indexes.size());
		assertFalse(indexes.contains(mockExistingIndex));

		Index actualIndex = indexFactoryBean.createIndex(mockQueryService, "ExistingIndex");

		assertSame(mockExistingIndex, actualIndex);
		assertEquals(2, indexes.size());
		assertTrue(indexes.contains(mockExistingIndex));

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
	public void createIndexDefinesKeyIndex() throws Exception {
		QueryService mockQueryService = mock(QueryService.class, "createIndexDefinesKeyIndex.MockQueryService");

		when(mockQueryService.getIndexes()).thenReturn(Collections.<Index>emptyList());

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setDefine(true);
		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setName("MockIndex");
		indexFactoryBean.setType("key");

		Index actualIndex = indexFactoryBean.createIndex(mockQueryService, "MockIndex");

		assertTrue(actualIndex instanceof IndexFactoryBean.IndexWrapper);

		IndexFactoryBean.IndexWrapper indexWrapper = (IndexFactoryBean.IndexWrapper) actualIndex;

		assertSame(mockQueryService, indexWrapper.getQueryService());
		assertEquals("MockIndex", indexWrapper.getIndexName());

		verify(mockQueryService, times(1)).defineKeyIndex(eq("MockIndex"), eq("id"), eq("/Example"));
	}

	@Test
	public void createFunctionalIndex() throws Exception {
		Index mockIndex = mock(Index.class, "createFunctionalIndex.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "createFunctionalIndex.MockQueryService");

		when(mockQueryService.createIndex(eq("FunctionalIndex"), eq("purchaseDate"), eq("/Orders")))
			.thenReturn(mockIndex);

		Index actualIndex = indexFactoryBean.createFunctionalIndex(mockQueryService, "FunctionalIndex",
			"purchaseDate", "/Orders", null);

		assertSame(mockIndex, actualIndex);

		verify(mockQueryService, times(1)).createIndex(eq("FunctionalIndex"), eq("purchaseDate"), eq("/Orders"));
	}

	@Test
	public void createFunctionalIndexWithImports() throws Exception {
		Index mockIndex = mock(Index.class, "createFunctionalIndexWithImports.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "createFunctionalIndexWithImports.MockQueryService");

		when(mockQueryService.createIndex(eq("FunctionalIndexWithImports"), eq("purchaseDate"), eq("/Orders"),
			eq("org.example.Order"))).thenReturn(mockIndex);

		Index actualIndex = indexFactoryBean.createFunctionalIndex(mockQueryService, "FunctionalIndexWithImports",
			"purchaseDate", "/Orders", "org.example.Order");

		assertSame(mockIndex, actualIndex);

		verify(mockQueryService, times(1)).createIndex(eq("FunctionalIndexWithImports"), eq("purchaseDate"),
			eq("/Orders"), eq("org.example.Order"));
	}

	@Test
	public void defineFunctionalIndex() throws Exception {
		QueryService mockQueryService = mock(QueryService.class, "defineFunctionalIndex.MockQueryService");

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

		verify(mockQueryService, times(1)).defineIndex(eq("FunctionalIndex"), eq("purchaseDate"), eq("/Orders"));
	}

	@Test
	public void defineFunctionalIndexWithImports() throws Exception {
		QueryService mockQueryService = mock(QueryService.class, "defineFunctionalIndexWithImports.MockQueryService");

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				return null;
			}
		}).when(mockQueryService).defineIndex(eq("FunctionalIndex"), eq("purchaseDate"), eq("/Orders"),
			eq("org.example.Order"));

		indexFactoryBean.setDefine(true);

		Index actualIndex = indexFactoryBean.createFunctionalIndex(mockQueryService, "FunctionalIndex",
			"purchaseDate", "/Orders", "org.example.Order");

		assertTrue(actualIndex instanceof IndexFactoryBean.IndexWrapper);
		assertSame(mockQueryService, ((IndexFactoryBean.IndexWrapper) actualIndex).getQueryService());
		assertEquals("FunctionalIndex", ((IndexFactoryBean.IndexWrapper) actualIndex).getIndexName());

		verify(mockQueryService, times(1)).defineIndex(eq("FunctionalIndex"), eq("purchaseDate"), eq("/Orders"),
			eq("org.example.Order"));
	}

	@Test
	public void createHashIndex() throws Exception {
		Index mockIndex = mock(Index.class, "createHashIndex.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "createHashIndex.MockQueryService");

		when(mockQueryService.createHashIndex(eq("HashIndex"), eq("name"), eq("/People"))).thenReturn(mockIndex);

		Index actualIndex = indexFactoryBean.createHashIndex(mockQueryService, "HashIndex",
			"name", "/People", "  ");

		assertSame(mockIndex, actualIndex);

		verify(mockQueryService, times(1)).createHashIndex(eq("HashIndex"), eq("name"), eq("/People"));
	}

	@Test
	public void createHashIndexWithImports() throws Exception {
		Index mockIndex = mock(Index.class, "createHashIndexWithImports.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "createHashIndexWithImports.MockQueryService");

		when(mockQueryService.createHashIndex(eq("HashIndexWithImports"), eq("name"), eq("/People"),
			eq("org.example.Person"))).thenReturn(mockIndex);

		Index actualIndex = indexFactoryBean.createHashIndex(mockQueryService, "HashIndexWithImports",
			"name", "/People", "org.example.Person");

		assertSame(mockIndex, actualIndex);

		verify(mockQueryService, times(1)).createHashIndex(eq("HashIndexWithImports"), eq("name"), eq("/People"),
			eq("org.example.Person"));
	}

	@Test
	public void defineHashIndex() throws Exception {
		QueryService mockQueryService = mock(QueryService.class, "defineHashIndex.MockQueryService");

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

		verify(mockQueryService, times(1)).defineHashIndex(eq("HashIndex"), eq("name"), eq("/People"));
	}

	@Test
	public void defineHashIndexWithImports() throws Exception {
		QueryService mockQueryService = mock(QueryService.class, "defineHashIndex.MockQueryService");

		doAnswer(new Answer<Void>() {
			@Override public Void answer(final InvocationOnMock invocation) throws Throwable {
				return null;
			}
		}).when(mockQueryService).defineHashIndex(eq("HashIndex"), eq("name"), eq("/People"), eq("org.example.Person"));

		indexFactoryBean.setDefine(true);

		Index actualIndex = indexFactoryBean.createHashIndex(mockQueryService, "HashIndex",
			"name", "/People", "org.example.Person");

		assertTrue(actualIndex instanceof IndexFactoryBean.IndexWrapper);
		assertSame(mockQueryService, ((IndexFactoryBean.IndexWrapper) actualIndex).getQueryService());
		assertEquals("HashIndex", ((IndexFactoryBean.IndexWrapper) actualIndex).getIndexName());

		verify(mockQueryService, times(1)).defineHashIndex(eq("HashIndex"), eq("name"), eq("/People"),
			eq("org.example.Person"));
	}

	@Test
	public void getExistingIndex() {
		Index mockIndexOne = mock(Index.class, "getExistingIndex.MockIndex.One");
		Index mockIndexTwo = mock(Index.class, "getExistingIndex.MockIndex.Two");
		Index mockIndexThree = mock(Index.class, "getExistingIndex.MockIndex.Two");

		QueryService mockQueryService = mock(QueryService.class, "getExistingIndex.MockQueryService");

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
		QueryService mockQueryService = mock(QueryService.class, "getObjectLooksUpIndex.MockQueryService");

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
		when(mockIndex.getType()).thenReturn(com.gemstone.gemfire.cache.query.IndexType.HASH);
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
		assertEquals(com.gemstone.gemfire.cache.query.IndexType.HASH, indexWrapper.getType());

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
