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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.same;
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

import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.client.ClientCache;
import com.gemstone.gemfire.cache.query.Index;
import com.gemstone.gemfire.cache.query.IndexExistsException;
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

	@Test
	public void testAfterPropertiesSet() throws Exception {
		QueryService mockQueryService = mock(QueryService.class, "testAfterPropertiesSet.MockQueryService");

		Cache mockCache = mock(Cache.class, "testAfterPropertiesSet.MockCache");

		when(mockCache.getQueryService()).thenReturn(mockQueryService);

		final Index mockIndex = mock(Index.class, "testAfterPropertiesSet.MockIndex");

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
	public void testAfterPropertiesSetWithNullCache() throws Exception {
		try {
			new IndexFactoryBean().afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("The GemFire Cache reference must not be null!", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAfterPropertiesSetWithNullQueryService() throws Exception {
		try {
			Cache mockCache = mock(Cache.class, "testAfterPropertiesSetWithNullQueryService.MockCache");

			when(mockCache.getQueryService()).thenReturn(null);

			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

			indexFactoryBean.setCache(mockCache);
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("A QueryService is required for Index creation!", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAfterPropertiesSetWithUnspecifiedExpression() throws Exception {
		try {
			QueryService mockQueryService = mock(QueryService.class,
				"testAfterPropertiesSetWithUnspecifiedExpression.MockQueryService");

			Cache mockCache = mock(Cache.class, "testAfterPropertiesSetWithUnspecifiedExpression.MockCache");

			when(mockCache.getQueryService()).thenReturn(mockQueryService);

			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

			indexFactoryBean.setCache(mockCache);
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("The Index 'expression' is required!", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAfterPropertiesSetWithUnspecifiedFromClause() throws Exception {
		try {
			QueryService mockQueryService = mock(QueryService.class,
				"testAfterPropertiesSetWithUnspecifiedFromClause.MockQueryService");

			Cache mockCache = mock(Cache.class, "testAfterPropertiesSetWithUnspecifiedFromClause.MockCache");

			when(mockCache.getQueryService()).thenReturn(mockQueryService);

			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

			indexFactoryBean.setCache(mockCache);
			indexFactoryBean.setExpression("id");
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertEquals("The Index 'from' clause (a Region's full-path) is required!", expected.getMessage());
			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void testAfterPropertiesSetWithUnspecifiedIndexName() throws Exception {
		try {
			QueryService mockQueryService = mock(QueryService.class,
				"testAfterPropertiesSetWithUnspecifiedIndexName.MockQueryService");

			Cache mockCache = mock(Cache.class, "testAfterPropertiesSetWithUnspecifiedIndexName.MockCache");

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
	public void testAfterPropertiesSetWithInvalidTypeUsingImports() throws Exception {
		try {
			QueryService mockQueryService = mock(QueryService.class,
				"testAfterPropertiesSetWithInvalidTypeUsingImports.MockQueryService");

			Cache mockCache = mock(Cache.class, "testAfterPropertiesSetWithInvalidTypeUsingImports.MockCache");

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
	public void testLookupQueryServiceOnClientCache() {
		QueryService mockQueryService = mock(QueryService.class, "testLookupQueryServiceOnClientCache.MockQueryService");

		ClientCache mockClientCache = mock(ClientCache.class, "testLookupQueryServiceOnClientCache.MockClientCache");

		when(mockClientCache.getLocalQueryService()).thenReturn(mockQueryService);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setCache(mockClientCache);

		assertSame(mockQueryService, indexFactoryBean.lookupQueryService());

		verify(mockClientCache, times(1)).getLocalQueryService();
	}

	@Test
	public void testLookupQueryServiceOnPeerCache() {
		QueryService mockQueryService = mock(QueryService.class, "testLookupQueryServiceOnPeerCache.MockQueryService");

		Cache mockCache = mock(Cache.class, "testLookupQueryServiceOnPeerCache.MockCache");

		when(mockCache.getQueryService()).thenReturn(mockQueryService);

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

		indexFactoryBean.setCache(mockCache);
		indexFactoryBean.setQueryService(null);

		assertSame(mockQueryService, indexFactoryBean.lookupQueryService());

		verify(mockCache, times(1)).getQueryService();
	}

	@Test
	public void testCreateIndexReturnsNewKeyIndex() throws Exception {
		Index mockIndex = mock(Index.class, "testCreateIndexReturnsNewKeyIndex.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "testCreateIndexReturnsNewKeyIndex.MockQueryService");

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
	}

	@Test
	public void testCreateIndexReturnsNewHashIndex() throws Exception {
		Index mockIndex = mock(Index.class, "testCreateIndexReturnsNewHashIndex.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "testCreateIndexReturnsNewHashIndex.MockQueryService");

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
	}

	@Test
	public void testCreateIndexReturnsNewFunctionalIndex() throws Exception {
		Index mockIndex = mock(Index.class, "testCreateIndexReturnsNewFunctionalIndex.MockIndex");

		QueryService mockQueryService = mock(QueryService.class,
			"testCreateIndexReturnsNewFunctionalIndex.MockQueryService");

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
	}

	@Test
	public void testCreateIndexReturnsExistingIndex() throws Exception {
		Index mockExistingIndex = mock(Index.class, "testCreateIndexReturnsExistingIndex.MockIndex.Existing");
		Index mockIndexTwo = mock(Index.class, "testCreateIndexReturnsExistingIndex.MockIndex.Two");

		QueryService mockQueryService = mock(QueryService.class, "testCreateIndexReturnsExistingIndex.MockQueryService");

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
	public void testCreateIndexReturnsExistingIndexForIndexExistsException() throws Exception {
		Index mockExistingIndex = mock(Index.class,
			"testCreateIndexReturnsExistingIndexForIndexExistsException.MockIndex.Existing");

		QueryService mockQueryService = mock(QueryService.class,
			"testCreateIndexReturnsExistingIndexForIndexExistsException.MockQueryService");

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
	}

	@Test
	public void testCreateIndexOverridesExistingIndex() throws Exception {
		Index mockExistingIndex = mock(Index.class, "testCreateIndexOverridesExistingIndex.MockIndex.Existing");
		Index mockOverridingIndex = mock(Index.class, "testCreateIndexOverridesExistingIndex.MockIndex.Overriding");

		QueryService mockQueryService = mock(QueryService.class,
			"testCreateIndexOverridesExistingIndex.MockQueryService");

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
	}

	@Test
	public void testCreateIndexAddsExistingIndexOnException() throws Exception {
		final Index mockExistingIndex = mock(Index.class, "testCreateIndexAddsExistingIndexOnException.MockIndex.Existing");
		final Index mockIndexTwo = mock(Index.class, "testCreateIndexAddsExistingIndexOnException.MockIndex.Two");

		final List<Index> indexes = new ArrayList<Index>(1);

		indexes.add(mockIndexTwo);

		QueryService mockQueryService = mock(QueryService.class,
			"testCreateIndexAddsExistingIndexOnException.MockQueryService");

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
	}

	@Test(expected = RuntimeException.class)
	public void testCreateIndexThrowsException() throws Exception {
		Index mockExistingIndex = mock(Index.class, "testCreateIndexThrowsException.MockIndex.Existing");

		QueryService mockQueryService = mock(QueryService.class, "testCreateIndexThrowsException.MockQueryService");

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
		}
	}

	@Test
	public void testCreateFunctionalIndex() throws Exception {
		Index mockIndex = mock(Index.class, "testCreateFunctionalIndex.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "testCreateFunctionalIndex.MockQueryService");

		when(mockQueryService.createIndex(eq("FunctionalIndex"), eq("purchaseDate"), eq("/Orders")))
			.thenReturn(mockIndex);

		Index actualIndex = new IndexFactoryBean().createFunctionalIndex(mockQueryService, "FunctionalIndex",
			"purchaseDate", "/Orders", null);

		assertSame(mockIndex, actualIndex);
	}

	@Test
	public void testCreateFunctionalIndexWithImports() throws Exception {
		Index mockIndex = mock(Index.class, "testCreateFunctionalIndexWithImports.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "testCreateFunctionalIndexWithImports.MockQueryService");

		when(mockQueryService.createIndex(eq("FunctionalIndexWithImports"), eq("purchaseDate"), eq("/Orders"),
			eq("org.example.Order"))).thenReturn(mockIndex);

		Index actualIndex = new IndexFactoryBean().createFunctionalIndex(mockQueryService, "FunctionalIndexWithImports",
			"purchaseDate", "/Orders", "org.example.Order");

		assertSame(mockIndex, actualIndex);
	}

	@Test
	public void testCreateHashIndex() throws Exception {
		Index mockIndex = mock(Index.class, "testCreateHashIndex.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "testCreateHashIndex.MockQueryService");

		when(mockQueryService.createHashIndex(eq("HashIndex"), eq("name"), eq("/People"))).thenReturn(mockIndex);

		Index actualIndex = new IndexFactoryBean().createHashIndex(mockQueryService, "HashIndex",
			"name", "/People", "  ");

		assertSame(mockIndex, actualIndex);
	}

	@Test
	public void testCreateHashIndexWithImports() throws Exception {
		Index mockIndex = mock(Index.class, "testCreateHashIndexWithImports.MockIndex");

		QueryService mockQueryService = mock(QueryService.class, "testCreateHashIndexWithImports.MockQueryService");

		when(mockQueryService.createHashIndex(eq("HashIndexWithImports"), eq("name"), eq("/People"),
			eq("org.example.Person"))).thenReturn(mockIndex);

		Index actualIndex = new IndexFactoryBean().createHashIndex(mockQueryService, "HashIndexWithImports",
			"name", "/People", "org.example.Person");

		assertSame(mockIndex, actualIndex);
	}

	@Test
	public void testGetExistingIndex() {
		Index mockIndexOne = mock(Index.class, "testGetExistingIndex.MockIndex.One");
		Index mockIndexTwo = mock(Index.class, "testGetExistingIndex.MockIndex.Two");
		Index mockIndexThree = mock(Index.class, "testGetExistingIndex.MockIndex.Two");

		QueryService mockQueryService = mock(QueryService.class, "testGetExistingIndex.MockQueryService");

		when(mockIndexOne.getName()).thenReturn("PrimaryIndex");
		when(mockIndexTwo.getName()).thenReturn("SecondaryIndex");
		when(mockIndexThree.getName()).thenReturn("TernaryIndex");
		when(mockQueryService.getIndexes()).thenReturn(Arrays.asList(mockIndexOne, mockIndexTwo, mockIndexThree));

		IndexFactoryBean indexFactoryBean = new IndexFactoryBean();

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

}
