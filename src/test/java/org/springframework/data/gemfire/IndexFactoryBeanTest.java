/*
 * Copyright 2010-2019 the original author or authors.
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

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.commons.logging.Log;
import org.apache.geode.cache.Cache;
import org.apache.geode.cache.client.ClientCache;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.IndexExistsException;
import org.apache.geode.cache.query.IndexNameConflictException;
import org.apache.geode.cache.query.IndexStatistics;
import org.apache.geode.cache.query.QueryService;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.data.gemfire.config.xml.GemfireConstants;

/**
 * Unit tests for {@link IndexFactoryBean}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.apache.geode.cache.Cache
 * @see org.apache.geode.cache.client.ClientCache
 * @see org.apache.geode.cache.query.Index
 * @see org.apache.geode.cache.query.QueryService
 * @see org.springframework.data.gemfire.IndexFactoryBean
 * @since 1.5.2
 */
@RunWith(MockitoJUnitRunner.class)
public class IndexFactoryBeanTest {

	@Mock
	private BeanFactory mockBeanFactory;

	@Mock
	private Cache mockCache;

	@Spy
	private IndexFactoryBean indexFactoryBean;

	@Mock
	private Log mockLog;

	@Mock
	private QueryService mockQueryService;

	@Before
	public void setup() {
		when(mockCache.getQueryService()).thenReturn(mockQueryService);
	}

	@After
	public void tearDown() {
		indexFactoryBean.setBeanFactory(null);
		indexFactoryBean.setCache(null);
		indexFactoryBean.setDefine(false);
		indexFactoryBean.setExpression(null);
		indexFactoryBean.setFrom(null);
		indexFactoryBean.setImports(null);
		indexFactoryBean.setQueryService(null);
		indexFactoryBean.setType((IndexType) null);
	}

	private Index mockIndex(String name) {

		Index mockIndex = mock(Index.class, name);

		when(mockIndex.getName()).thenReturn(name);

		return mockIndex;
	}

	private Index mockIndexWithDefinition(String name, String expression, String fromClause, IndexType type) {

		Index mockIndex = mockIndex(name);

		when(mockIndex.getIndexedExpression()).thenReturn(expression);
		when(mockIndex.getFromClause()).thenReturn(fromClause);
		when(mockIndex.getType()).thenReturn(type.getGemfireIndexType());

		return mockIndex;
	}

	private QueryService mockQueryService(String name) {
		return mock(QueryService.class, name);
	}

	private IndexFactoryBean newIndexFactoryBean() {

		IndexFactoryBean indexFactoryBean = spy(new IndexFactoryBean() {
			@Override
			protected Log newLog() {
				return mockLog;
			}
		});

		indexFactoryBean.setBeanFactory(mockBeanFactory);
		indexFactoryBean.setCache(mockCache);
		indexFactoryBean.setQueryService(mockQueryService);

		return indexFactoryBean;
	}

	@Test
	@SuppressWarnings("all")
	public void afterPropertiesSetIsSuccessful() throws Exception {

		ConfigurableBeanFactory mockConfigurableBeanFactory = mock(ConfigurableBeanFactory.class);

		Index mockIndex = mock(Index.class, "testAfterPropertiesSetIsSuccessful.MockIndex");

		when(mockQueryService.createKeyIndex(eq("TestKeyIndex"), eq("id"), eq("/Example"))).thenReturn(mockIndex);

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setBeanFactory(mockConfigurableBeanFactory);
		indexFactoryBean.setBeanName("KeyIndexBean");
		indexFactoryBean.setDefine(false);
		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setName("TestKeyIndex");
		indexFactoryBean.setType("key");
		indexFactoryBean.afterPropertiesSet();

		assertThat(indexFactoryBean.getIndex()).isEqualTo(mockIndex);

		Index actualIndex = indexFactoryBean.getObject();

		assertThat(actualIndex).isEqualTo(mockIndex);
		assertThat(indexFactoryBean.getObject()).isSameAs(actualIndex);
		assertThat(Index.class).isAssignableFrom(indexFactoryBean.getObjectType());
		assertThat(indexFactoryBean.isSingleton()).isTrue();

		verify(indexFactoryBean, times(1)).getBeanName();
		verify(indexFactoryBean, never()).lookupQueryService();
		verify(mockConfigurableBeanFactory, times(1))
			.registerAlias(eq("KeyIndexBean"), eq("TestKeyIndex"));
		verify(mockQueryService, times(1))
			.createKeyIndex(eq("TestKeyIndex"), eq("id"), eq("/Example"));
		verifyZeroInteractions(mockCache);
	}

	@Test(expected = IllegalArgumentException.class)
	public void afterPropertiesSetWithNoIndexName() throws Exception {
		try {
			newIndexFactoryBean().afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertThat(expected).hasMessage("Index name is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalStateException.class)
	public void afterPropertiesSetWithNullCache() throws Exception {
		try {
			IndexFactoryBean indexFactoryBean = new IndexFactoryBean();
			indexFactoryBean.setName("TestIndex");
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalStateException expected) {
			assertThat(expected).hasMessage("Cache is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalStateException.class)
	public void afterPropertiesSetWithNullQueryService() throws Exception {

		IndexFactoryBean indexFactoryBean = spy(new IndexFactoryBean());

		doReturn(null).when(indexFactoryBean).lookupQueryService();

		try {
			indexFactoryBean.setCache(mockCache);
			indexFactoryBean.setName("TestIndex");
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalStateException expected) {
			assertThat(expected).hasMessage("QueryService is required to create an Index");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			verify(indexFactoryBean, times(1)).lookupQueryService();
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
			assertThat(expected).hasMessage("Index expression is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void afterPropertiesSetWithUnspecifiedFromClause() throws Exception {
		try {
			IndexFactoryBean indexFactoryBean = newIndexFactoryBean();
			indexFactoryBean.setName("TestIndex");
			indexFactoryBean.setExpression("id");
			indexFactoryBean.afterPropertiesSet();
		}
		catch (Exception expected) {
			assertThat(expected).hasMessage("Index from clause is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void afterPropertiesSetUsingImportsWithInvalidIndexKeyType() throws Exception {
		try {
			IndexFactoryBean indexFactoryBean = newIndexFactoryBean();
			indexFactoryBean.setName("TestIndex");
			indexFactoryBean.setExpression("id");
			indexFactoryBean.setFrom("/Example");
			indexFactoryBean.setImports("org.example.DomainType");
			indexFactoryBean.setType("PriMary_Key");
			indexFactoryBean.afterPropertiesSet();
		}
		catch (IllegalArgumentException expected) {
			assertThat(expected).hasMessage("Imports are not supported with a KEY Index");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void resolveCacheFromCacheProperty() {
		assertThat(newIndexFactoryBean().resolveCache()).isSameAs(mockCache);
	}

	@Test(expected = IllegalStateException.class)
	public void resolveCacheThrowsExceptionForUnresolvableCache() {
		try {
			IndexFactoryBean indexFactoryBean = newIndexFactoryBean();
			indexFactoryBean.setCache(null);
			indexFactoryBean.resolveCache();
		}
		catch (IllegalStateException expected) {
			assertThat(expected).hasMessage("Cache is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void resolveIndexNameFromBeanNameProperty() {

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setBeanName("TestIndexBeanName");
		indexFactoryBean.setName(null);

		assertThat(indexFactoryBean.resolveIndexName()).isEqualTo("TestIndexBeanName");
	}

	@Test
	public void resolveIndexNameFromNameProperty() {

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setBeanName("TestIndexBeanName");
		indexFactoryBean.setName("TestIndex");

		assertThat(indexFactoryBean.resolveIndexName()).isEqualTo("TestIndex");
	}

	@Test(expected = IllegalArgumentException.class)
	public void resolveIndexNameThrowsExceptionForUnresolvableIndexName() {
		try {
			IndexFactoryBean indexFactoryBean = newIndexFactoryBean();
			indexFactoryBean.setBeanName(null);
			indexFactoryBean.setName(null);
			indexFactoryBean.resolveIndexName();
		}
		catch (IllegalArgumentException expected) {
			assertThat(expected).hasMessage("Index name is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void doLookupQueryService() {

		QueryService mockQueryServiceOne = mockQueryService("testDoLookupQueryService.MockQueryService.One");

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setQueryService(mockQueryServiceOne);

		assertThat(indexFactoryBean.doLookupQueryService()).isSameAs(mockQueryServiceOne);

		verify(mockCache, never()).getQueryService();
	}

	@Test
	public void doLookupQueryServiceOnClientCache() {

		ClientCache mockClientCache = mock(ClientCache.class);
		QueryService mockQueryService = mockQueryService("testDoLookupQueryServiceOnClientCache.MockQueryService");

		when(mockClientCache.getLocalQueryService()).thenReturn(mockQueryService);

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setCache(mockClientCache);
		indexFactoryBean.setQueryService(null);

		assertThat(indexFactoryBean.doLookupQueryService()).isSameAs(mockQueryService);

		verify(mockClientCache, times(1)).getLocalQueryService();
		verify(mockClientCache, never()).getQueryService();
	}

	@Test
	public void doLookupQueryServiceOnPeerCache() {

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setCache(mockCache);
		indexFactoryBean.setQueryService(null);

		assertThat(indexFactoryBean.doLookupQueryService()).isSameAs(mockQueryService);

		verify(mockCache, times(1)).getQueryService();
	}

	@Test
	public void lookupQueryServiceFromBeanFactory() {

		QueryService mockQueryService =
			mockQueryService("testLookupQueryServiceFromBeanFactory.MockQueryService");

		when(mockBeanFactory.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE)))
			.thenReturn(true);
		when(mockBeanFactory.getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE),
			eq(QueryService.class))).thenReturn(mockQueryService);

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		assertThat(indexFactoryBean.lookupQueryService()).isSameAs(mockQueryService);

		verify(mockBeanFactory, times(1)).containsBean(
			eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE));
		verify(mockBeanFactory, times(1)).getBean(
			eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE), eq(QueryService.class));
		verify(mockCache, never()).getQueryService();
	}

	@Test
	public void lookupQueryServiceFromCache() {

		QueryService mockQueryService = mockQueryService("testLookupQueryServiceFromCache.MockQueryService");

		when(mockBeanFactory.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE)))
			.thenReturn(false);
		when(mockCache.getQueryService()).thenReturn(mockQueryService);

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setDefine(false);
		indexFactoryBean.setQueryService(null);

		assertThat(indexFactoryBean.lookupQueryService()).isSameAs(mockQueryService);

		verify(mockBeanFactory, times(1)).containsBean(
			eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE));
		verify(mockBeanFactory, never()).getBean(
			eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE), eq(QueryService.class));
		verify(mockCache, times(1)).getQueryService();
	}

	@Test
	public void resolveQueryServiceReturnsIndexFactoryBeanConfiguredQueryService() {
		assertThat(newIndexFactoryBean().resolveQueryService()).isSameAs(mockQueryService);
	}

	@Test
	public void resolveQueryServiceReturnsQueryServiceFromLookup() {

		QueryService mockQueryService =
			mockQueryService("testResolveQueryServiceReturnsQueryServiceFromLookup.MockQueryService");

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setQueryService(null);

		doReturn(mockQueryService).when(indexFactoryBean).lookupQueryService();

		assertThat(indexFactoryBean.resolveQueryService()).isSameAs(mockQueryService);

		verify(indexFactoryBean, times(1)).lookupQueryService();
	}

	@Test(expected = IllegalStateException.class)
	public void resolveQueryServiceThrowsExceptionForUnresolvableQueryService() {

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		try {
			indexFactoryBean.setQueryService(null);
			doReturn(null).when(indexFactoryBean).lookupQueryService();
			indexFactoryBean.resolveQueryService();
		}
		catch (IllegalStateException expected) {
			assertThat(expected).hasMessage("QueryService is required to create an Index");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			verify(indexFactoryBean, times(1)).lookupQueryService();
		}
	}

	@Test
	public void registerQueryServiceBeanWhenIndexIsCreated() {

		ConfigurableBeanFactory mockBeanFactory = mock(ConfigurableBeanFactory.class,
			"testRegisterQueryServiceBeanWhenIndexIsCreated.MockBeanFactory");

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setBeanFactory(mockBeanFactory);
		indexFactoryBean.setDefine(false);

		assertThat(indexFactoryBean.registerQueryServiceBean("queryServiceBeanName", mockQueryService))
			.isSameAs(mockQueryService);

		verify(mockBeanFactory, never()).registerSingleton(eq("queryServiceBeanName"), same(mockQueryService));
	}

	@Test
	public void registerQueryServiceBeanWhenIndexIsDefined() {

		ConfigurableBeanFactory mockBeanFactory = mock(ConfigurableBeanFactory.class,
			"testRegisterQueryServiceBeanWhenIndexIsDefined.MockBeanFactory");

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setBeanFactory(mockBeanFactory);
		indexFactoryBean.setDefine(true);

		assertThat(indexFactoryBean.registerQueryServiceBean("queryServiceBeanName", mockQueryService))
			.isSameAs(mockQueryService);

		verify(mockBeanFactory, times(1))
			.registerSingleton(eq("queryServiceBeanName"), same(mockQueryService));
	}

	@Test
	public void registerAliasWhenBeanFactoryIsNull() {

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setBeanFactory(null);

		indexFactoryBean.registerAlias("IndexBean", "TestIndex");
	}

	@Test
	public void registerAliasWhenBeanFactoryIsNotConfigurable() {

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.registerAlias("IndexBean", "TestIndex");

		verifyZeroInteractions(mockBeanFactory);
	}

	@Test
	public void registerAliasWhenBeanNameAndIndexNameMatch() {

		ConfigurableBeanFactory mockConfigurableBeanFactory = mock(ConfigurableBeanFactory.class);

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setBeanFactory(mockConfigurableBeanFactory);
		indexFactoryBean.registerAlias("TestIndex", "TestIndex");

		assertThat(indexFactoryBean.getBeanFactory()).isSameAs(mockConfigurableBeanFactory);

		verifyZeroInteractions(mockConfigurableBeanFactory);
	}

	@Test
	public void registerAliasWhenBeanNameAndIndexNameAreDifferent() {

		ConfigurableBeanFactory mockConfigurableBeanFactory = mock(ConfigurableBeanFactory.class);

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setBeanFactory(mockConfigurableBeanFactory);
		indexFactoryBean.registerAlias("IndexBean", "TestIndex");

		assertThat(indexFactoryBean.getBeanFactory()).isSameAs(mockConfigurableBeanFactory);

		verify(mockConfigurableBeanFactory, times(1))
			.registerAlias(eq("IndexBean"), eq("TestIndex"));
	}

	@Test
	public void createIndexReturnsNewKeyIndex() throws Exception {

		Index mockIndex = mockIndex("testCreateIndexReturnsNewKeyIndex.MockIndex");

		when(mockQueryService.createKeyIndex(eq("KeyIndex"), eq("id"), eq("/Example")))
			.thenReturn(mockIndex);

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setName("KeyIndex");
		indexFactoryBean.setType("key");

		Index actualIndex = indexFactoryBean.createIndex(mockQueryService, "KeyIndex");

		assertThat(actualIndex).isSameAs(mockIndex);

		verify(mockQueryService, times(1))
			.createKeyIndex(eq("KeyIndex"), eq("id"), eq("/Example"));
	}

	@Test
	public void createIndexReturnsNewHashIndex() throws Exception {

		Index mockIndex = mockIndex("testCreateIndexReturnsNewHashIndex.MockIndex");

		when(mockQueryService.createHashIndex(eq("HashIndex"), eq("name"), eq("/Animals"), eq("org.example.Dog")))
			.thenReturn(mockIndex);

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setExpression("name");
		indexFactoryBean.setFrom("/Animals");
		indexFactoryBean.setImports("org.example.Dog");
		indexFactoryBean.setName("HashIndex");
		indexFactoryBean.setType("HasH");

		Index actualIndex = indexFactoryBean.createIndex(mockQueryService, "HashIndex");

		assertThat(actualIndex).isSameAs(mockIndex);

		verify(mockQueryService, times(1))
			.createHashIndex(eq("HashIndex"), eq("name"), eq("/Animals"), eq("org.example.Dog"));
	}

	@Test
	public void createIndexReturnsNewFunctionalIndex() throws Exception {

		Index mockIndex = mockIndex("testCreateIndexReturnsNewFunctionalIndex.MockIndex");

		when(mockQueryService.createIndex(eq("FunctionalIndex"), eq("someField"), eq("/Example")))
			.thenReturn(mockIndex);

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setExpression("someField");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setImports("  ");
		indexFactoryBean.setName("FunctionalIndex");
		indexFactoryBean.setType((String) null);

		Index actualIndex = indexFactoryBean.createIndex(mockQueryService, "FunctionalIndex");

		assertThat(actualIndex).isSameAs(mockIndex);

		verify(mockQueryService, times(1))
			.createIndex(eq("FunctionalIndex"), eq("someField"), eq("/Example"));
	}

	@Test(expected = GemfireIndexException.class)
	public void createIndexThrowsIndexExistsException() throws Exception {

		Index mockIndex =
			mockIndexWithDefinition("MockIndex", "id", "/Example", IndexType.PRIMARY_KEY);

		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockIndex));

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		doThrow(new IndexExistsException("TEST")).when(indexFactoryBean)
			.createKeyIndex(any(QueryService.class), anyString(), anyString(), anyString());

		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setName("TestIndex");
		indexFactoryBean.setType(IndexType.PRIMARY_KEY);

		assertThat(indexFactoryBean.isIgnoreIfExists()).isFalse();
		assertThat(indexFactoryBean.isOverride()).isFalse();

		try {
			indexFactoryBean.createIndex(mockQueryService, "TestIndex");
		}
		catch (GemfireIndexException expected) {

			assertThat(expected).hasMessageStartingWith(String.format(
				"An Index with a different name [MockIndex] having the same definition [%s] already exists;"
					+ " You may attempt to override the existing Index [MockIndex] with the new name [TestIndex]"
					+ " by setting the 'override' property to 'true'", indexFactoryBean.toBasicIndexDefinition()));

			assertThat(expected).hasCauseInstanceOf(IndexExistsException.class);
			assertThat(expected.getCause()).hasMessage("TEST");
			assertThat(expected.getCause()).hasNoCause();

			throw expected;
		}
		finally {
			verify(mockQueryService, times(1)).getIndexes();

			verify(indexFactoryBean, times(1))
				.createKeyIndex(eq(mockQueryService), eq("TestIndex"), eq("id"), eq("/Example"));
		}
	}

	@Test(expected = GemfireIndexException.class)
	public void createIndexThrowsIndexExistsExceptionAndCannotFindExistingIndexByDefinition() throws Exception {

		when(mockQueryService.getIndexes()).thenReturn(Collections.emptyList());

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		doThrow(new IndexExistsException("TEST")).when(indexFactoryBean)
			.createKeyIndex(any(QueryService.class), anyString(), anyString(), anyString());

		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setIgnoreIfExists(true);
		indexFactoryBean.setName("TestIndex");
		indexFactoryBean.setOverride(true);
		indexFactoryBean.setType(IndexType.KEY);

		assertThat(indexFactoryBean.isIgnoreIfExists()).isTrue();
		assertThat(indexFactoryBean.isOverride()).isTrue();

		try {
			indexFactoryBean.createIndex(mockQueryService, "TestIndex");
		}
		catch (GemfireIndexException expected) {

			assertThat(expected).hasMessageStartingWith(String.format(
				"An Index with a different name [unknown] having the same definition [%s] already exists;"
					+ " You may attempt to override the existing Index [unknown] with the new name [TestIndex]"
					+ " by setting the 'override' property to 'true'", indexFactoryBean.toBasicIndexDefinition()));

			assertThat(expected).hasCauseInstanceOf(IndexExistsException.class);
			assertThat(expected.getCause()).hasMessage("TEST");
			assertThat(expected.getCause()).hasNoCause();

			throw expected;
		}
		finally {
			verify(mockQueryService, times(1)).getIndexes();

			verify(indexFactoryBean, times(1))
				.createKeyIndex(eq(mockQueryService), eq("TestIndex"), eq("id"), eq("/Example"));
		}
	}

	@Test
	public void createIndexThrowsIndexExistsExceptionAndIgnoresThisIndex() throws Exception {

		Index mockIndex =
			mockIndexWithDefinition("MockIndex", "id", "/Example", IndexType.PRIMARY_KEY);

		when(mockLog.isWarnEnabled()).thenReturn(true);

		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockIndex));

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		doThrow(new IndexExistsException("TEST")).when(indexFactoryBean)
			.createKeyIndex(any(QueryService.class), anyString(), anyString(), anyString());

		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setIgnoreIfExists(true);
		indexFactoryBean.setName("TestIndex");
		indexFactoryBean.setOverride(true);
		indexFactoryBean.setType(IndexType.PRIMARY_KEY);

		assertThat(indexFactoryBean.isIgnoreIfExists()).isTrue();
		assertThat(indexFactoryBean.isOverride()).isTrue();
		assertThat(indexFactoryBean.createIndex(mockQueryService, "TestIndex")).isEqualTo(mockIndex);

		verify(indexFactoryBean, times(1))
			.createKeyIndex(eq(mockQueryService), eq("TestIndex"), eq("id"), eq("/Example"));

		verify(mockLog, times(1)).warn(
			eq(String.format("WARNING! You are choosing to ignore this Index [TestIndex] and return the existing Index"
					+ " having the same basic definition [%s] but with a different name [MockIndex];"
					+ " Make sure no OQL Query Hints refer to this Index by name [TestIndex]",
				indexFactoryBean.toBasicIndexDefinition())));

		verify(mockQueryService, times(1)).getIndexes();
	}

	@Test
	public void createIndexThrowsIndexExistsExceptionAndOverridesExistingIndex() throws Exception {

		Index mockIndex =
			mockIndexWithDefinition("MockIndex", "id", "/Example", IndexType.PRIMARY_KEY);

		Index testIndex =
			mockIndexWithDefinition("TestIndex", "id", "/Example", IndexType.PRIMARY_KEY);

		when(mockLog.isWarnEnabled()).thenReturn(true);

		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockIndex));

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		doThrow(new IndexExistsException("TEST")).doReturn(testIndex).when(indexFactoryBean)
			.createKeyIndex(any(QueryService.class), anyString(), anyString(), anyString());

		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setIgnoreIfExists(false);
		indexFactoryBean.setName("TestIndex");
		indexFactoryBean.setOverride(true);
		indexFactoryBean.setType(IndexType.PRIMARY_KEY);

		assertThat(indexFactoryBean.isIgnoreIfExists()).isFalse();
		assertThat(indexFactoryBean.isOverride()).isTrue();
		assertThat(indexFactoryBean.createIndex(mockQueryService, "TestIndex")).isEqualTo(testIndex);

		verify(indexFactoryBean, times(2))
			.createKeyIndex(eq(mockQueryService), eq("TestIndex"), eq("id"), eq("/Example"));

		verify(mockLog, times(1)).warn(
			eq(String.format("WARNING! You are attempting to 'override' an existing Index [MockIndex]"
					+ " having the same basic definition [%s] as the Index that will be created by this"
					+ " IndexFactoryBean [TestIndex]; 'Override' effectively 'renames' the existing Index [MockIndex]"
					+ " by removing it then recreating it under the new name [TestIndex] with the same definition;"
					+ " You should be careful to update any existing OQL Query Hints referring to the old"
					+ " Index name [MockIndex] to now use the new name [TestIndex]",
				indexFactoryBean.toBasicIndexDefinition())));

		verify(mockQueryService, times(1)).getIndexes();
	}

	@Test(expected = GemfireIndexException.class)
	public void createIndexThrowsIndexExistsExceptionAndOverrideThrowsRuntimeException() throws Exception {

		Index mockIndex =
			mockIndexWithDefinition("MockIndex", "id", "/Example", IndexType.PRIMARY_KEY);

		when(mockLog.isWarnEnabled()).thenReturn(true);

		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockIndex))
			.thenReturn(Collections.emptyList());

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		doThrow(new IndexExistsException("TEST")).doThrow(new RuntimeException("RETRY")).when(indexFactoryBean)
			.createKeyIndex(any(QueryService.class), anyString(), anyString(), anyString());

		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setIgnoreIfExists(false);
		indexFactoryBean.setName("TestIndex");
		indexFactoryBean.setOverride(true);
		indexFactoryBean.setType(IndexType.PRIMARY_KEY);

		assertThat(indexFactoryBean.isIgnoreIfExists()).isFalse();
		assertThat(indexFactoryBean.isOverride()).isTrue();

		try {
			indexFactoryBean.createIndex(mockQueryService, "TestIndex");
		}
		catch (GemfireIndexException expected) {

			assertThat(expected).hasMessageStartingWith(
				"Attempt to 'override' existing Index [MockIndex] with the Index that would be created by this"
					+ " IndexFactoryBean [TestIndex] failed; you should verify the state of your system"
					+ " and make sure the previously existing Index [MockIndex] still exits");

			assertThat(expected).hasCauseInstanceOf(GemfireIndexException.class);
			assertThat(expected.getCause()).hasMessageStartingWith(
				String.format("Failed to create Index [%s]", indexFactoryBean.toDetailedIndexDefinition()));
			assertThat(expected.getCause()).hasCauseInstanceOf(RuntimeException.class);
			assertThat(expected.getCause().getCause()).hasMessageStartingWith("RETRY");
			assertThat(expected.getCause().getCause()).hasNoCause();

			throw expected;
		}
		finally {

			verify(indexFactoryBean, times(2))
				.createKeyIndex(eq(mockQueryService), eq("TestIndex"), eq("id"), eq("/Example"));

			verify(mockLog, times(1)).warn(
				eq(String.format("WARNING! You are attempting to 'override' an existing Index [MockIndex]"
						+ " having the same basic definition [%s] as the Index that will be created by this"
						+ " IndexFactoryBean [TestIndex]; 'Override' effectively 'renames' the existing Index [MockIndex]"
						+ " by removing it then recreating it under the new name [TestIndex] with the same definition;"
						+ " You should be careful to update any existing OQL Query Hints referring to the old"
						+ " Index name [MockIndex] to now use the new name [TestIndex]",
					indexFactoryBean.toBasicIndexDefinition())));

			verify(mockQueryService, times(1)).getIndexes();
		}
	}

	@Test(expected = GemfireIndexException.class)
	public void createIndexThrowsIndexNameConflictException() throws Exception {

		Index mockIndex =
			mockIndexWithDefinition("TestIndex", "purchaseDate", "/Orders", IndexType.HASH);

		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockIndex));

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		doThrow(new IndexNameConflictException("TEST")).when(indexFactoryBean)
			.createFunctionalIndex(eq(mockQueryService), anyString(), anyString(), anyString(), any());

		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setName("TestIndex");
		indexFactoryBean.setType(IndexType.FUNCTIONAL);

		assertThat(indexFactoryBean.isIgnoreIfExists()).isFalse();
		assertThat(indexFactoryBean.isOverride()).isFalse();

		try {
			indexFactoryBean.createIndex(mockQueryService, "TestIndex");
		}
		catch (GemfireIndexException expected) {

			String existingIndexDefinition = String.format(IndexFactoryBean.DETAILED_INDEX_DEFINITION,
				mockIndex.getName(), mockIndex.getIndexedExpression(), mockIndex.getFromClause(), "unknown",
					IndexType.valueOf(mockIndex.getType()));

			assertThat(expected).hasMessageStartingWith(String.format(
				"An Index with the same name [TestIndex] having possibly a different definition already exists;"
					+ " you may choose to ignore this Index definition [%1$s] and use the existing Index"
					+ " definition [%2$s] by setting the 'ignoreIfExists' property to 'true'",
				indexFactoryBean.toDetailedIndexDefinition(), existingIndexDefinition));

			assertThat(expected).hasCauseInstanceOf(IndexNameConflictException.class);
			assertThat(expected.getCause()).hasMessage("TEST");
			assertThat(expected.getCause()).hasNoCause();

			throw expected;
		}
		finally {
			verify(indexFactoryBean, times(1))
				.createFunctionalIndex(eq(mockQueryService), eq("TestIndex"),
					eq("id"), eq("/Example"), eq(null));

			verifyZeroInteractions(mockLog);

			verify(mockQueryService, times(1)).getIndexes();
		}
	}

	@Test(expected = GemfireIndexException.class)
	public void createIndexThrowsIndexNameConflictExceptionAndCannotFindExistingIndexByName() throws Exception {

		when(mockQueryService.getIndexes()).thenReturn(Collections.emptyList());

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		doThrow(new IndexNameConflictException("TEST")).when(indexFactoryBean)
			.createFunctionalIndex(eq(mockQueryService), anyString(), anyString(), anyString(), any());

		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setIgnoreIfExists(true);
		indexFactoryBean.setName("TestIndex");
		indexFactoryBean.setOverride(true);
		indexFactoryBean.setType(IndexType.FUNCTIONAL);

		assertThat(indexFactoryBean.isIgnoreIfExists()).isTrue();
		assertThat(indexFactoryBean.isOverride()).isTrue();

		try {
			indexFactoryBean.createIndex(mockQueryService, "TestIndex");
		}
		catch (GemfireIndexException expected) {

			assertThat(expected).hasMessageStartingWith(String.format(
				"An Index with the same name [TestIndex] having possibly a different definition already exists;"
					+ " you may choose to ignore this Index definition [%s] and use the existing Index"
					+ " definition [unknown] by setting the 'ignoreIfExists' property to 'true'",
				indexFactoryBean.toDetailedIndexDefinition()));

			assertThat(expected).hasCauseInstanceOf(IndexNameConflictException.class);
			assertThat(expected.getCause()).hasMessage("TEST");
			assertThat(expected.getCause()).hasNoCause();

			throw expected;
		}
		finally {
			verify(indexFactoryBean, times(1))
				.createFunctionalIndex(eq(mockQueryService), eq("TestIndex"),
					eq("id"), eq("/Example"), eq(null));

			verifyZeroInteractions(mockLog);

			verify(mockQueryService, times(1)).getIndexes();
		}
	}

	@Test
	public void createIndexThrowsIndexNameConflictExceptionAndIgnoresThisIndex() throws Exception {

		Index mockIndex =
			mockIndexWithDefinition("TestIndex", "price", "/Orders", IndexType.FUNCTIONAL);

		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockIndex));

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		doThrow(new IndexNameConflictException("TEST")).when(indexFactoryBean)
			.createFunctionalIndex(eq(mockQueryService), anyString(), anyString(), anyString(), any());

		indexFactoryBean.setExpression("price");
		indexFactoryBean.setFrom("/Orders");
		indexFactoryBean.setIgnoreIfExists(true);
		indexFactoryBean.setName("TestIndex");
		indexFactoryBean.setOverride(true);
		indexFactoryBean.setType(IndexType.FUNCTIONAL);

		assertThat(indexFactoryBean.isIgnoreIfExists()).isTrue();
		assertThat(indexFactoryBean.isOverride()).isTrue();
		assertThat(indexFactoryBean.createIndex(mockQueryService, "TestIndex")).isEqualTo(mockIndex);

		verify(indexFactoryBean, times(1))
			.createFunctionalIndex(eq(mockQueryService), eq("TestIndex"), eq("price"),
				eq("/Orders"), eq(null));

		verifyZeroInteractions(mockLog);

		verify(mockQueryService, times(1)).getIndexes();
	}

	@Test
	public void createIndexThrowsIndexNameConflictExceptionAndIgnoresThisIndexWithWarning() throws Exception {

		Index mockIndex =
			mockIndexWithDefinition("TestIndex", "id", "/Orders", IndexType.PRIMARY_KEY);

		when(mockLog.isWarnEnabled()).thenReturn(true);

		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockIndex));

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		doThrow(new IndexNameConflictException("TEST")).when(indexFactoryBean)
			.createFunctionalIndex(eq(mockQueryService), anyString(), anyString(), anyString(), any());

		indexFactoryBean.setExpression("price");
		indexFactoryBean.setFrom("/Orders");
		indexFactoryBean.setIgnoreIfExists(true);
		indexFactoryBean.setName("TestIndex");
		indexFactoryBean.setOverride(true);
		indexFactoryBean.setType(IndexType.FUNCTIONAL);

		assertThat(indexFactoryBean.isIgnoreIfExists()).isTrue();
		assertThat(indexFactoryBean.isOverride()).isTrue();
		assertThat(indexFactoryBean.createIndex(mockQueryService, "TestIndex")).isEqualTo(mockIndex);

		String existingIndexDefinition = String.format(IndexFactoryBean.BASIC_INDEX_DEFINITION,
			"id", "/Orders", IndexType.PRIMARY_KEY);

		verify(indexFactoryBean, times(1))
			.createFunctionalIndex(eq(mockQueryService), eq("TestIndex"), eq("price"),
				eq("/Orders"), eq(null));

		verify(mockLog, times(1)).warn(String.format(
			"WARNING! Returning existing Index [TestIndex] having a definition [%1$s] that does not match"
				+ " the Index defined [%2$s] by this IndexFactoryBean [TestIndex]",
			existingIndexDefinition, indexFactoryBean.toBasicIndexDefinition()));

		verify(mockQueryService, times(1)).getIndexes();
	}

	@Test
	public void createIndexThrowsIndexNameConflictExceptionAndOverridesExistingIndex() throws Exception {

		Index mockIndex =
			mockIndexWithDefinition("TestIndex", "id", "/Example", IndexType.PRIMARY_KEY);

		Index testIndex =
			mockIndexWithDefinition("TestIndex", "id", "/Example", IndexType.PRIMARY_KEY);

		assertThat(mockIndex).isNotSameAs(testIndex);

		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockIndex));

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		doThrow(new IndexNameConflictException("TEST")).doReturn(testIndex).when(indexFactoryBean)
			.createKeyIndex(any(QueryService.class), anyString(), anyString(), anyString());

		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setIgnoreIfExists(false);
		indexFactoryBean.setName("TestIndex");
		indexFactoryBean.setOverride(true);
		indexFactoryBean.setType(IndexType.PRIMARY_KEY);

		assertThat(indexFactoryBean.isIgnoreIfExists()).isFalse();
		assertThat(indexFactoryBean.isOverride()).isTrue();

		// NOTE: this test case is also testing the smart "override" handling logic, which just
		// returns the existing Index if both the name and definition are the same on when an
		// IndexNameConflictException is thrown.
		assertThat(indexFactoryBean.createIndex(mockQueryService, "TestIndex")).isEqualTo(mockIndex);

		verify(indexFactoryBean, times(1))
			.createKeyIndex(eq(mockQueryService), eq("TestIndex"), eq("id"), eq("/Example"));

		verifyZeroInteractions(mockLog);

		verify(mockQueryService, times(1)).getIndexes();
	}

	@Test
	public void createIndexThrowsIndexNameConflictExceptionAndOverridesExistingIndexWithWarning() throws Exception {

		Index mockIndex =
			mockIndexWithDefinition("TestIndex", "price", "/Orders", IndexType.FUNCTIONAL);

		Index testIndex =
			mockIndexWithDefinition("TestIndex", "purchaseDate", "/Orders", IndexType.HASH);

		assertThat(mockIndex).isNotSameAs(testIndex);

		when(mockLog.isWarnEnabled()).thenReturn(true);

		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockIndex));

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		doThrow(new IndexNameConflictException("TEST")).doReturn(testIndex).when(indexFactoryBean)
			.createHashIndex(any(QueryService.class), anyString(), anyString(), anyString(), any());

		indexFactoryBean.setExpression("purchaseDate");
		indexFactoryBean.setFrom("/Orders");
		indexFactoryBean.setIgnoreIfExists(false);
		indexFactoryBean.setName("TestIndex");
		indexFactoryBean.setOverride(true);
		indexFactoryBean.setType(IndexType.HASH);

		assertThat(indexFactoryBean.isIgnoreIfExists()).isFalse();
		assertThat(indexFactoryBean.isOverride()).isTrue();
		assertThat(indexFactoryBean.createIndex(mockQueryService, "TestIndex")).isEqualTo(testIndex);

		String existingIndexDefinition = String.format(IndexFactoryBean.BASIC_INDEX_DEFINITION,
			mockIndex.getIndexedExpression(), mockIndex.getFromClause(), IndexType.valueOf(mockIndex.getType()));

		verify(indexFactoryBean, times(2)).createHashIndex(eq(mockQueryService),
			eq("TestIndex"), eq("purchaseDate"), eq("/Orders"), eq(null));

		verify(mockLog, times(1)).warn(eq(String.format(
			"WARNING! Overriding existing Index [TestIndex] having a definition [%1$s] that does not match"
				+ " the Index defined [%2$s] by this IndexFactoryBean [TestIndex]",
			existingIndexDefinition, indexFactoryBean.toBasicIndexDefinition())));

		verify(mockQueryService, times(1)).getIndexes();
	}

	@Test(expected = GemfireIndexException.class)
	public void createIndexThrowsIndexNameConflictExceptionAndOverrideThrowsRuntimeException() throws Exception {

		Index mockIndex =
			mockIndexWithDefinition("MockIndex", "purchaseDate", "/Example",
				IndexType.HASH);

		when(mockLog.isWarnEnabled()).thenReturn(true);

		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockIndex))
			.thenReturn(Collections.emptyList());

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		doThrow(new IndexNameConflictException("TEST")).doThrow(new RuntimeException("RETRY")).when(indexFactoryBean)
			.createKeyIndex(any(QueryService.class), anyString(), anyString(), anyString());

		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Example");
		indexFactoryBean.setIgnoreIfExists(false);
		indexFactoryBean.setName("MockIndex");
		indexFactoryBean.setOverride(true);
		indexFactoryBean.setType(IndexType.PRIMARY_KEY);

		assertThat(indexFactoryBean.isIgnoreIfExists()).isFalse();
		assertThat(indexFactoryBean.isOverride()).isTrue();

		try {
			indexFactoryBean.createIndex(mockQueryService, "MockIndex");
		}
		catch (GemfireIndexException expected) {

			assertThat(expected).hasMessageStartingWith(
				"Attempt to 'override' existing Index [MockIndex] with the Index that would be created by this"
					+ " IndexFactoryBean [MockIndex] failed; you should verify the state of your system"
					+ " and make sure the previously existing Index [MockIndex] still exits");

			assertThat(expected).hasCauseInstanceOf(GemfireIndexException.class);
			assertThat(expected.getCause()).hasMessageStartingWith(
				String.format("Failed to create Index [%s]", indexFactoryBean.toDetailedIndexDefinition()));
			assertThat(expected.getCause()).hasCauseInstanceOf(RuntimeException.class);
			assertThat(expected.getCause().getCause()).hasMessageStartingWith("RETRY");
			assertThat(expected.getCause().getCause()).hasNoCause();

			throw expected;
		}
		finally {

			String existingIndexDefinition = String.format(IndexFactoryBean.BASIC_INDEX_DEFINITION,
				mockIndex.getIndexedExpression(), mockIndex.getFromClause(), IndexType.valueOf(mockIndex.getType()));

			verify(indexFactoryBean, times(2))
				.createKeyIndex(eq(mockQueryService), eq("MockIndex"), eq("id"), eq("/Example"));

			verify(mockLog, times(1)).warn(eq(String.format(
				"WARNING! Overriding existing Index [MockIndex] having a definition [%1$s] that does not match"
					+ " the Index defined [%2$s] by this IndexFactoryBean [MockIndex]",
				existingIndexDefinition, indexFactoryBean.toBasicIndexDefinition())));

			verify(mockQueryService, times(1)).getIndexes();
		}
	}

	@Test
	public void createFunctionalIndex() throws Exception {

		Index mockIndex = mockIndex("testCreateFunctionalIndex.MockIndex");

		when(mockQueryService.createIndex(eq("FunctionalIndex"), eq("purchaseDate"), eq("/Orders")))
			.thenReturn(mockIndex);

		Index actualIndex = indexFactoryBean.createFunctionalIndex(mockQueryService, "FunctionalIndex",
			"purchaseDate", "/Orders", null);

		assertThat(actualIndex).isSameAs(mockIndex);

		verify(mockQueryService, times(1))
			.createIndex(eq("FunctionalIndex"), eq("purchaseDate"), eq("/Orders"));
		verify(mockQueryService, never()).defineIndex(anyString(), anyString(), anyString());
	}

	@Test
	public void createFunctionalIndexWithImports() throws Exception {

		Index mockIndex = mockIndex("testCreateFunctionalIndexWithImports.MockIndex");

		when(mockQueryService.createIndex(eq("FunctionalIndexWithImports"), eq("purchaseDate"),
			eq("/Orders"), eq("example.Order"))).thenReturn(mockIndex);

		Index actualIndex = indexFactoryBean.createFunctionalIndex(mockQueryService, "FunctionalIndexWithImports",
			"purchaseDate", "/Orders", "example.Order");

		assertThat(actualIndex).isSameAs(mockIndex);

		verify(mockQueryService, times(1))
			.createIndex(eq("FunctionalIndexWithImports"), eq("purchaseDate"), eq("/Orders"),
				eq("example.Order"));

		verify(mockQueryService, never()).defineIndex(anyString(), anyString(), anyString(), anyString());
	}

	@Test
	public void defineFunctionalIndex() throws Exception {

		indexFactoryBean.setDefine(true);

		Index actualIndex = indexFactoryBean.createFunctionalIndex(mockQueryService, "FunctionalIndex",
			"purchaseDate", "/Orders", null);

		assertThat(actualIndex).isInstanceOf(IndexFactoryBean.IndexWrapper.class);
		assertThat(((IndexFactoryBean.IndexWrapper) actualIndex).getIndexName()).isEqualTo("FunctionalIndex");
		assertThat(((IndexFactoryBean.IndexWrapper) actualIndex).getQueryService()).isSameAs(mockQueryService);

		verify(mockQueryService, never()).createIndex(anyString(), anyString(), anyString());
		verify(mockQueryService, times(1))
			.defineIndex(eq("FunctionalIndex"), eq("purchaseDate"), eq("/Orders"));
	}

	@Test
	public void defineFunctionalIndexWithImports() throws Exception {

		indexFactoryBean.setDefine(true);

		Index actualIndex = indexFactoryBean.createFunctionalIndex(mockQueryService,
			"FunctionalIndexWithImports", "purchaseDate", "/Orders", "example.Order");

		assertThat(actualIndex).isInstanceOf(IndexFactoryBean.IndexWrapper.class);
		assertThat(((IndexFactoryBean.IndexWrapper) actualIndex).getIndexName()).isEqualTo("FunctionalIndexWithImports");
		assertThat(((IndexFactoryBean.IndexWrapper) actualIndex).getQueryService()).isSameAs(mockQueryService);

		verify(mockQueryService, never()).createIndex(anyString(), anyString(), anyString(), anyString());
		verify(mockQueryService, times(1)).defineIndex(eq("FunctionalIndexWithImports"),
			eq("purchaseDate"), eq("/Orders"), eq("example.Order"));
	}

	@Test
	public void createHashIndex() throws Exception {

		Index mockIndex = mockIndex("testCreateHashIndex.MockIndex");

		when(mockQueryService.createHashIndex(eq("HashIndex"), eq("name"), eq("/People")))
			.thenReturn(mockIndex);

		Index actualIndex = indexFactoryBean.createHashIndex(mockQueryService, "HashIndex",
			"name", "/People", "  ");

		assertThat(actualIndex).isSameAs(mockIndex);

		verify(mockQueryService, times(1))
			.createHashIndex(eq("HashIndex"), eq("name"), eq("/People"));

		verify(mockQueryService, never()).defineHashIndex(anyString(), anyString(), anyString());
	}

	@Test
	public void createHashIndexWithImports() throws Exception {

		Index mockIndex = mockIndex("testCreateHashIndexWithImports.MockIndex");

		when(mockQueryService.createHashIndex(eq("HashIndexWithImports"), eq("name"), eq("/People"),
			eq("example.Person"))).thenReturn(mockIndex);

		Index actualIndex = indexFactoryBean.createHashIndex(mockQueryService, "HashIndexWithImports",
			"name", "/People", "example.Person");

		assertThat(actualIndex).isSameAs(mockIndex);

		verify(mockQueryService, times(1)).createHashIndex(eq("HashIndexWithImports"),
			eq("name"), eq("/People"), eq("example.Person"));

		verify(mockQueryService, never()).defineHashIndex(anyString(), anyString(), anyString(), anyString());
	}

	@Test
	public void defineHashIndex() throws Exception {

		indexFactoryBean.setDefine(true);

		Index actualIndex = indexFactoryBean.createHashIndex(mockQueryService, "HashIndex",
			"name", "/People", "  ");

		assertThat(actualIndex).isInstanceOf(IndexFactoryBean.IndexWrapper.class);
		assertThat(((IndexFactoryBean.IndexWrapper) actualIndex).getIndexName()).isEqualTo("HashIndex");
		assertThat(((IndexFactoryBean.IndexWrapper) actualIndex).getQueryService()).isSameAs(mockQueryService);

		verify(mockQueryService, never()).createHashIndex(anyString(), anyString(), anyString());
		verify(mockQueryService, times(1))
			.defineHashIndex(eq("HashIndex"), eq("name"), eq("/People"));
	}

	@Test
	public void defineHashIndexWithImport() throws Exception {

		indexFactoryBean.setDefine(true);

		Index actualIndex = indexFactoryBean.createHashIndex(mockQueryService, "HashIndexWithImports",
			"name", "/People", "example.Person");

		assertThat(actualIndex).isInstanceOf(IndexFactoryBean.IndexWrapper.class);
		assertThat(((IndexFactoryBean.IndexWrapper) actualIndex).getIndexName()).isEqualTo("HashIndexWithImports");
		assertThat(((IndexFactoryBean.IndexWrapper) actualIndex).getQueryService()).isSameAs(mockQueryService);

		verify(mockQueryService, never()).createHashIndex(anyString(), anyString(), anyString(), anyString());
		verify(mockQueryService, times(1)).defineHashIndex(eq("HashIndexWithImports"),
			eq("name"), eq("/People"), eq("example.Person"));
	}

	@Test
	public void createKeyIndex() throws Exception {

		Index mockIndex = mockIndex("testCreateKeyIndex.MockKeyIndex");

		when(mockQueryService.createKeyIndex(eq("KeyIndex"), eq("id"), eq("/Example")))
			.thenReturn(mockIndex);

		Index actualIndex = indexFactoryBean.createKeyIndex(mockQueryService, "KeyIndex",
			"id", "/Example");

		assertThat(actualIndex).isSameAs(mockIndex);

		verify(mockQueryService, times(1))
			.createKeyIndex(eq("KeyIndex"), eq("id"), eq("/Example"));

		verify(mockQueryService, never()).defineKeyIndex(anyString(), anyString(), anyString());
	}

	@Test
	public void defineKeyIndex() throws Exception {

		indexFactoryBean.setDefine(true);

		Index actualIndex = indexFactoryBean.createKeyIndex(mockQueryService, "KeyIndex",
			"id", "/Example");

		assertThat(actualIndex).isInstanceOf(IndexFactoryBean.IndexWrapper.class);
		assertThat(((IndexFactoryBean.IndexWrapper) actualIndex).getIndexName()).isEqualTo("KeyIndex");
		assertThat(((IndexFactoryBean.IndexWrapper) actualIndex).getQueryService()).isSameAs(mockQueryService);

		verify(mockQueryService, never()).createKeyIndex(anyString(), anyString(), anyString());
		verify(mockQueryService, times(1)).defineKeyIndex(eq("KeyIndex"),
			eq("id"), eq("/Example"));
	}

	@Test
	public void tryToFindExistingIndexByDefinitionReturnsIndex() {

		Index mockIndexOne =
			mockIndexWithDefinition("PrimaryIndex", "id", "/People", IndexType.HASH);

		Index mockIndexTwo =
			mockIndexWithDefinition("SecondaryIndex", "id", "/People", IndexType.PRIMARY_KEY);

		Index mockIndexThree =
			mockIndexWithDefinition("TernaryIndex", "purchaseDate", "/Orders",
				IndexType.FUNCTIONAL);

		when(mockQueryService.getIndexes()).thenReturn(Arrays.asList(mockIndexOne, mockIndexTwo, mockIndexThree));

		assertThat(indexFactoryBean.tryToFindExistingIndexByDefinition(mockQueryService,
			"id", "/People", IndexType.PRIMARY_KEY).orElse(null)).isSameAs(mockIndexTwo);

		assertThat(indexFactoryBean.tryToFindExistingIndexByDefinition(mockQueryService,
			"purchaseDate", "/Orders", IndexType.FUNCTIONAL).orElse(null)).isSameAs(mockIndexThree);

		assertThat(indexFactoryBean.tryToFindExistingIndexByDefinition(mockQueryService,
			"id", "/People", IndexType.HASH).orElse(null)).isSameAs(mockIndexOne);

		verify(mockQueryService, times(3)).getIndexes();
	}

	@Test
	public void tryToFindExistingIndexByDefinitionReturnsNull() {

		Index mockIndex =
			mockIndexWithDefinition("PrimaryIndex", "id", "/People", IndexType.HASH);

		when(mockQueryService.getIndexes()).thenReturn(Collections.singletonList(mockIndex));

		assertThat(indexFactoryBean.tryToFindExistingIndexByDefinition(mockQueryService,
			"key", "/People", IndexType.HASH).orElse(null)).isNull();

		assertThat(indexFactoryBean.tryToFindExistingIndexByDefinition(mockQueryService,
			"id", "/Persons", IndexType.HASH).orElse(null)).isNull();

		assertThat(indexFactoryBean.tryToFindExistingIndexByDefinition(mockQueryService,
			"id", "/People", IndexType.KEY).orElse(null)).isNull();

		verify(mockQueryService, times(3)).getIndexes();
	}

	@Test
	public void tryToFindExistingIndexByDefinitionWithQueryServiceHavingNoIndexesReturnsNull() {

		when(mockQueryService.getIndexes()).thenReturn(Collections.emptyList());

		assertThat(indexFactoryBean.tryToFindExistingIndexByDefinition(mockQueryService,
			"id", "/People", IndexType.KEY).orElse(null)).isNull();

		verify(mockQueryService, times(1)).getIndexes();
	}

	@Test
	public void tryToFindExistingIndexByDefinitionWithQueryServiceReturningNullIndexesReturnsNull() {

		when(mockQueryService.getIndexes()).thenReturn(null);

		assertThat(indexFactoryBean.tryToFindExistingIndexByDefinition(mockQueryService,
			"id", "/People", IndexType.KEY).orElse(null)).isNull();

		verify(mockQueryService, times(1)).getIndexes();
	}

	@Test
	public void tryToFindExistingIndexByNameReturnsIndex() {

		Index mockIndexOne = mockIndex("PrimaryIndex");
		Index mockIndexTwo = mockIndex("SecondaryIndex");
		Index mockIndexThree = mockIndex("TernaryIndex");

		when(mockQueryService.getIndexes()).thenReturn(Arrays.asList(mockIndexOne, mockIndexTwo, mockIndexThree));

		assertThat(indexFactoryBean.tryToFindExistingIndexByName(mockQueryService, "PRIMARYINDEX")
			.orElse(null)).isSameAs(mockIndexOne);

		assertThat(indexFactoryBean.tryToFindExistingIndexByName(mockQueryService, "SecondaryIndex")
			.orElse(null)).isSameAs(mockIndexTwo);

		assertThat(indexFactoryBean.tryToFindExistingIndexByName(mockQueryService, "ternaryindex")
			.orElse(null)).isSameAs(mockIndexThree);

		verify(mockQueryService, times(3)).getIndexes();
	}

	@Test
	public void tryToFindExistingIndexByNameReturnsNull() {

		Index mockIndexOne = mockIndex("PrimaryIndex");
		Index mockIndexTwo = mockIndex("SecondaryIndex");
		Index mockIndexThree = mockIndex("TernaryIndex");

		when(mockQueryService.getIndexes()).thenReturn(Arrays.asList(mockIndexOne, mockIndexTwo, mockIndexThree));

		assertThat(indexFactoryBean.tryToFindExistingIndexByName(mockQueryService, null)
			.orElse(null)).isNull();

		assertThat(indexFactoryBean.tryToFindExistingIndexByName(mockQueryService, "")
			.orElse(null)).isNull();

		assertThat(indexFactoryBean.tryToFindExistingIndexByName(mockQueryService, "  ")
			.orElse(null)).isNull();

		assertThat(indexFactoryBean.tryToFindExistingIndexByName(mockQueryService, "Primary Index")
			.orElse(null)).isNull();

		assertThat(indexFactoryBean.tryToFindExistingIndexByName(mockQueryService, "Secondary_Index")
			.orElse(null)).isNull();

		assertThat(indexFactoryBean.tryToFindExistingIndexByName(mockQueryService, "QuadIndex")
			.orElse(null)).isNull();

		verify(mockQueryService, times(6)).getIndexes();
	}

	@Test
	public void tryToFindExistingIndexByNameWithQueryServiceHavingNoIndexesReturnsNull() {

		when(mockQueryService.getIndexes()).thenReturn(Collections.emptyList());

		assertThat(indexFactoryBean.tryToFindExistingIndexByName(mockQueryService, "TestIndex")
			.orElse(null)).isNull();

		assertThat(indexFactoryBean.tryToFindExistingIndexByName(mockQueryService, "KeyIndex")
			.orElse(null)).isNull();

		assertThat(indexFactoryBean.tryToFindExistingIndexByName(mockQueryService, "IdIndex")
			.orElse(null)).isNull();

		verify(mockQueryService, times(3)).getIndexes();
	}

	@Test
	public void tryToFindExistingIndexByNameWithQueryServiceReturningNullIndexesReturnsNull() {

		when(mockQueryService.getIndexes()).thenReturn(null);

		assertThat(indexFactoryBean.tryToFindExistingIndexByName(mockQueryService, "TestIndex")
			.orElse(null)).isNull();

		assertThat(indexFactoryBean.tryToFindExistingIndexByName(mockQueryService, "KeyIndex")
			.orElse(null)).isNull();

		assertThat(indexFactoryBean.tryToFindExistingIndexByName(mockQueryService, "IdIndex")
			.orElse(null)).isNull();

		verify(mockQueryService, times(3)).getIndexes();
	}

	@Test
	public void getObjectReturnsFactoryIndex() throws Exception {

		Index mockIndex = mockIndex("testGetObjectReturnsFactoryIndex.MockIndex");

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		doReturn(mockIndex).when(indexFactoryBean).createIndex(eq(mockQueryService),
			eq("testGetObjectReturnsFactoryIndex.MockIndex"));

		indexFactoryBean.setName("testGetObjectReturnsFactoryIndex.MockIndex");
		indexFactoryBean.setExpression("id");
		indexFactoryBean.setFrom("/Mock");
		indexFactoryBean.afterPropertiesSet();

		assertThat(indexFactoryBean.getIndex()).isEqualTo(mockIndex);
		assertThat(indexFactoryBean.getObject()).isEqualTo(mockIndex);
		assertThat(indexFactoryBean.getObject()).isEqualTo(mockIndex);

		verify(indexFactoryBean, times(1))
			.createIndex(eq(mockQueryService), eq("testGetObjectReturnsFactoryIndex.MockIndex"));
	}

	@Test
	public void getObjectReturnsLookedUpIndex() throws Exception {

		Index mockIndexOne = mockIndex("MockIndexOne");
		Index mockIndexTwo = mockIndex("MockIndexTwo");

		when(mockQueryService.getIndexes()).thenReturn(Arrays.asList(mockIndexOne, mockIndexTwo));

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setName("MockIndexTwo");

		assertThat(indexFactoryBean.getObject()).isEqualTo(mockIndexTwo);
	}

	@Test
	public void getObjectReturnsNull() {

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		indexFactoryBean.setName("TestIndex");

		assertThat(indexFactoryBean.getObject()).isNull();
	}

	@Test
	public void getObjectTypeReturnsIndexClassType() {
		assertThat(newIndexFactoryBean().getObjectType()).isEqualTo(Index.class);
	}

	@Test
	@SuppressWarnings("all")
	public void getObjectTypeReturnsMockIndexClassType() throws Exception {

		Index mockIndex = mockIndex("MockIndex");

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		doReturn(mockIndex).when(indexFactoryBean).getIndex();

		assertThat(indexFactoryBean.getObjectType()).isEqualTo(mockIndex.getClass());
	}

	@Test
	public void isSingletonReturnsTrue() {
		assertThat(indexFactoryBean.isSingleton()).isTrue();
	}

	@Test
	public void setAndIsIgnoreIfExists() {

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		assertThat(indexFactoryBean.isIgnoreIfExists()).isFalse();

		indexFactoryBean.setIgnoreIfExists(true);

		assertThat(indexFactoryBean.isIgnoreIfExists()).isTrue();

		indexFactoryBean.setIgnoreIfExists(false);

		assertThat(indexFactoryBean.isIgnoreIfExists()).isFalse();
	}

	@Test
	public void setAndIsOverride() {

		IndexFactoryBean indexFactoryBean = newIndexFactoryBean();

		assertThat(indexFactoryBean.isOverride()).isFalse();

		indexFactoryBean.setOverride(true);

		assertThat(indexFactoryBean.isOverride()).isTrue();

		indexFactoryBean.setOverride(false);

		assertThat(indexFactoryBean.isOverride()).isFalse();
	}

	@Test
	public void defineMultipleIndexesWithSeparateIndexFactoryBeansSameSpringContext() throws Exception {

		ConfigurableBeanFactory mockBeanFactory = mock(ConfigurableBeanFactory.class,
			"testDefineMultipleIndexesWithSeparateIndexFactoryBeansSameSpringContext.MockBeanFactory");

		Cache mockCacheOne =
			mock(Cache.class, "testDefineMultipleIndexesWithSeparateIndexFactoryBeansSameSpringContext.MockCacheOne");

		Cache mockCacheTwo =
			mock(Cache.class, "testDefineMultipleIndexesWithSeparateIndexFactoryBeansSameSpringContext.MockCacheTwo");

		AtomicReference<QueryService> queryServiceReference = new AtomicReference<>(null);

		doAnswer(invocation -> (queryServiceReference.get() != null)).when(mockBeanFactory)
			.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE));

		doAnswer(invocation -> queryServiceReference.get()).when(mockBeanFactory)
			.getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE), eq(QueryService.class));

		doAnswer(invocation -> {

			assertEquals(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE,
				invocation.getArgument(0));

			queryServiceReference.compareAndSet(null, invocation.getArgument(1));

			return null;
		}).when(mockBeanFactory).registerSingleton(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE),
			any(QueryService.class));

		when(mockCacheOne.getQueryService()).thenReturn(mockQueryService);

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

		verify(mockBeanFactory, times(2))
			.containsBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE));

		verify(mockBeanFactory, times(1))
			.getBean(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE), eq(QueryService.class));

		verify(mockBeanFactory, times(1))
			.registerSingleton(eq(GemfireConstants.DEFAULT_GEMFIRE_INDEX_DEFINITION_QUERY_SERVICE),
				same(mockQueryService));

		verify(mockCacheOne, times(1)).getQueryService();
		verify(mockCacheTwo, never()).getQueryService();

		verify(mockQueryService, times(1))
			.defineKeyIndex(eq("PersonIdIndex"), eq("id"), eq("/People"));

		verify(mockQueryService, times(1))
			.defineHashIndex(eq("PurchaseDateIndex"), eq("purchaseDate"), eq("/Orders"),
				eq("org.example.Order"));
	}

	@Test
	@SuppressWarnings("deprecation")
	public void indexWrapperDelegation() {

		Index mockIndex = mockIndex("testIndexWrapperDelegation.MockIndex");

		IndexStatistics mockIndexStats = mock(IndexStatistics.class, "testIndexWrapperDelegation.MockIndexStats");

		QueryService mockQueryService = mock(QueryService.class, "testIndexWrapperDelegation.MockQueryService");

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

		IndexFactoryBean.IndexWrapper indexWrapper =
			new IndexFactoryBean.IndexWrapper(mockQueryService, "MockIndex");

		assertThat(indexWrapper).isNotNull();
		assertThat(indexWrapper.getIndexName()).isEqualTo("MockIndex");
		assertThat(indexWrapper.getQueryService()).isEqualTo(mockQueryService);

		Index actualIndex = indexWrapper.resolveIndex();

		assertThat(actualIndex).isSameAs(mockIndex);

		assertThat(indexWrapper.getCanonicalizedFromClause()).isEqualTo("/Example");
		assertThat(indexWrapper.getCanonicalizedIndexedExpression()).isEqualTo("ID");
		assertThat(indexWrapper.getCanonicalizedProjectionAttributes()).isEqualTo("identifier");
		assertThat(indexWrapper.getFromClause()).isEqualTo("Example");
		assertThat(indexWrapper.getIndexedExpression()).isEqualTo("id");
		assertThat(indexWrapper.getName()).isEqualTo("MockIndex");
		assertThat(indexWrapper.getProjectionAttributes()).isEqualTo("id");
		assertThat(indexWrapper.getStatistics()).isSameAs(mockIndexStats);
		assertThat(indexWrapper.getType()).isEqualTo(org.apache.geode.cache.query.IndexType.HASH);

		Index sameIndex = indexWrapper.resolveIndex();

		assertThat(sameIndex).isSameAs(actualIndex);
	}

	@Test(expected = IllegalArgumentException.class)
	public void createIndexWrapperWithNullQueryService() {
		try {
			new IndexFactoryBean.IndexWrapper(null, "TestIndex");
		}
		catch (IllegalArgumentException expected) {
			assertThat(expected).hasMessage("QueryService is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = IllegalArgumentException.class)
	public void createIndexWrapperWithNoIndexName() {
		try {
			new IndexFactoryBean.IndexWrapper(mockQueryService, "  ");
		}
		catch (IllegalArgumentException expected) {
			assertThat(expected).hasMessage("Name of Index is required");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test(expected = GemfireIndexException.class)
	public void indexWrapperGetIndexThrowsExceptionWhenIndexNotFound() {

		when(mockQueryService.getIndexes()).thenReturn(Collections.emptyList());

		IndexFactoryBean.IndexWrapper indexWrapper =
			new IndexFactoryBean.IndexWrapper(mockQueryService, "NonExistingIndex");

		assertThat(indexWrapper).isNotNull();
		assertThat(indexWrapper.getIndexName()).isEqualTo("NonExistingIndex");
		assertThat(indexWrapper.getQueryService()).isSameAs(mockQueryService);

		try {
			indexWrapper.resolveIndex();
		}
		catch (GemfireIndexException expected) {
			assertThat(expected).hasMessage("Index with name [NonExistingIndex] was not found");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}
}
