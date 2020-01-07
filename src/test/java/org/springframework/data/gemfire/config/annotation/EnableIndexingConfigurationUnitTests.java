/*
 * Copyright 2016-2020 the original author or authors.
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
 *
 */

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.RegionFactory;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.lucene.LuceneIndexFactory;
import org.apache.geode.cache.lucene.LuceneService;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.IndexExistsException;
import org.apache.geode.cache.query.IndexNameConflictException;
import org.apache.geode.cache.query.QueryService;
import org.apache.geode.internal.concurrent.ConcurrentHashSet;

import org.apache.lucene.analysis.Analyzer;
import org.junit.After;
import org.junit.Test;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.FilterType;
import org.springframework.data.gemfire.IndexFactoryBean;
import org.springframework.data.gemfire.IndexType;
import org.springframework.data.gemfire.config.annotation.test.entities.ClientRegionEntity;
import org.springframework.data.gemfire.config.annotation.test.entities.CollocatedPartitionRegionEntity;
import org.springframework.data.gemfire.config.annotation.test.entities.GenericRegionEntity;
import org.springframework.data.gemfire.config.annotation.test.entities.LocalRegionEntity;
import org.springframework.data.gemfire.config.annotation.test.entities.NonEntity;
import org.springframework.data.gemfire.config.annotation.test.entities.ReplicateRegionEntity;

/**
 * Unit tests for the {@link EnableIndexing} and {@link IndexConfiguration} class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.cache.Region
 * @see org.apache.geode.cache.lucene.LuceneIndex
 * @see org.apache.geode.cache.query.Index
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.annotation.Bean
 * @see org.springframework.context.annotation.Configuration
 * @see org.springframework.data.gemfire.IndexFactoryBean
 * @see org.springframework.data.gemfire.IndexType
 * @see org.springframework.data.gemfire.config.annotation.EnableIndexing
 * @see org.springframework.data.gemfire.config.annotation.IndexConfiguration
 * @see org.springframework.data.gemfire.mapping.annotation.Indexed
 * @see org.springframework.data.gemfire.mapping.annotation.LuceneIndexed
 * @since 1.9.0
 */
@SuppressWarnings("unused")
public class EnableIndexingConfigurationUnitTests {

	private static final Set<Index> indexes = new ConcurrentHashSet<>();

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
		indexes.clear();
	}

	private static String[] asArray(List<String> list) {
		return list.toArray(new String[list.size()]);
	}

	private static String[] toStringArray(Object[] array) {

		String[] stringArray = new String[array.length];

		int index = 0;

		for (Object element : array) {
			stringArray[index++] = String.valueOf(element);
		}

		return stringArray;
	}

	/* (non-Javadoc) */
	private static Index findIndexByName(String indexName) {

		for (Index index : indexes) {
			if (index.getName().equalsIgnoreCase(indexName)) {
				return index;
			}
		}

		return null;
	}

	/* (non-Javadoc) */
	private void assertLuceneIndex(LuceneIndex index, String name, String regionPath, String... fields) {

		assertThat(index).isNotNull();
		assertThat(index.getName()).isEqualTo(name);
		assertThat(index.getRegionPath()).isEqualTo(regionPath);
		assertThat(index.getFieldNames()).hasSize(fields.length);
		assertThat(index.getFieldNames()).contains(fields);
	}

	/* (non-Javadoc) */
	private void assertOqlIndex(Index index, String name, String expression, String from, IndexType indexType) {

		assertThat(index).isNotNull();
		assertThat(index.getName()).isEqualTo(name);
		assertThat(index.getIndexedExpression()).isEqualTo(expression);
		assertThat(index.getFromClause()).isEqualTo(from);
		assertThat(index.getType()).isEqualTo(indexType.getGemfireIndexType());
	}

	/* (non-Javadoc) */
	private ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {

		ConfigurableApplicationContext applicationContext = new AnnotationConfigApplicationContext(annotatedClasses);

		applicationContext.registerShutdownHook();

		return applicationContext;
	}

	@Test
	public void persistentEntityIndexesAreCreated() {

		this.applicationContext = newApplicationContext(IndexingEnabledWithIndexedPersistentEntityConfiguration.class);

		assertLuceneIndexes(this.applicationContext);
		assertOqlIndexes(this.applicationContext);
	}

	private void assertLuceneIndexes(ConfigurableApplicationContext applicationContext) {

		LuceneIndex luceneIndex = applicationContext.getBean("TitleLuceneIdx", LuceneIndex.class);

		assertLuceneIndex(luceneIndex, "TitleLuceneIdx", "Customers", "title");
	}

	private void assertOqlIndexes(ConfigurableApplicationContext applicationContext) {

		Index customersIdIndex = applicationContext.getBean("CustomersIdKeyIdx", Index.class);

		assertOqlIndex(customersIdIndex, "CustomersIdKeyIdx", "id", "/Customers", IndexType.KEY);

		Index customersFirstNameIndex = applicationContext.getBean("CustomersFirstNameFunctionalIdx", Index.class);

		assertOqlIndex(customersFirstNameIndex, "CustomersFirstNameFunctionalIdx", "first_name",
			"/LoyalCustomers", IndexType.FUNCTIONAL);

		Index lastNameIndex = applicationContext.getBean("LastNameIdx", Index.class);

		assertOqlIndex(lastNameIndex, "LastNameIdx", "surname", "/Customers", IndexType.HASH);
	}

	@Test
	public void persistentEntityIndexesAreNotCreated() {

		this.applicationContext =
			newApplicationContext(IndexingNotEnabledWithIndexedPersistentEntityConfiguration.class);

		Map<String, Index> indexes = this.applicationContext.getBeansOfType(Index.class);

		assertThat(indexes).isNotNull();
		assertThat(indexes).isEmpty();
	}

	@Test
	public void indexAnnotatedEntityPropertyIsIgnoredWithExistingIndexHavingSameDefinition() {

		this.applicationContext =
			newApplicationContext(IndexAnnotatedEntityPropertyIsIgnoredWithExistingIndexHavingSameDefinitionConfiguration.class);

		Index firstNameIndex = this.applicationContext.getBean("LoyalCustomersFirstNameFunctionalIdx", Index.class);

		assertOqlIndex(firstNameIndex, "LoyalCustomersFirstNameFunctionalIdx",
			"first_name", "/LoyalCustomers", IndexType.FUNCTIONAL);

		assertThat(findIndexByName("CustomersFirstNameFunctionalIdx")).isNull();
	}

	@Test
	public void indexAnnotatedEntityPropertyIsIgnoredWithExistingIndexHavingSameName() {

		this.applicationContext =
			newApplicationContext(IndexAnnotatedEntityPropertyIsIgnoredWithExistingIndexHavingSameNameConfiguration.class);

		Index lastNameIndex = this.applicationContext.getBean("LastNameIdx", Index.class);

		assertOqlIndex(lastNameIndex, "LastNameIdx", "last_name", "/People", IndexType.HASH);
	}

	@Configuration
	@SuppressWarnings("unused")
	static class GemFireConfiguration {

		@Bean
		@SuppressWarnings("unchecked")
		Cache gemfireCache() throws Exception {
			return mockQueryService(mockRegionFactory(mock(Cache.class, "MockGemFireCache")));
		}

		Cache mockQueryService(Cache mockCache) throws Exception {

			QueryService mockQueryService = mock(QueryService.class);

			when(mockCache.getQueryService()).thenReturn(mockQueryService);

			when(mockQueryService.createHashIndex(anyString(), anyString(), anyString()))
				.thenAnswer(new HashIndexAnswer());

			when(mockQueryService.createIndex(anyString(), anyString(), anyString()))
				.thenAnswer(new FunctionalIndexAnswer());

			when(mockQueryService.createKeyIndex(anyString(), anyString(), anyString()))
				.thenAnswer(new KeyIndexAnswer());

			when(mockQueryService.getIndexes()).thenReturn(indexes);

			doAnswer(invocation -> {

				Index indexToRemove = invocation.getArgument(0);

				indexes.remove(findIndexByName(indexToRemove.getName()));

				return null;

			}).when(mockQueryService).removeIndex(any(Index.class));

			return mockCache;
		}

		@SuppressWarnings("unchecked")
		Cache mockRegionFactory(Cache mockCache) {

			RegionFactory mockRegionFactory = mock(RegionFactory.class);

			when(mockCache.createRegionFactory()).thenReturn(mockRegionFactory);
			when(mockCache.createRegionFactory(any(RegionAttributes.class))).thenReturn(mockRegionFactory);
			when(mockCache.createRegionFactory(any(RegionShortcut.class))).thenReturn(mockRegionFactory);
			when(mockCache.createRegionFactory(anyString())).thenReturn(mockRegionFactory);

			return mockCache;
		}

		@Bean
		@SuppressWarnings("unchecked")
		LuceneService luceneService() {
			LuceneService mockLuceneService = mock(LuceneService.class);

			when(mockLuceneService.createIndexFactory()).thenAnswer(invocation -> {
				LuceneIndexFactory mockLuceneIndexFactory = mock(LuceneIndexFactory.class);

				List<String> fieldNames = new ArrayList<>();

				when(mockLuceneIndexFactory.setFields((String[]) any())).thenAnswer(setFieldsInvocation -> {
					Collections.addAll(fieldNames, toStringArray(setFieldsInvocation.getArguments()));
					return mockLuceneIndexFactory;
				});

				Map<String, Analyzer> fieldAnalyzers = new HashMap<>();

				when(mockLuceneIndexFactory.setFields(any(Map.class))).thenAnswer(setFieldsInvocation -> {
					fieldAnalyzers.putAll(setFieldsInvocation.getArgument(0));
					return mockLuceneIndexFactory;
				});

				doAnswer(createInvocation -> {
					LuceneIndex mockLuceneIndex = mock(LuceneIndex.class);

					String indexName = createInvocation.getArgument(0);
					String regionPath = createInvocation.getArgument(1);

					when(mockLuceneIndex.getName()).thenReturn(indexName);
					when(mockLuceneIndex.getRegionPath()).thenReturn(regionPath);
					when(mockLuceneIndex.getFieldAnalyzers()).thenReturn(fieldAnalyzers);
					when(mockLuceneIndex.getFieldNames()).thenReturn(asArray(fieldNames));

					when(mockLuceneService.getIndex(eq(indexName), eq(regionPath))).thenReturn(mockLuceneIndex);

					return mockLuceneIndex;
				}).when(mockLuceneIndexFactory).create(anyString(), anyString());

				return mockLuceneIndexFactory;
			});

			return mockLuceneService;
		}
	}

	static abstract class AbstractIndexAnswer implements Answer<Index> {

		@Override
		public Index answer(InvocationOnMock invocation) throws Throwable {

			IndexType indexType = getType();

			String name = invocation.getArgument(0);
			String expression = invocation.getArgument(1);
			String from = invocation.getArgument(2);

			validateIndexDefinition(name, expression, from, indexType);
			validateIndexName(name);

			Index mockIndex = mock(Index.class, name);

			when(mockIndex.getName()).thenReturn(name);
			when(mockIndex.getIndexedExpression()).thenReturn(expression);
			when(mockIndex.getFromClause()).thenReturn(from);
			when(mockIndex.getType()).thenReturn(indexType.getGemfireIndexType());

			indexes.add(mockIndex);

			return mockIndex;
		}

		abstract IndexType getType();

		private void validateIndexDefinition(String name, String expression, String fromClause, IndexType type)
			throws IndexExistsException {

			for (Index index : indexes) {
				if (index.getIndexedExpression().equalsIgnoreCase(expression)
					&& index.getFromClause().equalsIgnoreCase(fromClause)
					&& index.getType().equals(type.getGemfireIndexType())) {

					throw new IndexExistsException(String.format(
						"Index [%1$s] has the same definition as existing Index [%2$s]",
						name, index.getName()));

				}
			}
		}

		private void validateIndexName(String name) throws IndexNameConflictException {

			for (Index index : indexes) {
				if (index.getName().equalsIgnoreCase(name)) {
					throw new IndexNameConflictException(String.format("Index with name [%s] already exists", name));
				}
			}
		}
	}

	static class FunctionalIndexAnswer extends AbstractIndexAnswer {

		@Override
		IndexType getType() {
			return IndexType.FUNCTIONAL;
		}
	}

	static class HashIndexAnswer extends AbstractIndexAnswer {

		@Override
		IndexType getType() {
			return IndexType.HASH;
		}
	}

	static class KeyIndexAnswer extends AbstractIndexAnswer {

		@Override
		IndexType getType() {
			return IndexType.KEY;
		}
	}

	@EnableIndexing
	@EnableEntityDefinedRegions(basePackageClasses = NonEntity.class,
		excludeFilters = @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = {
			ClientRegionEntity.class, CollocatedPartitionRegionEntity.class, GenericRegionEntity.class,
			LocalRegionEntity.class, ReplicateRegionEntity.class }))
	private static class IndexingEnabledWithIndexedPersistentEntityConfiguration extends GemFireConfiguration {

		@Bean("LoyalCustomers")
		Region mockRegion() {
			return mock(Region.class);
		}
	}

	@EnableEntityDefinedRegions(basePackageClasses = NonEntity.class,
		excludeFilters = @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = {
			ClientRegionEntity.class, CollocatedPartitionRegionEntity.class, GenericRegionEntity.class,
			LocalRegionEntity.class, ReplicateRegionEntity.class }))
	private static class IndexingNotEnabledWithIndexedPersistentEntityConfiguration extends GemFireConfiguration {

	}

	@EnableIndexing
	@EnableEntityDefinedRegions(basePackageClasses = NonEntity.class,
		excludeFilters = @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = {
			ClientRegionEntity.class, CollocatedPartitionRegionEntity.class, GenericRegionEntity.class,
			LocalRegionEntity.class, ReplicateRegionEntity.class }))
	static class IndexAnnotatedEntityPropertyIsIgnoredWithExistingIndexHavingSameDefinitionConfiguration
		extends GemFireConfiguration {

		@Bean("LoyalCustomers")
		Region mockRegion() {
			return mock(Region.class);
		}

		@Bean
		@SuppressWarnings("unused")
		IndexFactoryBean firstNameIndex(GemFireCache gemfireCache) {

			IndexFactoryBean firstNameIndex = new IndexFactoryBean();

			firstNameIndex.setCache(gemfireCache);
			firstNameIndex.setName("LoyalCustomersFirstNameFunctionalIdx");
			firstNameIndex.setExpression("first_name");
			firstNameIndex.setFrom("/LoyalCustomers");
			firstNameIndex.setType(IndexType.FUNCTIONAL);

			return firstNameIndex;
		}
	}

	@EnableIndexing
	@EnableEntityDefinedRegions(basePackageClasses = NonEntity.class,
		excludeFilters = @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = {
			ClientRegionEntity.class, CollocatedPartitionRegionEntity.class, GenericRegionEntity.class,
			LocalRegionEntity.class, ReplicateRegionEntity.class }))
	static class IndexAnnotatedEntityPropertyIsIgnoredWithExistingIndexHavingSameNameConfiguration
		extends GemFireConfiguration {

		@Bean("LoyalCustomers")
		Region mockRegion() {
			return mock(Region.class);
		}

		@Bean
		@SuppressWarnings("unused")
		IndexFactoryBean lastNameIndex(GemFireCache gemfireCache) {

			IndexFactoryBean lastNameIndex = new IndexFactoryBean();

			lastNameIndex.setCache(gemfireCache);
			lastNameIndex.setName("LastNameIdx");
			lastNameIndex.setExpression("last_name");
			lastNameIndex.setFrom("/People");
			lastNameIndex.setType(IndexType.HASH);

			return lastNameIndex;
		}
	}
}
