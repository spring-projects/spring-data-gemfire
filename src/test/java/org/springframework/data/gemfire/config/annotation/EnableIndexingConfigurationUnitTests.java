/*
 * Copyright 2016 the original author or authors.
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

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import org.apache.geode.cache.Cache;
import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.RegionAttributes;
import org.apache.geode.cache.RegionFactory;
import org.apache.geode.cache.RegionShortcut;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.lucene.LuceneIndexFactory;
import org.apache.geode.cache.lucene.LuceneService;
import org.apache.geode.cache.query.Index;
import org.apache.geode.cache.query.QueryService;
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
 * @see EnableIndexing
 * @see org.springframework.data.gemfire.config.annotation.IndexConfiguration
 * @since 1.9.0
 */
public class EnableIndexingConfigurationUnitTests {

	private static final Set<Index> indexes = new HashSet<>();

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		Optional.ofNullable(this.applicationContext).ifPresent(ConfigurableApplicationContext::close);
		indexes.clear();
	}

	/* (non-Javadoc) */
	protected void assertIndex(Index index, String name, String expression, String from, IndexType indexType) {
		assertThat(index).isNotNull();
		assertThat(index.getName()).isEqualTo(name);
		assertThat(index.getIndexedExpression()).isEqualTo(expression);
		assertThat(index.getFromClause()).isEqualTo(from);
		assertThat(index.getType()).isEqualTo(indexType.getGemfireIndexType());
	}

	/* (non-Javadoc) */
	protected void assertLuceneIndex(LuceneIndex index, String name, String regionPath, String... fields) {
		assertThat(index).isNotNull();
		assertThat(index.getName()).isEqualTo(name);
		assertThat(index.getRegionPath()).isEqualTo(regionPath);
		assertThat(index.getFieldNames()).contains(fields);
		assertThat(index.getFieldNames()).hasSize(fields.length);
	}

	/* (non-Javadoc) */
	protected ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		ConfigurableApplicationContext applicationContext = new AnnotationConfigApplicationContext(annotatedClasses);
		applicationContext.registerShutdownHook();
		return applicationContext;
	}

	@Test
	public void persistentEntityIndexesCreatedSuccessfully() {
		applicationContext = newApplicationContext(IndexedPersistentEntityConfiguration.class);

		assertLuceneIndexes(applicationContext);
		assertOqlIndexes(applicationContext);
	}

	private void assertLuceneIndexes(ConfigurableApplicationContext applicationContext) {
		LuceneIndex luceneIndex = applicationContext.getBean("TitleLuceneIdx", LuceneIndex.class);

		assertLuceneIndex(luceneIndex, "TitleLuceneIdx", "Customers", "title");
	}

	private void assertOqlIndexes(ConfigurableApplicationContext applicationContext) {
		Index customersIdIdx = applicationContext.getBean("CustomersIdKeyIdx", Index.class);

		assertIndex(customersIdIdx, "CustomersIdKeyIdx", "id", "Customers", IndexType.KEY);

		Index customersFirstNameIdx = applicationContext.getBean("CustomersFirstNameFunctionalIdx", Index.class);

		assertIndex(customersFirstNameIdx, "CustomersFirstNameFunctionalIdx", "first_name",
			"/LoyalCustomers", IndexType.FUNCTIONAL);

		Index lastNameIdx = applicationContext.getBean("LastNameIdx", Index.class);

		assertIndex(lastNameIdx, "LastNameIdx", "surname", "Customers", IndexType.HASH);
	}

	@Test
	public void persistentEntityIndexesWillNotBeCreated() {
		applicationContext = newApplicationContext(NoIndexesCreatedForIndexedPersistentEntityConfiguration.class);

		Map<String, Index> indexes = applicationContext.getBeansOfType(Index.class);

		assertThat(indexes).isNotNull();
		assertThat(indexes.isEmpty()).isTrue();
	}

	@Test
	public void indexAnnotatedEntityPropertyDoesNotOverrideIndexBeanDefinition() {
		applicationContext = newApplicationContext(IndexAnnotatedEntityPropertyDoesNotOverrideBeanDefinitionConfiguration.class);

		Index lastNameIdx = applicationContext.getBean("LastNameIdx", Index.class);

		assertIndex(lastNameIdx, "LastNameIdx", "last_name", "/People", IndexType.HASH);
	}

	@Test
	public void indexAnnotatedEntityPropertyOverridesIndexBeanDefinition() {
		applicationContext = newApplicationContext(IndexAnnotatedEntityPropertyOverridesIndexBeanDefinitionConfiguration.class);

		Index customersFirstNameIdx = applicationContext.getBean("CustomersFirstNameFunctionalIdx", Index.class);

		assertIndex(customersFirstNameIdx, "CustomersFirstNameFunctionalIdx",
			"first_name", "/LoyalCustomers", IndexType.FUNCTIONAL);
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
			when(mockQueryService.getIndexes()).thenReturn(indexes);

			when(mockQueryService.createHashIndex(anyString(), anyString(), anyString()))
				.thenAnswer(new HashIndexAnswer());

			when(mockQueryService.createIndex(anyString(), anyString(), anyString()))
				.thenAnswer(new FunctionalIndexAnswer());

			when(mockQueryService.createKeyIndex(anyString(), anyString(), anyString()))
				.thenAnswer(new KeyIndexAnswer());

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
		LuceneService luceneService() {
			LuceneService mockLuceneService = mock(LuceneService.class);

			LuceneIndexFactory mockLuceneIndexFactory = mock(LuceneIndexFactory.class);

			doReturn(mockLuceneIndexFactory).when(mockLuceneService).createIndexFactory();

			doAnswer(invocation -> {
				LuceneIndex mockLuceneIndex = mock(LuceneIndex.class);

				String indexName = invocation.getArgument(0);
				String regionPath = invocation.getArgument(1);

				when(mockLuceneIndex.getName()).thenReturn(indexName);
				when(mockLuceneIndex.getRegionPath()).thenReturn(regionPath);
				when(mockLuceneIndex.getFieldNames()).thenReturn(resolveFieldNames(invocation));
				when(mockLuceneService.getIndex(eq(indexName), eq(regionPath))).thenReturn(mockLuceneIndex);

				return mockLuceneIndex;
			}).when(mockLuceneIndexFactory).create(anyString(), anyString());

			return mockLuceneService;
		}

		@SuppressWarnings("all")
		String[] resolveFieldNames(InvocationOnMock invocation) {
			String[] fieldNames = new String[invocation.getArguments().length - 2];
			System.arraycopy(invocation.getArguments(), 2, fieldNames, 0, fieldNames.length);
			return fieldNames;
		}
	}

	static abstract class AbstractIndexAnswer implements Answer<Index> {

		@Override
		public Index answer(InvocationOnMock invocation) throws Throwable {
			String name = invocation.getArgument(0);
			String expression = invocation.getArgument(1);
			String from = invocation.getArgument(2);

			Index mockIndex = mock(Index.class, name);

			when(mockIndex.getName()).thenReturn(name);
			when(mockIndex.getIndexedExpression()).thenReturn(expression);
			when(mockIndex.getFromClause()).thenReturn(from);
			when(mockIndex.getType()).thenReturn(getType().getGemfireIndexType());

			indexes.add(mockIndex);

			return mockIndex;
		}

		abstract IndexType getType();

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
	static class IndexedPersistentEntityConfiguration extends GemFireConfiguration {

	}

	@EnableEntityDefinedRegions(basePackageClasses = NonEntity.class,
		excludeFilters = @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = {
			ClientRegionEntity.class, CollocatedPartitionRegionEntity.class, GenericRegionEntity.class,
			LocalRegionEntity.class, ReplicateRegionEntity.class }))
	static class NoIndexesCreatedForIndexedPersistentEntityConfiguration extends GemFireConfiguration {

	}

	@EnableIndexing
	@EnableEntityDefinedRegions(basePackageClasses = NonEntity.class,
		excludeFilters = @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = {
			ClientRegionEntity.class, CollocatedPartitionRegionEntity.class, GenericRegionEntity.class,
			LocalRegionEntity.class, ReplicateRegionEntity.class }))
	static class IndexAnnotatedEntityPropertyDoesNotOverrideBeanDefinitionConfiguration extends GemFireConfiguration {

		@Bean
		@SuppressWarnings("unused")
		IndexFactoryBean lastNameIndex(GemFireCache gemfireCache) {
			IndexFactoryBean lastNameIndex = new IndexFactoryBean();

			lastNameIndex.setCache(gemfireCache);
			lastNameIndex.setExpression("last_name");
			lastNameIndex.setFrom("/People");
			lastNameIndex.setName("LastNameIdx");
			lastNameIndex.setType(IndexType.HASH);

			return lastNameIndex;
		}
	}

	@EnableIndexing
	@EnableEntityDefinedRegions(basePackageClasses = NonEntity.class,
		excludeFilters = @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = {
			ClientRegionEntity.class, CollocatedPartitionRegionEntity.class, GenericRegionEntity.class,
			LocalRegionEntity.class, ReplicateRegionEntity.class }))
	static class IndexAnnotatedEntityPropertyOverridesIndexBeanDefinitionConfiguration extends GemFireConfiguration {

		@Bean
		@SuppressWarnings("unused")
		IndexFactoryBean firstNameIndex(GemFireCache gemfireCache) {
			IndexFactoryBean firstNameIndex = new IndexFactoryBean();

			firstNameIndex.setCache(gemfireCache);
			firstNameIndex.setExpression("given_name");
			firstNameIndex.setFrom("/ProspectiveCustomers");
			firstNameIndex.setName("CustomersFirstNameFunctionalIdx");
			firstNameIndex.setType(IndexType.HASH);

			return firstNameIndex;
		}
	}
}
