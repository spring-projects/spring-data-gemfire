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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.RegionAttributes;
import com.gemstone.gemfire.cache.RegionFactory;
import com.gemstone.gemfire.cache.RegionShortcut;
import com.gemstone.gemfire.cache.query.Index;
import com.gemstone.gemfire.cache.query.QueryService;

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
import org.springframework.data.gemfire.IndexType;
import org.springframework.data.gemfire.config.annotation.test.entities.ClientRegionEntity;
import org.springframework.data.gemfire.config.annotation.test.entities.CollocatedPartitionRegionEntity;
import org.springframework.data.gemfire.config.annotation.test.entities.GenericRegionEntity;
import org.springframework.data.gemfire.config.annotation.test.entities.LocalRegionEntity;
import org.springframework.data.gemfire.config.annotation.test.entities.NonEntity;
import org.springframework.data.gemfire.config.annotation.test.entities.ReplicateRegionEntity;

/**
 * Unit tests for the {@link EnableIndexes} and {@link IndexConfiguration} class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.config.annotation.EnableIndexes
 * @see org.springframework.data.gemfire.config.annotation.IndexConfiguration
 * @since 1.9.0
 */
public class EnableIndexesConfigurationUnitTests {

	private ConfigurableApplicationContext applicationContext;

	@After
	public void tearDown() {
		if (applicationContext != null) {
			applicationContext.close();
		}
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
	protected ConfigurableApplicationContext newApplicationContext(Class<?>... annotatedClasses) {
		ConfigurableApplicationContext applicationContext = new AnnotationConfigApplicationContext(annotatedClasses);
		applicationContext.registerShutdownHook();
		return applicationContext;
	}

	@Test
	public void pesistentEntityIndexesCreatedSuccessfully() {
		applicationContext = newApplicationContext(IndexedPersistentEntityConfiguration.class);

		Index customersIdIdx = applicationContext.getBean("CustomersIdKeyIdx", Index.class);

		assertIndex(customersIdIdx, "CustomersIdKeyIdx", "id", "Customers", IndexType.KEY);

		Index customersFirstNameIdx = applicationContext.getBean("CustomersFirstNameFunctionalIdx", Index.class);

		assertIndex(customersFirstNameIdx, "CustomersFirstNameFunctionalIdx", "first_name", "/LoyalCustomers",
			IndexType.FUNCTIONAL);

		Index lastNameIdx = applicationContext.getBean("LastNameIdx", Index.class);

		assertIndex(lastNameIdx, "LastNameIdx", "lastName", "Customers", IndexType.HASH);
	}

	@Configuration
	@SuppressWarnings("unused")
	static class CacheConfiguration {

		@Bean
		@SuppressWarnings("unchecked")
		Cache gemfireCache() throws Exception {
			Cache mockCache = mock(Cache.class);
			QueryService mockQueryService = mock(QueryService.class);
			RegionFactory mockRegionFactory = mock(RegionFactory.class);

			when(mockCache.getQueryService()).thenReturn(mockQueryService);
			when(mockCache.createRegionFactory()).thenReturn(mockRegionFactory);
			when(mockCache.createRegionFactory(any(RegionAttributes.class))).thenReturn(mockRegionFactory);
			when(mockCache.createRegionFactory(any(RegionShortcut.class))).thenReturn(mockRegionFactory);
			when(mockCache.createRegionFactory(anyString())).thenReturn(mockRegionFactory);

			when(mockQueryService.createHashIndex(anyString(), anyString(), anyString()))
				.thenAnswer(new HashIndexAnswer());

			when(mockQueryService.createIndex(anyString(), anyString(), anyString()))
				.thenAnswer(new FunctionalIndexAnswer());

			when(mockQueryService.createKeyIndex(anyString(), anyString(), anyString()))
				.thenAnswer(new KeyIndexAnswer());

			return mockCache;
		}
	}

	static abstract class AbstractIndexAnswer implements Answer<Index> {

		@Override
		public Index answer(InvocationOnMock invocation) throws Throwable {
			String name = invocation.getArgumentAt(0, String.class);
			String expression = invocation.getArgumentAt(1, String.class);
			String from = invocation.getArgumentAt(2, String.class);

			Index mockIndex = mock(Index.class, name);

			when(mockIndex.getName()).thenReturn(name);
			when(mockIndex.getIndexedExpression()).thenReturn(expression);
			when(mockIndex.getFromClause()).thenReturn(from);
			when(mockIndex.getType()).thenReturn(getType().getGemfireIndexType());

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

	@EnableIndexes
	@EnableEntityDefinedRegions(basePackageClasses = NonEntity.class,
		excludeFilters = @ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = {
			ClientRegionEntity.class, CollocatedPartitionRegionEntity.class, GenericRegionEntity.class,
			LocalRegionEntity.class, ReplicateRegionEntity.class }))
	static class IndexedPersistentEntityConfiguration extends CacheConfiguration {

	}
}
