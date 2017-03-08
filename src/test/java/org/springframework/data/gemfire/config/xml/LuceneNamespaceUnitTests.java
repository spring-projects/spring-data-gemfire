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

package org.springframework.data.gemfire.config.xml;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.anyVararg;
import static org.mockito.Matchers.eq;
import static org.mockito.Matchers.isA;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Map;
import java.util.Optional;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.lucene.LuceneService;
import org.apache.lucene.analysis.Analyzer;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Unit tests for the {@link LuceneServiceParser} and {@link LuceneIndexParser}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.springframework.test.context.ContextConfiguration
 * @see org.springframework.test.context.junit4.SpringRunner
 * @since 1.1.0
 */
@RunWith(SpringRunner.class)
@ContextConfiguration
@SuppressWarnings("unused")
public class LuceneNamespaceUnitTests {

	private static final String[] EMPTY_STRING_ARRAY = {};

	@Autowired
	private LuceneService luceneService;

	@Autowired
	@Qualifier("IndexOne")
	private LuceneIndex luceneIndexOne;

	@Autowired
	@Qualifier("IndexTwo")
	private LuceneIndex luceneIndexTwo;

	@Autowired
	@Qualifier("IndexThree")
	private LuceneIndex luceneIndexThree;

	@Autowired
	@Qualifier("IndexFour")
	private LuceneIndex luceneIndexFour;

	protected void assertLuceneIndex(LuceneIndex index, String name, String regionPath) {
		assertThat(index).isNotNull();
		assertThat(index.getName()).isEqualTo(name);
		assertThat(index.getRegionPath()).isEqualTo(regionPath);
	}

	protected void assertLuceneIndexWithFieldAnalyzers(LuceneIndex index, String name, String regionPath,
			String... keys) {

		assertLuceneIndex(index, name, regionPath);
		assertThat(index.getFieldNames()).isEmpty();
		assertThat(index.getFieldAnalyzers()).hasSize(keys.length);
		assertThat(index.getFieldAnalyzers()).containsKeys(keys);
	}

	protected void assertLuceneIndexWithFields(LuceneIndex index, String name, String regionPath, String... fieldNames) {
		assertLuceneIndex(index, name, regionPath);
		assertThat(index.getFieldAnalyzers()).isEmpty();
		assertThat(index.getFieldNames()).contains(fieldNames);
	}

	@Test
	@SuppressWarnings({ "deprecation", "unchecked" })
	public void luceneServiceConfigurationAndInteractionsAreCorrect() {
		assertThat(this.luceneService).isNotNull();

		verify(this.luceneService, times(1))
			.createIndex(eq("IndexOne"), eq("/Example"), eq("fieldOne"), eq("fieldTwo"));

		verify(this.luceneService, times(1))
			.createIndex(eq("IndexTwo"), eq("/AnotherExample"), isA(Map.class));

		verify(this.luceneService, never()).destroyIndex(any(LuceneIndex.class));
	}

	@Test
	public void luceneIndexOneIsConfiguredCorrectly() {
		assertLuceneIndexWithFields(this.luceneIndexOne, "IndexOne", "/Example",
			"fieldOne", "fieldTwo");
	}

	@Test
	public void luceneIndexTwoIsConfiguredCorrectly() {
		assertLuceneIndexWithFieldAnalyzers(this.luceneIndexTwo, "IndexTwo", "/AnotherExample",
			"fieldOne", "fieldTwo");
	}

	@Test
	public void luceneIndexThreeIsConfiguredCorrectly() {
		assertLuceneIndexWithFields(this.luceneIndexThree, "IndexThree", "/Example",
			"singleField");
	}

	@Test
	public void luceneIndexFourIsConfiguredCorrectly() {
		assertLuceneIndexWithFieldAnalyzers(this.luceneIndexFour, "IndexFour", "/YetAnotherExample",
			"singleField");
	}

	public static class LuceneNamespaceUnitTestsBeanFactoryPostProcessor implements BeanFactoryPostProcessor {

		@Override
		public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
			beanFactory.getBeanDefinition("luceneService")
				.setBeanClassName(MockLuceneServiceFactoryBean.class.getName());
		}
	}

	public static class MockLuceneServiceFactoryBean implements FactoryBean<LuceneService>, InitializingBean {

		@SuppressWarnings("all")
		private GemFireCache gemfireCache;

		private LuceneService luceneService;

		@Override
		public void afterPropertiesSet() throws Exception {
			assertThat(this.gemfireCache).describedAs("GemFireCache must not be null").isNotNull();
		}

		@Override
		@SuppressWarnings("unchecked")
		public LuceneService getObject() throws Exception {
			return Optional.ofNullable(this.luceneService).orElseGet(() -> {
				this.luceneService = mock(LuceneService.class);

				Answer<LuceneIndex> mockLuceneIndex = newMockLuceneIndex(this.luceneService);

				doAnswer(mockLuceneIndex).when(this.luceneService)
					.createIndex(anyString(), anyString(), (String[]) anyVararg());

				doAnswer(mockLuceneIndex).when(this.luceneService)
					.createIndex(anyString(), anyString(), isA(Map.class));

				return this.luceneService;
			});
		}

		@SuppressWarnings("unchecked")
		private Answer<LuceneIndex> newMockLuceneIndex(LuceneService mockLuceneService) {
			return (invocationOnMock) -> {
				String indexName = invocationOnMock.getArgumentAt(0, String.class);
				String regionPath = invocationOnMock.getArgumentAt(1, String.class);

				LuceneIndex mockLuceneIndex = mock(LuceneIndex.class, indexName);

				when(mockLuceneIndex.getName()).thenReturn(indexName);
				when(mockLuceneIndex.getRegionPath()).thenReturn(regionPath);

				if (invocationOnMock.getArguments().length > 2) {
					Object fields = invocationOnMock.getArgumentAt(2, Object.class);

					if (fields instanceof Map) {
						when(mockLuceneIndex.getFieldAnalyzers()).thenReturn((Map<String, Analyzer>) fields);
						when(mockLuceneIndex.getFieldNames()).thenReturn(EMPTY_STRING_ARRAY);
					}
					else {
						when(mockLuceneIndex.getFieldAnalyzers()).thenReturn(Collections.emptyMap());
						when(mockLuceneIndex.getFieldNames()).thenReturn(extractFields(invocationOnMock));
					}
				}

				when(mockLuceneService.getIndex(eq(indexName), eq(regionPath))).thenReturn(mockLuceneIndex);

				return mockLuceneIndex;
			};
		}

		private String[] asStringArray(Object fields) {
			return (fields instanceof String[] ? (String[]) fields : String.valueOf(fields).split(", "));
		}

		@SuppressWarnings("all")
		private String[] extractFields(InvocationOnMock invocationOnMock) {
			String[] fields = new String[invocationOnMock.getArguments().length - 2];
			System.arraycopy(invocationOnMock.getArguments(), 2, fields, 0, fields.length);
			return fields;
		}

		@Override
		public Class<?> getObjectType() {
			return Optional.ofNullable(this.luceneService).<Class<?>>map(LuceneService::getClass)
				.orElse(LuceneService.class);
		}

		public void setCache(GemFireCache gemfireCache) {
			this.gemfireCache = gemfireCache;
		}
	}

	public static class MockAnalyzerFactoryBean implements FactoryBean<Analyzer> {

		@SuppressWarnings("unused")
		private Analyzer analyzer;

		private String name;

		@Override
		public Analyzer getObject() throws Exception {
			return Optional.ofNullable(this.analyzer).orElseGet(() -> this.analyzer = mock(Analyzer.class, getName()));
		}

		@Override
		public Class<?> getObjectType() {
			return Optional.ofNullable(this.analyzer).<Class<?>>map(Analyzer::getClass).orElse(Analyzer.class);
		}

		public void setName(String name) {
			this.name = name;
		}

		public String getName() {
			return this.name;
		}
	}
}
