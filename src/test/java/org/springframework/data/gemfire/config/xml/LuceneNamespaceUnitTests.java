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

package org.springframework.data.gemfire.config.xml;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.lucene.LuceneIndexFactory;
import org.apache.geode.cache.lucene.LuceneSerializer;
import org.apache.geode.cache.lucene.LuceneService;
import org.apache.lucene.analysis.Analyzer;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.stubbing.Answer;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.lang.Nullable;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringRunner;

/**
 * Unit tests for the {@link LuceneServiceParser} and {@link LuceneIndexParser}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.apache.geode.cache.lucene.LuceneIndex
 * @see org.apache.geode.cache.lucene.LuceneService
 * @see org.springframework.data.gemfire.config.xml.LuceneIndexParser
 * @see org.springframework.data.gemfire.config.xml.LuceneServiceParser
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

	@Autowired
	private LuceneSerializer luceneSerializer;

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

	private void assertLuceneIndex(LuceneIndex index, String name, String regionPath) {

		assertThat(index).isNotNull();
		assertThat(index.getName()).isEqualTo(name);
		assertThat(index.getRegionPath()).isEqualTo(regionPath);
	}

	private void assertLuceneIndexWithFieldAnalyzers(LuceneIndex index, String name, String regionPath,
			String... keys) {

		assertLuceneIndex(index, name, regionPath);
		assertThat(index.getFieldAnalyzers()).hasSize(keys.length);
		assertThat(index.getFieldAnalyzers()).containsKeys(keys);
		assertThat(index.getFieldNames()).isEmpty();
	}

	private void assertLuceneIndexWithFields(LuceneIndex index, String name, String regionPath, String... fieldNames) {

		assertLuceneIndex(index, name, regionPath);
		assertThat(index.getFieldAnalyzers()).isEmpty();
		assertThat(index.getFieldNames()).contains(fieldNames);
	}

	@Test
	public void luceneServiceConfigurationAndInteractionsAreCorrect() {

		assertThat(this.luceneService).isNotNull();
		verify(this.luceneService, times(4)).createIndexFactory();
		verify(this.luceneService, never()).destroyIndex(anyString(), anyString());
	}

	@Test
	public void luceneIndexOneIsConfiguredCorrectly() {

		assertLuceneIndexWithFields(this.luceneIndexOne, "IndexOne", "/Example",
			"fieldOne", "fieldTwo");

		assertThat(this.luceneIndexOne.getLuceneSerializer()).isNull();
	}

	@Test
	public void luceneIndexTwoIsConfiguredCorrectly() {

		assertLuceneIndexWithFieldAnalyzers(this.luceneIndexTwo, "IndexTwo", "/AnotherExample",
			"fieldOne", "fieldTwo");

		assertThat(this.luceneIndexTwo.getLuceneSerializer()).isInstanceOf(LuceneSerializer.class);
	}

	@Test
	public void luceneIndexThreeIsConfiguredCorrectly() {

		assertLuceneIndexWithFields(this.luceneIndexThree, "IndexThree", "/Example",
			"singleField");

		assertThat(this.luceneIndexThree.getLuceneSerializer()).isNull();
	}

	@Test
	public void luceneIndexFourIsConfiguredCorrectly() {

		assertLuceneIndexWithFieldAnalyzers(this.luceneIndexFour, "IndexFour", "/YetAnotherExample",
			"singleField");

		assertThat(this.luceneIndexFour.getLuceneSerializer()).isEqualTo(luceneSerializer);
	}

	public static class LuceneNamespaceUnitTestsBeanFactoryPostProcessor implements BeanFactoryPostProcessor {

		@Override
		public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
			beanFactory.getBeanDefinition("luceneService")
				.setBeanClassName(MockLuceneServiceFactoryBean.class.getName());
		}
	}

	public static class MockLuceneServiceFactoryBean implements FactoryBean<LuceneService>, InitializingBean {

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

				when(this.luceneService.createIndexFactory()).thenAnswer(invocation -> {

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

					AtomicReference<LuceneSerializer> luceneSerializer = new AtomicReference<>(null);

					when(mockLuceneIndexFactory.setLuceneSerializer(any())).thenAnswer(setLuceneSerializerInvocation -> {
						luceneSerializer.set(setLuceneSerializerInvocation.getArgument(0));
						return mockLuceneIndexFactory;
					});

					Answer mockLuceneIndex =
						mockLuceneIndex(this.luceneService, fieldAnalyzers, fieldNames, luceneSerializer);

					doAnswer(mockLuceneIndex).when(mockLuceneIndexFactory).create(anyString(), anyString());

					return mockLuceneIndexFactory;
				});

				return this.luceneService;
			});
		}

		@SuppressWarnings("unchecked")
		private Answer<LuceneIndex> mockLuceneIndex(LuceneService mockLuceneService,
				Map<String, Analyzer> fieldAnalyzers, List<String> fieldNames,
					AtomicReference<LuceneSerializer> luceneSerializer) {

			return invocation -> {

				String indexName = invocation.getArgument(0);
				String regionPath = invocation.getArgument(1);

				LuceneIndex mockLuceneIndex = mock(LuceneIndex.class, indexName);

				when(mockLuceneIndex.getFieldAnalyzers()).thenReturn(fieldAnalyzers);
				when(mockLuceneIndex.getFieldNames()).thenReturn(asArray(fieldNames));
				when(mockLuceneIndex.getLuceneSerializer()).thenAnswer(it -> luceneSerializer.get());
				when(mockLuceneIndex.getName()).thenReturn(indexName);
				when(mockLuceneIndex.getRegionPath()).thenReturn(regionPath);
				when(mockLuceneService.getIndex(eq(indexName), eq(regionPath))).thenReturn(mockLuceneIndex);

				return mockLuceneIndex;
			};
		}

		@Override
		public Class<?> getObjectType() {

			return Optional.ofNullable(this.luceneService)
				.<Class<?>>map(LuceneService::getClass)
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

	public static class MockLuceneSerializerFactoryBean implements FactoryBean<LuceneSerializer> {

		private LuceneSerializer luceneSerializer;

		@Nullable @Override
		public LuceneSerializer getObject() throws Exception {

			return Optional.ofNullable(this.luceneSerializer)
				.orElseGet(() -> this.luceneSerializer = mock(LuceneSerializer.class));
		}

		@Nullable @Override
		public Class<?> getObjectType() {

			return Optional.ofNullable(this.luceneSerializer)
				.<Class<?>>map(LuceneSerializer::getClass)
				.orElse(LuceneSerializer.class);
		}
	}
}
