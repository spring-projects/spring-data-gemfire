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

package org.springframework.data.gemfire.search.lucene;

import static org.assertj.core.api.Java6Assertions.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.lucene.LuceneIndex;
import org.apache.geode.cache.lucene.LuceneIndexFactory;
import org.apache.geode.cache.lucene.LuceneSerializer;
import org.apache.geode.cache.lucene.LuceneService;
import org.apache.lucene.analysis.Analyzer;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.data.gemfire.GemfireUtils;

/**
 * Unit tests for {@link LuceneIndexFactoryBean}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.search.lucene.LuceneIndexFactoryBean
 * @since 1.1.0
 */
@RunWith(MockitoJUnitRunner.class)
public class LuceneIndexFactoryBeanUnitTests {

	@Mock
	private Analyzer mockAnalyzer;

	@Mock
	private BeanFactory mockBeanFactory;

	@Mock
	private GemFireCache mockCache;

	@Mock
	private LuceneIndex mockLuceneIndex;

	@Mock
	private LuceneIndexFactory mockLuceneIndexFactory;

	@Mock
	private LuceneSerializer mockLuceneSerializer;

	@Mock
	private LuceneService mockLuceneService;

	@Mock
	private Region<Object, Object> mockRegion;

	private LuceneIndexFactoryBean factoryBean;

	@Before
	@SuppressWarnings("unchecked")
	public void setup() {

		factoryBean = spy(new LuceneIndexFactoryBean());

		doReturn(mockLuceneService).when(factoryBean).resolveLuceneService(eq(mockCache));
		doReturn(mockLuceneIndexFactory).when(mockLuceneService).createIndexFactory();
		doReturn(mockLuceneIndexFactory).when(mockLuceneIndexFactory).setFields((String[]) any());
		doReturn(mockLuceneIndexFactory).when(mockLuceneIndexFactory).setFields(any(Map.class));
	}

	@Test
	public void afterPropertiesSetResolvesCacheLuceneServiceRegionPathAndCreatesLuceneIndex() throws Exception {

		doReturn(mockCache).when(factoryBean).resolveCache();
		doReturn(mockLuceneService).when(factoryBean).resolveLuceneService();
		doReturn("/Example").when(factoryBean).resolveRegionPath();
		doReturn(mockLuceneIndex).when(factoryBean).createLuceneIndex(eq("ExampleIndex"), eq("/Example"));

		factoryBean.setIndexName("ExampleIndex");

		assertThat(factoryBean.getIndexName()).isEqualTo("ExampleIndex");

		factoryBean.afterPropertiesSet();

		assertThat(factoryBean.getObject()).isEqualTo(mockLuceneIndex);

		verify(factoryBean, times(1)).resolveCache();
		verify(factoryBean, times(2)).resolveLuceneService();
		verify(factoryBean, times(1)).resolveRegionPath();
		verify(factoryBean, times(1))
			.createLuceneIndex(eq("ExampleIndex"), eq("/Example"));
	}

	@Test(expected = IllegalStateException.class)
	public void afterPropertiesSetThrowsIllegalStateExceptionWhenIndexNameNotSet() throws Exception {

		try {
			factoryBean.afterPropertiesSet();
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessageContaining("indexName was not properly initialized");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void createLuceneIndexWithAllFields() {

		factoryBean.setLuceneService(mockLuceneService);

		when(mockLuceneService.getIndex(eq("ExampleIndex"), eq("/Example"))).thenReturn(mockLuceneIndex);

		assertThat(factoryBean.getFieldAnalyzers()).isEmpty();
		assertThat(factoryBean.getFields()).isEmpty();
		assertThat(factoryBean.getLuceneService()).isSameAs(mockLuceneService);
		assertThat(factoryBean.createLuceneIndex("ExampleIndex", "/Example")).isEqualTo(mockLuceneIndex);

		verify(factoryBean, times(1)).postProcess(eq(mockLuceneIndexFactory));
		verify(mockLuceneService, times(1)).createIndexFactory();
		verify(mockLuceneIndexFactory, times(1))
			.setFields(eq(LuceneService.REGION_VALUE_FIELD));
		verify(mockLuceneIndexFactory, never()).setLuceneSerializer(any(LuceneSerializer.class));
		verify(mockLuceneIndexFactory, times(1))
			.create(eq("ExampleIndex"), eq("/Example"));
		verify(mockLuceneService, times(1))
			.getIndex(eq("ExampleIndex"), eq("/Example"));
	}

	@Test
	public void createLuceneIndexWithFieldAnalyzerMapping() {

		Map<String, Analyzer> fieldAnalyzers = Collections.singletonMap("fieldOne", mockAnalyzer);

		factoryBean.setFieldAnalyzers(fieldAnalyzers);
		factoryBean.setLuceneSerializer(mockLuceneSerializer);
		factoryBean.setLuceneService(mockLuceneService);

		when(mockLuceneService.getIndex(eq("ExampleIndex"), eq("/Example"))).thenReturn(mockLuceneIndex);

		assertThat(factoryBean.getFieldAnalyzers()).isEqualTo(fieldAnalyzers);
		assertThat(factoryBean.getFields()).isEmpty();
		assertThat(factoryBean.getLuceneService()).isSameAs(mockLuceneService);
		assertThat(factoryBean.createLuceneIndex("ExampleIndex", "/Example"))
			.isEqualTo(mockLuceneIndex);

		verify(factoryBean, times(1)).postProcess(eq(mockLuceneIndexFactory));
		verify(mockLuceneService, times(1)).createIndexFactory();
		verify(mockLuceneIndexFactory, times(1)).setFields(eq(fieldAnalyzers));
		verify(mockLuceneIndexFactory, times(1)).setLuceneSerializer(eq(mockLuceneSerializer));
		verify(mockLuceneIndexFactory, times(1))
			.create(eq("ExampleIndex"), eq("/Example"));
		verify(mockLuceneService, times(1))
			.getIndex(eq("ExampleIndex"), eq("/Example"));
	}

	@Test
	public void createLuceneIndexWithTargetedFields() {

		factoryBean.setFields("fieldOne", "fieldTwo");
		factoryBean.setLuceneSerializer(mockLuceneSerializer);
		factoryBean.setLuceneService(mockLuceneService);

		when(mockLuceneService.getIndex(eq("ExampleIndex"), eq("/Example"))).thenReturn(mockLuceneIndex);

		assertThat(factoryBean.getFieldAnalyzers()).isEmpty();
		assertThat(factoryBean.getFields()).containsAll(Arrays.asList("fieldOne", "fieldTwo"));
		assertThat(factoryBean.getLuceneService()).isSameAs(mockLuceneService);
		assertThat(factoryBean.createLuceneIndex("ExampleIndex", "/Example")).isEqualTo(mockLuceneIndex);

		verify(factoryBean, times(1)).postProcess(eq(mockLuceneIndexFactory));
		verify(mockLuceneService, times(1)).createIndexFactory();
		verify(mockLuceneIndexFactory, times(1))
			.setFields(eq("fieldOne"), eq("fieldTwo"));
		verify(mockLuceneIndexFactory, times(1)).setLuceneSerializer(eq(mockLuceneSerializer));
		verify(mockLuceneIndexFactory, times(1))
			.create(eq("ExampleIndex"), eq("/Example"));
		verify(mockLuceneService, times(1))
			.getIndex(eq("ExampleIndex"), eq("/Example"));
	}

	@Test
	public void resolveLuceneIndexPostProcessesLuceneIndexOnCreate() {

		doReturn(mockLuceneService).when(factoryBean).resolveLuceneService();
		when(mockLuceneService.getIndex(anyString(), anyString())).thenReturn(null);
		doReturn(mockLuceneIndex).when(factoryBean).createLuceneIndex(anyString(), anyString());

		assertThat(factoryBean.resolveLuceneIndex("TestIndex",
			GemfireUtils.toRegionPath("TestRegion")))
				.isEqualTo(mockLuceneIndex);

		verify(factoryBean, times(1)).resolveLuceneService();
		verify(mockLuceneService, times(1))
			.getIndex(eq("TestIndex"), eq(GemfireUtils.toRegionPath("TestRegion")));
		verify(factoryBean, times(1))
			.createLuceneIndex(eq("TestIndex"), eq(GemfireUtils.toRegionPath("TestRegion")));
		verify(factoryBean, times(1)).postProcess(eq(mockLuceneIndex));
		verify(factoryBean, times(1)).getLuceneIndex();
	}

	@Test
	@SuppressWarnings("deprecation")
	public void destroyIsSuccessful() throws Exception {

		factoryBean.setDestroy(true);
		factoryBean.setLuceneService(mockLuceneService);

		assertThat(factoryBean.isDestroy()).isTrue();
		assertThat(factoryBean.getLuceneService()).isSameAs(mockLuceneService);

		doReturn(mockLuceneIndex).when(factoryBean).getObject();
		doReturn("MockIndex").when(mockLuceneIndex).getName();
		doReturn("MockRegion").when(mockLuceneIndex).getRegionPath();

		factoryBean.destroy();

		verify(factoryBean, times(1)).getObject();
		verify(mockLuceneService, times(1))
			.destroyIndex(eq("MockIndex"), eq("MockRegion"));
	}

	@Test
	@SuppressWarnings("deprecation")
	public void destroyDoesNothingWhenDestroyIsFalse() throws Exception {

		factoryBean.setDestroy(false);
		factoryBean.setLuceneService(mockLuceneService);

		assertThat(factoryBean.isDestroy()).isFalse();
		assertThat(factoryBean.getLuceneService()).isSameAs(mockLuceneService);

		doReturn(mockLuceneIndex).when(factoryBean).getObject();

		factoryBean.destroy();

		verify(factoryBean, times(1)).getObject();
		verify(mockLuceneService, never()).destroyIndex(anyString(), anyString());
	}

	@Test
	@SuppressWarnings("deprecation")
	public void destroyDoesNothingWhenLuceneIndexIsNull() throws Exception {

		factoryBean.setDestroy(true);
		factoryBean.setLuceneService(mockLuceneService);

		assertThat(factoryBean.isDestroy()).isTrue();
		assertThat(factoryBean.getLuceneService()).isSameAs(mockLuceneService);

		doReturn(null).when(factoryBean).getObject();

		factoryBean.destroy();

		verify(factoryBean, times(1)).getObject();
		verify(mockLuceneService, never()).destroyIndex(anyString(), anyString());
	}

	@Test
	public void isLuceneIndexDestroyableReturnsTrue() {

		factoryBean.setDestroy(true);

		assertThat(factoryBean.isDestroy()).isTrue();
		assertThat(factoryBean.isLuceneIndexDestroyable(mockLuceneIndex)).isTrue();
	}

	@Test
	public void isLuceneIndexDestroyableWhenDestroyIsFalseReturnsFalse() {

		factoryBean.setDestroy(false);

		assertThat(factoryBean.isDestroy()).isFalse();
		assertThat(factoryBean.isLuceneIndexDestroyable(mockLuceneIndex)).isFalse();
	}

	@Test
	public void isLuceneIndexDestroyableWhenLuceneIndexIsNullReturnsFalse() {

		factoryBean.setDestroy(true);

		assertThat(factoryBean.isDestroy()).isTrue();
		assertThat(factoryBean.isLuceneIndexDestroyable(null)).isFalse();
	}

	@Test
	public void getObjectReturnsLuceneIndexFromLuceneService() throws Exception {

		factoryBean.setCache(mockCache);
		factoryBean.setIndexName("ExampleIndex");
		factoryBean.setLuceneService(mockLuceneService);
		factoryBean.setRegionPath("/Example");

		when(mockLuceneService.getIndex(eq("ExampleIndex"), eq("/Example"))).thenReturn(mockLuceneIndex);

		assertThat(factoryBean.getCache()).isSameAs(mockCache);
		assertThat(factoryBean.getIndexName()).isEqualTo("ExampleIndex");
		assertThat(factoryBean.getLuceneService()).isSameAs(mockLuceneService);
		assertThat(factoryBean.getRegionPath()).isEqualTo("/Example");
		assertThat(factoryBean.getObject()).isEqualTo(mockLuceneIndex);

		verify(mockLuceneService, times(1))
			.getIndex(eq("ExampleIndex"), eq("/Example"));
	}

	@Test
	public void getObjectReturnsExistingLuceneIndex() throws Exception {

		factoryBean.setLuceneIndex(mockLuceneIndex);
		factoryBean.setLuceneService(mockLuceneService);

		assertThat(factoryBean.getObject()).isSameAs(mockLuceneIndex);

		verify(mockLuceneService, never()).getIndex(anyString(), anyString());
	}

	@Test
	public void getObjectReturnsNullWhenLuceneServiceIsNull() throws Exception {

		factoryBean.setCache(mockCache);
		factoryBean.setRegionPath("/Example");

		doReturn(null).when(factoryBean).resolveLuceneService(eq(mockCache));

		assertThat(factoryBean.getCache()).isSameAs(mockCache);
		assertThat(factoryBean.getLuceneService()).isNull();
		assertThat(factoryBean.getRegionPath()).isEqualTo("/Example");
		assertThat(factoryBean.getObject()).isNull();

		verify(factoryBean, times(1)).resolveLuceneService();
		verify(factoryBean, times(1)).resolveLuceneService(eq(mockCache));
	}

	@Test
	public void getObjectTypeBeforeInitialization() {
		assertThat(factoryBean.getObjectType()).isEqualTo(LuceneIndex.class);
	}

	@Test
	public void getObjectTypeAfterInitialization() {
		assertThat(factoryBean.setLuceneIndex(mockLuceneIndex).getObjectType()).isEqualTo(mockLuceneIndex.getClass());
	}

	@Test
	public void isSingletonReturnsTrue() {
		assertThat(factoryBean.isSingleton()).isTrue();
	}

	@Test
	public void resolveCacheReturnsConfiguredCache() {

		factoryBean.setCache(mockCache);

		assertThat(factoryBean.getCache()).isSameAs(mockCache);
		assertThat(factoryBean.resolveCache()).isSameAs(mockCache);
	}

	@Test
	public void resolveFieldsReturnsGivenFields() {

		List<String> expectedFields = Arrays.asList("fieldOne", "fieldTwo");

		assertThat(factoryBean.resolveFields(expectedFields)).isSameAs(expectedFields);
	}

	@Test
	public void resolveFieldsWithEmptyListReturnsAllFields() {
		assertThat(factoryBean.resolveFields(Collections.emptyList()))
			.isEqualTo(Collections.singletonList(LuceneService.REGION_VALUE_FIELD));
	}

	@Test
	public void resolveFieldsWithNullReturnsAllFields() {
		assertThat(factoryBean.resolveFields(null))
			.isEqualTo(Collections.singletonList(LuceneService.REGION_VALUE_FIELD));
	}

	@Test
	public void resolveLuceneIndexFactoryCallsLuceneServiceCreateIndexFactory() {

		factoryBean.setCache(mockCache);

		doReturn(mockLuceneIndexFactory).when(mockLuceneService).createIndexFactory();

		assertThat(factoryBean.resolveLuceneIndexFactory()).isEqualTo(mockLuceneIndexFactory);

		verify(mockLuceneService, times(1)).createIndexFactory();
	}

	@Test
	public void resolveLuceneServiceReturnsConfiguredLuceneService() {

		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.setCache(mockCache);
		factoryBean.setLuceneService(mockLuceneService);

		assertThat(factoryBean.getBeanFactory()).isSameAs(mockBeanFactory);
		assertThat(factoryBean.getCache()).isSameAs(mockCache);
		assertThat(factoryBean.getLuceneService()).isSameAs(mockLuceneService);
		assertThat(factoryBean.resolveLuceneService()).isSameAs(mockLuceneService);

		verifyZeroInteractions(mockBeanFactory);
		verifyZeroInteractions(mockCache);
		verify(factoryBean, never()).resolveLuceneService(any(GemFireCache.class));
	}

	@Test
	public void resolveLuceneServiceFromBeanFactory() {

		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.setCache(mockCache);

		when(mockBeanFactory.getBean(eq(LuceneService.class))).thenReturn(mockLuceneService);

		assertThat(factoryBean.getBeanFactory()).isSameAs(mockBeanFactory);
		assertThat(factoryBean.getCache()).isSameAs(mockCache);
		assertThat(factoryBean.getLuceneService()).isNull();
		assertThat(factoryBean.resolveLuceneService()).isSameAs(mockLuceneService);

		verify(mockBeanFactory, times(1)).getBean(eq(LuceneService.class));
		verifyZeroInteractions(mockCache);
		verify(factoryBean, never()).resolveLuceneService(any(GemFireCache.class));
	}

	@Test
	public void resolveLuceneServiceFromGemFireCache() {

		factoryBean.setCache(mockCache);

		doReturn(mockLuceneService).when(factoryBean).resolveLuceneService(eq(mockCache));

		assertThat(factoryBean.getBeanFactory()).isNull();
		assertThat(factoryBean.getCache()).isSameAs(mockCache);
		assertThat(factoryBean.getLuceneService()).isNull();
		assertThat(factoryBean.resolveLuceneService()).isSameAs(mockLuceneService);

		verifyZeroInteractions(mockCache);
		verify(factoryBean, times(1)).resolveLuceneService(eq(mockCache));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void resolveLuceneServiceFromGemFireCacheWithBeanFactoryThrowinBeansException() {

		factoryBean.setBeanFactory(mockBeanFactory);
		factoryBean.setCache(mockCache);

		when(mockBeanFactory.getBean(any(Class.class))).thenThrow(new NoSuchBeanDefinitionException("test"));
		doReturn(mockLuceneService).when(factoryBean).resolveLuceneService(eq(mockCache));

		assertThat(factoryBean.getBeanFactory()).isSameAs(mockBeanFactory);
		assertThat(factoryBean.getCache()).isSameAs(mockCache);
		assertThat(factoryBean.getLuceneService()).isNull();
		assertThat(factoryBean.resolveLuceneService()).isSameAs(mockLuceneService);

		verify(mockBeanFactory, times(1)).getBean(eq(LuceneService.class));
		verifyZeroInteractions(mockCache);
		verify(factoryBean, times(1)).resolveLuceneService(eq(mockCache));
	}

	@Test(expected = IllegalArgumentException.class)
	public void resolveLuceneServiceThrowsIllegalArgumentExceptionWhenGemFireCacheIsNotConfigured() {

		try {
			factoryBean.resolveLuceneService(null);
		}
		catch (IllegalArgumentException expected) {

			assertThat(expected).hasMessageContaining("A reference to the GemFireCache was not properly configured");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	@SuppressWarnings("all")
	public void resolveRegionReturnsConfiguredRegion() {

		factoryBean.setRegion(mockRegion);

		assertThat(factoryBean.resolveRegion()).isSameAs(mockRegion);

		verify(factoryBean, times(1)).getRegion();
		verify(factoryBean, never()).resolveCache();
		verify(factoryBean, never()).getRegionPath();
	}

	@Test
	@SuppressWarnings("all")
	public void resolveRegionReturnsRegionFromCache() {

		factoryBean.setRegionPath("/Example");

		doReturn(mockCache).when(factoryBean).resolveCache();
		when(mockCache.getRegion(eq("/Example"))).thenReturn(mockRegion);

		assertThat(factoryBean.resolveRegion()).isEqualTo(mockRegion);

		verify(factoryBean, times(1)).getRegion();
		verify(factoryBean, times(1)).resolveCache();
		verify(factoryBean, times(1)).getRegionPath();
		verify(mockCache, times(1)).getRegion(eq("/Example"));
	}

	@Test
	@SuppressWarnings("all")
	public void resolveRegionReturnsNullWhenCacheNotConfigured() {

		factoryBean.setRegionPath("/Example");

		doReturn(null).when(factoryBean).resolveCache();

		assertThat(factoryBean.resolveRegion()).isNull();

		verify(factoryBean, times(1)).getRegion();
		verify(factoryBean, times(1)).resolveCache();
		verify(factoryBean, times(1)).getRegionPath();
	}

	@Test
	@SuppressWarnings("all")
	public void resolveRegionReturnsNullWhenRegionPathNotConfigured() {

		factoryBean.setCache(mockCache);
		factoryBean.setRegionPath("  ");

		assertThat(factoryBean.resolveRegion()).isNull();

		verify(factoryBean, times(1)).getRegion();
		verify(factoryBean, times(1)).resolveCache();
		verify(factoryBean, times(1)).getRegionPath();
		verify(mockCache, never()).getRegion(anyString());
	}

	@Test
	@SuppressWarnings("all")
	public void resolveRegionPathReturnsConfiguredRegionPath() {

		factoryBean.setRegionPath("/Example");

		doReturn(null).when(factoryBean).resolveRegion();

		assertThat(factoryBean.resolveRegionPath()).isEqualTo("/Example");

		verify(factoryBean, times(1)).resolveRegion();
		verify(factoryBean, times(1)).getRegionPath();
	}

	@Test
	@SuppressWarnings("all")
	public void resolveRegionPathReturnsRegionFullPath() {

		doReturn(mockRegion).when(factoryBean).resolveRegion();
		when(mockRegion.getFullPath()).thenReturn("/Example");

		assertThat(factoryBean.resolveRegionPath()).isEqualTo("/Example");

		verify(factoryBean, times(1)).resolveRegion();
		verify(factoryBean, never()).getRegionPath();
		verify(mockRegion, times(1)).getFullPath();
	}

	@Test(expected = IllegalStateException.class)
	@SuppressWarnings("all")
	public void resolveRegionPathThrowsIllegalStateException() {

		doReturn(null).when(factoryBean).resolveRegion();

		factoryBean.setRegionPath(null);

		try {
			factoryBean.resolveRegionPath();
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessageContaining("Either Region or regionPath must be specified");
			assertThat(expected).hasNoCause();

			throw expected;
		}
		finally {
			verify(factoryBean, times(1)).resolveRegion();
			verify(factoryBean, times(1)).getRegionPath();
		}
	}

	@Test
	public void setAndGetFieldAnalyzers() {

		Map<String, Analyzer> fieldAnalyzers = factoryBean.getFieldAnalyzers();

		assertThat(fieldAnalyzers).isNotNull();
		assertThat(fieldAnalyzers).isEmpty();

		fieldAnalyzers = Collections.singletonMap("mockField", mockAnalyzer);
		factoryBean.setFieldAnalyzers(fieldAnalyzers);

		assertThat(factoryBean.getFieldAnalyzers()).isEqualTo(fieldAnalyzers);

		factoryBean.setFieldAnalyzers(null);
		fieldAnalyzers = factoryBean.getFieldAnalyzers();

		assertThat(fieldAnalyzers).isNotNull();
		assertThat(fieldAnalyzers).isEmpty();
	}

	@Test
	public void setAndGetFields() {

		List<String> fields = factoryBean.getFields();

		assertThat(fields).isNotNull();
		assertThat(fields).isEmpty();

		factoryBean.setFields(Arrays.asList("fieldOne", "fieldTwo"));

		assertThat(factoryBean.getFields()).containsAll(Arrays.asList("fieldOne", "fieldTwo"));

		factoryBean.setFields((String[]) null);
		fields = factoryBean.getFields();

		assertThat(fields).isNotNull();
		assertThat(fields).isEmpty();
	}

	@Test
	public void setAndGetIndexName() {

		factoryBean.setBeanName("IndexOne");

		assertThat(factoryBean.getIndexName()).isEqualTo("IndexOne");

		factoryBean.setIndexName("IndexTwo");

		assertThat(factoryBean.getIndexName()).isEqualTo("IndexTwo");

		factoryBean.setIndexName(null);
	}

	@Test
	public void setAndGetLuceneSerializer() {

		assertThat(factoryBean.getLuceneSerializer()).isNull();

		factoryBean.setLuceneSerializer(mockLuceneSerializer);

		assertThat(factoryBean.getLuceneSerializer()).isEqualTo(mockLuceneSerializer);
	}

	@Test(expected = IllegalStateException.class)
	public void getUninitializedIndexName() {

		try {
			factoryBean.getIndexName();
		}
		catch (IllegalStateException expected) {

			assertThat(expected).hasMessageContaining("indexName was not properly initialized");
			assertThat(expected).hasNoCause();

			throw expected;
		}
	}

	@Test
	public void factoryBeanInitializationIsSuccessful() {

		factoryBean.setCache(mockCache);
		factoryBean.setDestroy(true);
		factoryBean.setFieldAnalyzers(Collections.singletonMap("fieldThree", mockAnalyzer));
		factoryBean.setFields("fieldOne", "fieldTwo");
		factoryBean.setIndexName("TestIndex");
		factoryBean.setLuceneService(mockLuceneService);
		factoryBean.setRegion(mockRegion);
		factoryBean.setRegionPath("/Grandparent/Parent/Child");

		assertThat(factoryBean.getCache()).isSameAs(mockCache);
		assertThat(factoryBean.isDestroy()).isTrue();
		assertThat(factoryBean.getFieldAnalyzers()).isEqualTo(Collections.singletonMap("fieldThree", mockAnalyzer));
		assertThat(factoryBean.getFields()).containsAll(Arrays.asList("fieldOne", "fieldTwo"));
		assertThat(factoryBean.getIndexName()).isEqualTo("TestIndex");
		assertThat(factoryBean.getLuceneService()).isSameAs(mockLuceneService);
		assertThat(factoryBean.getRegion()).isSameAs(mockRegion);
		assertThat(factoryBean.getRegionPath()).isEqualTo("/Grandparent/Parent/Child");
	}
}
