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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;

import org.apache.geode.pdx.PdxSerializer;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.core.convert.ConversionService;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.MappingPdxSerializer;
import org.springframework.util.MethodInvoker;

/**
 * Unit tests for {@link AbstractCacheConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.Spy
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.data.gemfire.config.annotation.AbstractCacheConfiguration
 * @since 1.9.0
 */
@RunWith(MockitoJUnitRunner.class)
public class AbstractCacheConfigurationUnitTests {

	@Mock
	private BeanFactory mockBeanFactory;

	private TestCacheConfiguration cacheConfiguration;

	@Before
	public void setup() {
		cacheConfiguration = new TestCacheConfiguration();
	}

	@SuppressWarnings("unchecked")
	private <T> T invokeMethod(Object obj, String methodName) throws Exception {

		MethodInvoker methodInvoker = new MethodInvoker();

		methodInvoker.setTargetObject(obj);
		methodInvoker.setTargetMethod(methodName);
		methodInvoker.prepare();

		return (T) methodInvoker.invoke();
	}

	@Test
	public void configurePdxWhenEnablePdxIsConfigured() {

		AnnotationMetadata mockAnnotationMetadata = mock(AnnotationMetadata.class);
		PdxSerializer mockPdxSerializer = mock(PdxSerializer.class);

		Map<String, Object> annotationAttributes = new HashMap<>(5);

		annotationAttributes.put("diskStoreName", "BlockDiskStore");
		annotationAttributes.put("ignoreUnreadFields", Boolean.FALSE);
		annotationAttributes.put("persistent", Boolean.TRUE);
		annotationAttributes.put("readSerialized", Boolean.TRUE);
		annotationAttributes.put("serializerBeanName", "MockPdxSerializer");

		when(mockAnnotationMetadata.hasAnnotation(eq(EnablePdx.class.getName()))).thenReturn(true);
		when(mockAnnotationMetadata.getAnnotationAttributes(eq(EnablePdx.class.getName()))).thenReturn(annotationAttributes);
		when(mockBeanFactory.containsBean(eq("MockPdxSerializer"))).thenReturn(true);
		when(mockBeanFactory.getBean(eq("MockPdxSerializer"), eq(PdxSerializer.class))).thenReturn(mockPdxSerializer);

		cacheConfiguration.setBeanFactory(mockBeanFactory);
		cacheConfiguration.configurePdx(mockAnnotationMetadata);

		assertThat(cacheConfiguration.getPdxDiskStoreName()).isEqualTo("BlockDiskStore");
		assertThat(cacheConfiguration.getPdxIgnoreUnreadFields()).isFalse();
		assertThat(cacheConfiguration.getPdxPersistent()).isTrue();
		assertThat(cacheConfiguration.getPdxReadSerialized()).isTrue();
		assertThat(cacheConfiguration.getPdxSerializer()).isEqualTo(mockPdxSerializer);

		verify(mockAnnotationMetadata, times(1)).hasAnnotation(eq(EnablePdx.class.getName()));
		verify(mockAnnotationMetadata, times(1)).getAnnotationAttributes(eq(EnablePdx.class.getName()));
		verify(mockBeanFactory, times(1)).containsBean(eq("MockPdxSerializer"));
		verify(mockBeanFactory, times(1)).getBean(eq("MockPdxSerializer"), eq(PdxSerializer.class));
	}

	@Test
	public void configurePdxWhenEnablePdxIsNotConfigured() {

		AnnotationMetadata mockAnnotationMetadata = mock(AnnotationMetadata.class);

		when(mockAnnotationMetadata.hasAnnotation(anyString())).thenReturn(false);

		cacheConfiguration.configurePdx(mockAnnotationMetadata);

		assertThat(cacheConfiguration.getPdxDiskStoreName()).isNull();
		assertThat(cacheConfiguration.getPdxIgnoreUnreadFields()).isNull();
		assertThat(cacheConfiguration.getPdxPersistent()).isNull();
		assertThat(cacheConfiguration.getPdxReadSerialized()).isNull();
		assertThat(cacheConfiguration.getPdxSerializer()).isNull();

		verify(mockAnnotationMetadata, times(1)).hasAnnotation(eq(EnablePdx.class.getName()));
		verifyNoMoreInteractions(mockAnnotationMetadata);
	}

	@Test
	public void resolvePdxSerializerUsesPdxSerializerBean() {

		PdxSerializer mockPdxSerializer = mock(PdxSerializer.class);

		when(mockBeanFactory.containsBean(anyString())).thenReturn(true);
		when(mockBeanFactory.getBean(anyString(), eq(PdxSerializer.class))).thenReturn(mockPdxSerializer);

		cacheConfiguration.setBeanFactory(mockBeanFactory);

		PdxSerializer actualPdxSerializer = cacheConfiguration.resolvePdxSerializer("MockPdxSerializer");

		assertThat(actualPdxSerializer).isEqualTo(mockPdxSerializer);

		verify(mockBeanFactory, times(1)).containsBean(eq("MockPdxSerializer"));
		verify(mockBeanFactory, times(1)).getBean(eq("MockPdxSerializer"), eq(PdxSerializer.class));
	}

	@Test
	public void resolvePdxSerializerUsesConfiguredPdxSerializer() {

		PdxSerializer mockPdxSerializer = mock(PdxSerializer.class);

		when(mockBeanFactory.containsBean(anyString())).thenReturn(false);

		cacheConfiguration.setBeanFactory(mockBeanFactory);
		cacheConfiguration.setPdxSerializer(mockPdxSerializer);

		PdxSerializer actualPdxSerializer = cacheConfiguration.resolvePdxSerializer("TestPdxSerializer");

		assertThat(actualPdxSerializer).isEqualTo(mockPdxSerializer);

		verify(mockBeanFactory, times(1)).containsBean(eq("TestPdxSerializer"));
		verify(mockBeanFactory, never()).getBean(anyString(), eq(PdxSerializer.class));
		verifyZeroInteractions(mockPdxSerializer);
	}

	@Test
	public void resolvePdxSerializerCallsNewMappingPdxSerializer() {

		AbstractCacheConfiguration cacheConfigurationSpy = spy(this.cacheConfiguration);
		MappingPdxSerializer mockPdxSerializer = mock(MappingPdxSerializer.class);

		when(mockBeanFactory.containsBean(anyString())).thenReturn(false);
		doReturn(mockPdxSerializer).when(cacheConfigurationSpy).newPdxSerializer(any(BeanFactory.class));

		cacheConfigurationSpy.setBeanFactory(mockBeanFactory);

		PdxSerializer actualPdxSerializer = cacheConfigurationSpy.resolvePdxSerializer("TestPdxSerializer");

		assertThat(actualPdxSerializer).isEqualTo(mockPdxSerializer);

		verify(mockBeanFactory, times(1)).containsBean(eq("TestPdxSerializer"));
		verify(mockBeanFactory, never()).getBean(anyString(), eq(PdxSerializer.class));
		verify(cacheConfigurationSpy, times(1)).newPdxSerializer(eq(mockBeanFactory));
	}

	@Test
	public void newPdxSerializerUsesConfiguredConversionServiceAndMappingContext() throws Exception {

		ConfigurableBeanFactory mockBeanFactory = mock(ConfigurableBeanFactory.class);
		ConversionService mockConversionService = mock(ConversionService.class);
		GemfireMappingContext mockMappingContext = mock(GemfireMappingContext.class);

		when(mockBeanFactory.getConversionService()).thenReturn(mockConversionService);

		cacheConfiguration.setBeanFactory(mockBeanFactory);
		cacheConfiguration.setMappingContext(mockMappingContext);

		MappingPdxSerializer pdxSerializer = cacheConfiguration.newPdxSerializer();

		assertThat(pdxSerializer).isNotNull();
		assertThat((Object) invokeMethod(pdxSerializer, "getConversionService")).isEqualTo(mockConversionService);
		assertThat((Object) invokeMethod(pdxSerializer, "getMappingContext")).isEqualTo(mockMappingContext);

		verify(mockBeanFactory, times(2)).getConversionService();
		verifyZeroInteractions(mockConversionService);
		verifyZeroInteractions(mockMappingContext);
	}

	@Test
	public void newPdxSerializerDefaultsConversionServiceAndMappingContextWhenNotConfigured() throws Exception {

		cacheConfiguration.setBeanFactory(mockBeanFactory);

		MappingPdxSerializer pdxSerializer = cacheConfiguration.newPdxSerializer();

		assertThat(pdxSerializer).isNotNull();
		assertThat((Object) invokeMethod(pdxSerializer, "getConversionService")).isInstanceOf(ConversionService.class);
		assertThat((Object) invokeMethod(pdxSerializer, "getMappingContext")).isInstanceOf(GemfireMappingContext.class);
	}

	protected static class TestCacheConfiguration extends AbstractCacheConfiguration {

		@Override
		protected Class getAnnotationType() {
			throw new UnsupportedOperationException("Not Implemented");
		}

		@Override
		protected <T extends CacheFactoryBean> T newCacheFactoryBean() {
			throw new UnsupportedOperationException("Not Implemented");
		}
	}
}
