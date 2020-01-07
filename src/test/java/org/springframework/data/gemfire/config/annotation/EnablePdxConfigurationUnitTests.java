/*
 * Copyright 2018-2020 the original author or authors.
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
 */

package org.springframework.data.gemfire.config.annotation;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verifyZeroInteractions;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.apache.geode.pdx.PdxSerializer;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.core.convert.ConversionService;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.CacheFactoryBean;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.MappingPdxSerializer;
import org.springframework.util.MethodInvoker;

/**
 * Unit tests for {@link EnablePdx} and {@link PdxConfiguration}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mock
 * @see org.mockito.Spy
 * @see org.apache.geode.pdx.PdxSerializer
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.data.gemfire.CacheFactoryBean
 * @see org.springframework.data.gemfire.config.annotation.EnablePdx
 * @see org.springframework.data.gemfire.config.annotation.PdxConfiguration
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class EnablePdxConfigurationUnitTests {

	@Spy
	private PdxConfiguration pdxConfiguration;

	@SuppressWarnings("unchecked")
	private <T> T invokeMethod(Object obj, String methodName) throws Exception {

		MethodInvoker methodInvoker = new MethodInvoker();

		methodInvoker.setTargetObject(obj);
		methodInvoker.setTargetMethod(methodName);
		methodInvoker.prepare();

		return (T) methodInvoker.invoke();
	}

	@Test
	public void setImportMetadataWhenEnablePdxIsConfigured() {

		AnnotationMetadata mockAnnotationMetadata = mock(AnnotationMetadata.class);

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		Map<String, Object> annotationAttributes = new HashMap<>(5);

		annotationAttributes.put("diskStoreName", "MockDiskStore");
		annotationAttributes.put("ignoreUnreadFields", Boolean.TRUE);
		annotationAttributes.put("persistent", Boolean.TRUE);
		annotationAttributes.put("readSerialized", Boolean.TRUE);
		annotationAttributes.put("serializerBeanName", "MockPdxSerializer");

		when(mockAnnotationMetadata.hasAnnotation(eq(EnablePdx.class.getName()))).thenReturn(true);
		when(mockAnnotationMetadata.getAnnotationAttributes(eq(EnablePdx.class.getName())))
			.thenReturn(annotationAttributes);

		this.pdxConfiguration.setBeanFactory(mockBeanFactory);
		this.pdxConfiguration.setImportMetadata(mockAnnotationMetadata);

		assertThat(this.pdxConfiguration.getBeanFactory()).isEqualTo(mockBeanFactory);
		assertThat(this.pdxConfiguration.getDiskStoreName().orElse(null)).isEqualTo("MockDiskStore");
		assertThat(this.pdxConfiguration.isIgnoreUnreadFields()).isTrue();
		assertThat(this.pdxConfiguration.isPersistent()).isTrue();
		assertThat(this.pdxConfiguration.isReadSerialized()).isTrue();
		assertThat(this.pdxConfiguration.getSerializerBeanName().orElse(null)).isEqualTo("MockPdxSerializer");

		verify(mockAnnotationMetadata, times(1)).hasAnnotation(eq(EnablePdx.class.getName()));
		verify(mockAnnotationMetadata, times(1))
			.getAnnotationAttributes(eq(EnablePdx.class.getName()));
	}

	@Test
	public void setImportMetadataWhenEnablePdxIsNotConfigured() {

		AnnotationMetadata mockAnnotationMetadata = mock(AnnotationMetadata.class);

		when(mockAnnotationMetadata.hasAnnotation(anyString())).thenReturn(false);

		this.pdxConfiguration.setImportMetadata(mockAnnotationMetadata);

		assertThat(this.pdxConfiguration.getDiskStoreName().isPresent()).isFalse();
		assertThat(this.pdxConfiguration.isIgnoreUnreadFields()).isFalse();
		assertThat(this.pdxConfiguration.isPersistent()).isFalse();
		assertThat(this.pdxConfiguration.isReadSerialized()).isFalse();
		assertThat(this.pdxConfiguration.getSerializerBeanName().isPresent()).isFalse();

		verify(mockAnnotationMetadata, times(1)).hasAnnotation(eq(EnablePdx.class.getName()));
		verifyNoMoreInteractions(mockAnnotationMetadata);
	}

	@Test
	public void configuresPdxForCacheFactoryBean() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		PdxSerializer mockPdxSerializer = mock(PdxSerializer.class);

		when(mockBeanFactory.containsBean(eq("MockPdxSerializer"))).thenReturn(true);
		when(mockBeanFactory.getBean(eq("MockPdxSerializer"), eq(PdxSerializer.class)))
			.thenReturn(mockPdxSerializer);

		doReturn(Optional.of("MockPdxDiskStore")).when(this.pdxConfiguration).getDiskStoreName();
		doReturn(true).when(this.pdxConfiguration).isIgnoreUnreadFields();
		doReturn(true).when(this.pdxConfiguration).isPersistent();
		doReturn(true).when(this.pdxConfiguration).isReadSerialized();
		doReturn(Optional.of("MockPdxSerializer")).when(this.pdxConfiguration).getSerializerBeanName();

		CacheFactoryBean cacheFactoryBean = new CacheFactoryBean();

		this.pdxConfiguration.setBeanFactory(mockBeanFactory);
		this.pdxConfiguration.configurePdx(cacheFactoryBean);

		assertThat(cacheFactoryBean.getPdxDiskStoreName()).isEqualTo("MockPdxDiskStore");
		assertThat(cacheFactoryBean.getPdxIgnoreUnreadFields()).isTrue();
		assertThat(cacheFactoryBean.getPdxPersistent()).isTrue();
		assertThat(cacheFactoryBean.getPdxReadSerialized()).isTrue();
		assertThat(cacheFactoryBean.getPdxSerializer()).isEqualTo(mockPdxSerializer);

		verify(mockBeanFactory, times(1)).containsBean(eq("MockPdxSerializer"));
		verify(mockBeanFactory, times(1))
			.getBean(eq("MockPdxSerializer"), eq(PdxSerializer.class));

		verify(this.pdxConfiguration, times(1)).getDiskStoreName();
		verify(this.pdxConfiguration, times(1)).isIgnoreUnreadFields();
		verify(this.pdxConfiguration, times(1)).isPersistent();
		verify(this.pdxConfiguration, times(1)).isReadSerialized();
		verify(this.pdxConfiguration, times(1)).getSerializerBeanName();
	}

	@Test
	public void resolveConversionServiceWithNonConfigurableBeanFactoryReturnsNull() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		this.pdxConfiguration.setBeanFactory(mockBeanFactory);

		assertThat(this.pdxConfiguration.getBeanFactory()).isEqualTo(mockBeanFactory);
		assertThat(this.pdxConfiguration.resolveConversionService().isPresent()).isFalse();

		verifyZeroInteractions(mockBeanFactory);
	}

	@Test
	public void resolveConversionServiceReturnsConfiguredConversionService() {

		ConfigurableBeanFactory mockBeanFactory = mock(ConfigurableBeanFactory.class);

		ConversionService mockConversionService = mock(ConversionService.class);

		when(mockBeanFactory.getConversionService()).thenReturn(mockConversionService);

		this.pdxConfiguration.setBeanFactory(mockBeanFactory);

		assertThat(this.pdxConfiguration.getBeanFactory()).isEqualTo(mockBeanFactory);
		assertThat(this.pdxConfiguration.resolveConversionService().orElse(null))
			.isEqualTo(mockConversionService);

 		verify(mockBeanFactory, atLeastOnce()).getConversionService();
	}

	@Test
	@SuppressWarnings("unchecked")
	public void resolveMappingContextWhenBeanFactoryDoesNotContainMappingContextBeanReturnsNull() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		when(mockBeanFactory.getBean(any(Class.class))).thenThrow(new NoSuchBeanDefinitionException("test"));

		this.pdxConfiguration.setBeanFactory(mockBeanFactory);

		assertThat(this.pdxConfiguration.getBeanFactory()).isEqualTo(mockBeanFactory);
		assertThat(this.pdxConfiguration.resolveMappingContext().isPresent()).isFalse();

		verify(mockBeanFactory, times(1)).getBean(eq(GemfireMappingContext.class));
	}

	@Test
	public void resolveMappingContextReturnsConfiguredMappingContext() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		GemfireMappingContext mockMappingContext = mock(GemfireMappingContext.class);

		when(mockBeanFactory.getBean(eq(GemfireMappingContext.class))).thenReturn(mockMappingContext);

		this.pdxConfiguration.setBeanFactory(mockBeanFactory);

		assertThat(this.pdxConfiguration.getBeanFactory()).isEqualTo(mockBeanFactory);
		assertThat(this.pdxConfiguration.resolveMappingContext().orElse(null)).isEqualTo(mockMappingContext);

		verify(mockBeanFactory, times(1)).getBean(eq(GemfireMappingContext.class));
	}

	@Test
	public void resolvePdxSerializerWhenBeanFactoryDoesNotContainPdxSerializerBeanReturnsMappingPdxSerializer() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		when(mockBeanFactory.containsBean(anyString())).thenReturn(false);

		this.pdxConfiguration.setBeanFactory(mockBeanFactory);
		this.pdxConfiguration.setSerializerBeanName("MockPdxSerializer");

		assertThat(this.pdxConfiguration.getBeanFactory()).isEqualTo(mockBeanFactory);
		assertThat(this.pdxConfiguration.getSerializerBeanName().orElse(null)).isEqualTo("MockPdxSerializer");
		assertThat(this.pdxConfiguration.resolvePdxSerializer()).isInstanceOf(MappingPdxSerializer.class);

		verify(mockBeanFactory, times(1)).containsBean(eq("MockPdxSerializer"));
		verify(mockBeanFactory, never()).getBean(anyString(), any(PdxSerializer.class));
	}

	@Test
	public void resolvePdxSerializerReturnsConfiguredPdxSerializer() {

		BeanFactory mockBeanFactory = mock(BeanFactory.class);

		PdxSerializer mockPdxSerializer = mock(PdxSerializer.class);

		when(mockBeanFactory.containsBean(eq("MockPdxSerializer"))).thenReturn(true);
		when(mockBeanFactory.getBean(eq("MockPdxSerializer"), eq(PdxSerializer.class)))
			.thenReturn(mockPdxSerializer);

		this.pdxConfiguration.setBeanFactory(mockBeanFactory);
		this.pdxConfiguration.setSerializerBeanName("MockPdxSerializer");

		assertThat(this.pdxConfiguration.getBeanFactory()).isEqualTo(mockBeanFactory);
		assertThat(this.pdxConfiguration.getSerializerBeanName().orElse(null)).isEqualTo("MockPdxSerializer");
		assertThat(this.pdxConfiguration.resolvePdxSerializer()).isEqualTo(mockPdxSerializer);

		verify(mockBeanFactory, times(1)).containsBean(eq("MockPdxSerializer"));
		verify(mockBeanFactory, times(1))
			.getBean(eq("MockPdxSerializer"), eq(PdxSerializer.class));
	}

	@Test
	public void newPdxSerializerReturnsMappingPdxSerializerConfiguredWithResolvedConversionServiceAndMappingContext()
			throws Exception {

		ConfigurableBeanFactory mockBeanFactory = mock(ConfigurableBeanFactory.class);

		ConversionService mockConversionService = mock(ConversionService.class);

		GemfireMappingContext mockMappingContext = mock(GemfireMappingContext.class);

		when(mockBeanFactory.getBean(eq(GemfireMappingContext.class))).thenReturn(mockMappingContext);
		when(mockBeanFactory.getConversionService()).thenReturn(mockConversionService);

		this.pdxConfiguration.setBeanFactory(mockBeanFactory);

		PdxSerializer pdxSerializer = this.pdxConfiguration.newPdxSerializer();

		assertThat(pdxSerializer).isInstanceOf(MappingPdxSerializer.class);

		assertThat(this.<Object>invokeMethod(pdxSerializer, "getConversionService"))
			.isEqualTo(mockConversionService);

		assertThat(this.<Object>invokeMethod(pdxSerializer, "getMappingContext"))
			.isEqualTo(mockMappingContext);

		verify(mockBeanFactory, times(1)).getBean(eq(GemfireMappingContext.class));
		verify(mockBeanFactory, atLeastOnce()).getConversionService();
	}
}
