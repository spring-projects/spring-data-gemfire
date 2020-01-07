/*
 * Copyright 2012-2020 the original author or authors.
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

package org.springframework.data.gemfire.repository.config;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.annotation.Annotation;
import java.util.Collection;
import java.util.Optional;

import org.junit.Test;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import org.springframework.beans.PropertyValue;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.parsing.PassThroughSourceExtractor;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.xml.BeanDefinitionParserDelegate;
import org.springframework.beans.factory.xml.ParserContext;
import org.springframework.beans.factory.xml.XmlBeanDefinitionReader;
import org.springframework.beans.factory.xml.XmlReaderContext;
import org.springframework.core.env.Environment;
import org.springframework.core.io.ResourceLoader;
import org.springframework.data.gemfire.mapping.annotation.Region;
import org.springframework.data.gemfire.repository.GemfireRepository;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean;
import org.springframework.data.repository.config.AnnotationRepositoryConfigurationSource;
import org.springframework.data.repository.config.XmlRepositoryConfigurationSource;

/**
 * Unit tests for {@link GemfireRepositoryConfigurationExtension}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.repository.config.GemfireRepositoryConfigurationExtension
 * @since 1.6.3
 */
public class GemfireRepositoryConfigurationExtensionTest {

	private GemfireRepositoryConfigurationExtension repositoryConfigurationExtension =
		new GemfireRepositoryConfigurationExtension();

	protected Object getPropertyValue(BeanDefinitionBuilder builder, String propertyName) {
		return getPropertyValue(builder.getRawBeanDefinition(), propertyName);
	}

	protected Object getPropertyValue(BeanDefinition beanDefinition, String propertyName) {
		PropertyValue propertyValue = beanDefinition.getPropertyValues().getPropertyValue(propertyName);

		return (propertyValue != null ? propertyValue.getValue() : null);
	}

	protected Element mockElement() {
		Element mockElement = mock(Element.class);
		NodeList mockNodeList = mock(NodeList.class);

		when(mockNodeList.getLength()).thenReturn(0);
		when(mockElement.getChildNodes()).thenReturn(mockNodeList);

		return mockElement;
	}

	protected Environment mockEnvironment() {
		return mock(Environment.class);
	}

	protected ParserContext mockParserContext() {

		XmlReaderContext xmlReaderContext = mockXmlReaderContext();

		return new ParserContext(xmlReaderContext, newBeanDefinitionParserDelegate(xmlReaderContext));
	}

	protected XmlReaderContext mockXmlReaderContext() {

		BeanDefinitionRegistry mockRegistry = mock(BeanDefinitionRegistry.class);

		ResourceLoader mockResourceLoader = mock(ResourceLoader.class);

		XmlBeanDefinitionReader beanDefinitionReader = spy(new XmlBeanDefinitionReader(mockRegistry));

		when(mockResourceLoader.getClassLoader()).thenReturn(Thread.currentThread().getContextClassLoader());
		when(beanDefinitionReader.getResourceLoader()).thenReturn(mockResourceLoader);

		return new XmlReaderContext(null, null, null,
			new PassThroughSourceExtractor(), beanDefinitionReader, null);
	}

	protected BeanDefinitionParserDelegate newBeanDefinitionParserDelegate(XmlReaderContext readerContext) {
		return new BeanDefinitionParserDelegate(readerContext);
	}

	@Test
	public void identifyingAnnotationsIncludesRegionAnnotation() {
		Collection<Class<? extends Annotation>> identifyingAnnotations =
			repositoryConfigurationExtension.getIdentifyingAnnotations();

		assertThat(identifyingAnnotations, is(notNullValue(Collection.class)));
		assertThat(identifyingAnnotations.contains(Region.class), is(true));
	}

	@Test
	public void identifyingTypesContainsGemfireRepositoryAnnotation() {
		Collection<Class<?>> identifyingTypes = repositoryConfigurationExtension.getIdentifyingTypes();

		assertThat(identifyingTypes, is(notNullValue(Collection.class)));
		assertThat(identifyingTypes.contains(GemfireRepository.class), is(true));
	}

	@Test
	public void modulePrefixIsGemFire() {
		assertThat(repositoryConfigurationExtension.getModulePrefix(), is(equalTo("gemfire")));
	}

	@Test
	public void repositoryFactoryClassNameIsGemfireRepositoryFactoryBean() {
		assertThat(repositoryConfigurationExtension.getRepositoryFactoryBeanClassName(),
			is(equalTo(GemfireRepositoryFactoryBean.class.getName())));
	}

	@Test
	public void postProcessWithAnnotationRepositoryConfigurationSource() {
		AnnotationRepositoryConfigurationSource mockRepositoryConfigurationSource =
			mock(AnnotationRepositoryConfigurationSource.class);

		when(mockRepositoryConfigurationSource.getAttribute(eq("mappingContextRef")))
			.thenReturn(Optional.of("testMappingContext"));

		BeanDefinitionBuilder beanDefinitionBuilder = BeanDefinitionBuilder.genericBeanDefinition();

		repositoryConfigurationExtension.postProcess(beanDefinitionBuilder, mockRepositoryConfigurationSource);

		Object mappingContextRef = getPropertyValue(beanDefinitionBuilder, "gemfireMappingContext");

		assertThat(mappingContextRef, is(instanceOf(RuntimeBeanReference.class)));
		assertThat(((RuntimeBeanReference) mappingContextRef).getBeanName(), is(equalTo("testMappingContext")));

		verify(mockRepositoryConfigurationSource, times(1)).getAttribute(eq("mappingContextRef"));
	}

	@Test
	public void postProcessWithAnnotationRepositoryConfigurationSourceHavingNoMappingContextRefAttribute() {
		AnnotationRepositoryConfigurationSource mockRepositoryConfigurationSource =
			mock(AnnotationRepositoryConfigurationSource.class);

		when(mockRepositoryConfigurationSource.getAttribute(eq("mappingContextRef")))
			.thenReturn(Optional.empty());

		BeanDefinitionBuilder beanDefinitionBuilder = BeanDefinitionBuilder.genericBeanDefinition();

		repositoryConfigurationExtension.postProcess(beanDefinitionBuilder, mockRepositoryConfigurationSource);

		Object mappingContextRef = getPropertyValue(beanDefinitionBuilder, "gemfireMappingContext");

		assertThat(mappingContextRef, is(instanceOf(RuntimeBeanReference.class)));
		assertThat(((RuntimeBeanReference) mappingContextRef).getBeanName(),
			is(equalTo(GemfireRepositoryConfigurationExtension.DEFAULT_MAPPING_CONTEXT_BEAN_NAME)));

		verify(mockRepositoryConfigurationSource, times(1)).getAttribute(eq("mappingContextRef"));
	}

	@Test
	public void postProcessWithXmlRepositoryConfigurationSource() {

		Element mockElement = mockElement();

		when(mockElement.getAttribute(eq("mapping-context-ref"))).thenReturn("testMappingContext");

		XmlRepositoryConfigurationSource repositoryConfigurationSource = new XmlRepositoryConfigurationSource(
			mockElement, mockParserContext(), mockEnvironment());

		BeanDefinitionBuilder beanDefinitionBuilder = BeanDefinitionBuilder.genericBeanDefinition();

		repositoryConfigurationExtension.postProcess(beanDefinitionBuilder, repositoryConfigurationSource);

		Object mappingContextRef = getPropertyValue(beanDefinitionBuilder, "gemfireMappingContext");

		assertThat(mappingContextRef, is(instanceOf(RuntimeBeanReference.class)));
		assertThat(((RuntimeBeanReference) mappingContextRef).getBeanName(), is(equalTo("testMappingContext")));

		verify(mockElement, times(1)).getAttribute(eq("mapping-context-ref"));
	}

	@Test
	public void postProcessWithXmlRepositoryConfigurationSourceHavingNoMappingContextRefAttribute() {
		Element mockElement = mockElement();

		when(mockElement.getAttribute(eq("mapping-context-ref"))).thenReturn(null);

		XmlRepositoryConfigurationSource repositoryConfigurationSource = new XmlRepositoryConfigurationSource(
			mockElement, mockParserContext(), mockEnvironment());

		BeanDefinitionBuilder beanDefinitionBuilder = BeanDefinitionBuilder.genericBeanDefinition();

		repositoryConfigurationExtension.postProcess(beanDefinitionBuilder, repositoryConfigurationSource);

		Object mappingContextRef = getPropertyValue(beanDefinitionBuilder, "gemfireMappingContext");

		assertThat(mappingContextRef, is(instanceOf(RuntimeBeanReference.class)));
		assertThat(((RuntimeBeanReference) mappingContextRef).getBeanName(),
			is(equalTo(GemfireRepositoryConfigurationExtension.DEFAULT_MAPPING_CONTEXT_BEAN_NAME)));

		verify(mockElement, times(1)).getAttribute(eq("mapping-context-ref"));
	}
}
