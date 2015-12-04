/*
 * Copyright 2012 the original author or authors.
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
 */

package org.springframework.data.gemfire.repository.config;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean;
import org.springframework.data.repository.config.AnnotationRepositoryConfigurationSource;
import org.springframework.data.repository.config.XmlRepositoryConfigurationSource;
import org.w3c.dom.Element;

/**
 * The GemfireRepositoryConfigurationExtensionTest class is a test suite of test cases testing the contract
 * and functionality of the GemfireRepositoryConfigurationExtension class.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.springframework.data.gemfire.repository.config.GemfireRepositoryConfigurationExtension
 * @since 1.6.3
 */
public class GemfireRepositoryConfigurationExtensionTest {

	private GemfireRepositoryConfigurationExtension repositoryConfigurationExtension;

	@Before
	public void setup() {
		repositoryConfigurationExtension = new GemfireRepositoryConfigurationExtension();
	}

	@Test
	public void repositoryFactoryClassNameIsGemfireRepositoryFactoryBean() {
		assertThat(repositoryConfigurationExtension.getRepositoryFactoryClassName(),
			is(equalTo(GemfireRepositoryFactoryBean.class.getName())));
	}

	@Test
	public void modulePrefixIsGemFire() {
		assertThat(repositoryConfigurationExtension.getModulePrefix(), is(equalTo("gemfire")));
	}

	@Test
	public void postProcessWithAnnotationRepositoryConfigurationSource() {
		AnnotationRepositoryConfigurationSource mockRepositoryConfigurationSource =
			mock(AnnotationRepositoryConfigurationSource.class);

		when(mockRepositoryConfigurationSource.getAttribute(eq("mappingContextRef"))).thenReturn("testMappingContext");

		BeanDefinitionBuilder beanDefinitionBuilder = BeanDefinitionBuilder.genericBeanDefinition();

		repositoryConfigurationExtension.postProcess(beanDefinitionBuilder, mockRepositoryConfigurationSource);

		Object mappingContextRef = beanDefinitionBuilder.getRawBeanDefinition().getPropertyValues()
			.getPropertyValue("gemfireMappingContext").getValue();

		assertThat(mappingContextRef, is(instanceOf(RuntimeBeanReference.class)));
		assertThat(((RuntimeBeanReference) mappingContextRef).getBeanName(), is(equalTo("testMappingContext")));

		verify(mockRepositoryConfigurationSource, times(1)).getAttribute(eq("mappingContextRef"));
	}

	@Test
	public void postProcessWithAnnotationRepositoryConfigurationSourceHaingNoMappingContextRefAttribute() {
		AnnotationRepositoryConfigurationSource mockRepositoryConfigurationSource =
			mock(AnnotationRepositoryConfigurationSource.class);

		when(mockRepositoryConfigurationSource.getAttribute(eq("mappingContextRef"))).thenReturn("  ");

		BeanDefinitionBuilder beanDefinitionBuilder = BeanDefinitionBuilder.genericBeanDefinition();

		repositoryConfigurationExtension.postProcess(beanDefinitionBuilder, mockRepositoryConfigurationSource);

		Object mappingContextRef = beanDefinitionBuilder.getRawBeanDefinition().getPropertyValues()
			.getPropertyValue("gemfireMappingContext");

		assertThat(mappingContextRef, is(nullValue()));

		verify(mockRepositoryConfigurationSource, times(1)).getAttribute(eq("mappingContextRef"));
	}

	@Test
	public void postProcessWithXmlRepositoryConfigurationSourceHavingNoMappingContextRefAttribute() {
		Element mockElement = mock(Element.class);

		XmlRepositoryConfigurationSource mockRepositoryConfigurationSource =
			mock(XmlRepositoryConfigurationSource.class);

		when(mockRepositoryConfigurationSource.getElement()).thenReturn(mockElement);
		when(mockElement.getAttribute(eq("mapping-context-ref"))).thenReturn(null);

		BeanDefinitionBuilder beanDefinitionBuilder = BeanDefinitionBuilder.genericBeanDefinition();

		repositoryConfigurationExtension.postProcess(beanDefinitionBuilder, mockRepositoryConfigurationSource);

		Object mappingContextRef = beanDefinitionBuilder.getRawBeanDefinition().getPropertyValues()
			.getPropertyValue("gemfireMappingContext");

		assertThat(mappingContextRef, is(nullValue()));

		verify(mockRepositoryConfigurationSource, times(1)).getElement();
		verify(mockElement, times(1)).getAttribute(eq("mapping-context-ref"));
	}

	@Test
	public void postProcessWithXmlRepositoryConfigurationSourceWith() {
		Element mockElement = mock(Element.class);

		XmlRepositoryConfigurationSource mockRepositoryConfigurationSource =
			mock(XmlRepositoryConfigurationSource.class);

		when(mockRepositoryConfigurationSource.getElement()).thenReturn(mockElement);
		when(mockElement.getAttribute(eq("mapping-context-ref"))).thenReturn("testMappingContext");

		BeanDefinitionBuilder beanDefinitionBuilder = BeanDefinitionBuilder.genericBeanDefinition();

		repositoryConfigurationExtension.postProcess(beanDefinitionBuilder, mockRepositoryConfigurationSource);

		Object mappingContextRef = beanDefinitionBuilder.getRawBeanDefinition().getPropertyValues()
			.getPropertyValue("gemfireMappingContext").getValue();

		assertThat(mappingContextRef, is(instanceOf(RuntimeBeanReference.class)));
		assertThat(((RuntimeBeanReference) mappingContextRef).getBeanName(), is(equalTo("testMappingContext")));

		verify(mockRepositoryConfigurationSource, times(1)).getElement();
		verify(mockElement, times(1)).getAttribute(eq("mapping-context-ref"));
	}

}
