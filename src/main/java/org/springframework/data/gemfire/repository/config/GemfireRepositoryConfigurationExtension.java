/*
 * Copyright 2012-2015 the original author or authors.
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

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.RootBeanDefinition;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean;
import org.springframework.data.repository.config.AnnotationRepositoryConfigurationSource;
import org.springframework.data.repository.config.RepositoryConfigurationExtension;
import org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport;
import org.springframework.data.repository.config.RepositoryConfigurationSource;
import org.springframework.data.repository.config.XmlRepositoryConfigurationSource;
import org.springframework.util.StringUtils;

/**
 * {@link RepositoryConfigurationExtension} implementation to add Gemfire specific extensions to the repository XML
 * namespace and annotation based configuration.
 * 
 * @author Oliver Gierke
 * @author John Blum
 */
public class GemfireRepositoryConfigurationExtension extends RepositoryConfigurationExtensionSupport {

	private static final String ANNOTATION_MAPPING_CONTEXT_REF = "mappingContextRef";
	private static final String MAPPING_CONTEXT_PROPERTY_NAME = "gemfireMappingContext";
	private static final String XML_MAPPING_CONTEXT_REF = "mapping-context-ref";
	
	static final String DEFAULT_MAPPING_CONTEXT_BEAN_NAME = String.format("%1$s.%2$s",
			GemfireMappingContext.class.getName(), "DEFAULT");

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.config.RepositoryConfigurationExtension#getRepositoryFactoryClassName()
	 */
	@Override
	public String getRepositoryFactoryClassName() {
		return GemfireRepositoryFactoryBean.class.getName();
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport#getModulePrefix()
	 */
	@Override
	protected String getModulePrefix() {
		return "gemfire";
	}

	/* 
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport#registerBeansForRoot(org.springframework.beans.factory.support.BeanDefinitionRegistry, org.springframework.data.repository.config.RepositoryConfigurationSource)
	 */
	@Override
	public void registerBeansForRoot(BeanDefinitionRegistry registry, RepositoryConfigurationSource configurationSource) {

		super.registerBeansForRoot(registry, configurationSource);
		
		String attribute = configurationSource.getAttribute(ANNOTATION_MAPPING_CONTEXT_REF);

		if (!StringUtils.hasText(attribute)) {
			registry.registerBeanDefinition(DEFAULT_MAPPING_CONTEXT_BEAN_NAME,
					new RootBeanDefinition(GemfireMappingContext.class));
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport#postProcess(org.springframework.beans.factory.support.BeanDefinitionBuilder, org.springframework.data.repository.config.AnnotationRepositoryConfigurationSource)
	 */
	@Override
	public void postProcess(BeanDefinitionBuilder builder, AnnotationRepositoryConfigurationSource config) {

		String mappingContextRef = config.getAttribute(ANNOTATION_MAPPING_CONTEXT_REF);
		builder.addPropertyReference(MAPPING_CONTEXT_PROPERTY_NAME, getDefaultedMappingContextBeanName(mappingContextRef));
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport#postProcess(org.springframework.beans.factory.support.BeanDefinitionBuilder, org.springframework.data.repository.config.XmlRepositoryConfigurationSource)
	 */
	@Override
	public void postProcess(BeanDefinitionBuilder builder, XmlRepositoryConfigurationSource config) {

		String mappingContextRef = config.getElement().getAttribute(XML_MAPPING_CONTEXT_REF);
		builder.addPropertyReference(MAPPING_CONTEXT_PROPERTY_NAME, getDefaultedMappingContextBeanName(mappingContextRef));
	}

	private static String getDefaultedMappingContextBeanName(String source) {
		return StringUtils.hasText(source) ? source : DEFAULT_MAPPING_CONTEXT_BEAN_NAME;
	}
}
