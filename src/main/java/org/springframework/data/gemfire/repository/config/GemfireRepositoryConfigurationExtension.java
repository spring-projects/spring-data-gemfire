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

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean;
import org.springframework.data.repository.config.AnnotationRepositoryConfigurationSource;
import org.springframework.data.repository.config.RepositoryConfigurationExtension;
import org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport;
import org.springframework.data.repository.config.XmlRepositoryConfigurationSource;
import org.springframework.util.StringUtils;

/**
 * {@link RepositoryConfigurationExtension} implementation to add Gemfire
 * specific extensions to the repository XML namespace and annotation based
 * configuration.
 * 
 * @author Oliver Gierke
 * @author John Blum
 */
public class GemfireRepositoryConfigurationExtension extends RepositoryConfigurationExtensionSupport {

	private static final String ANNOTATION_MAPPING_CONTEXT_REF = "mappingContextRef";
	private static final String MAPPING_CONTEXT_PROPERTY_NAME = "gemfireMappingContext";
	private static final String XML_MAPPING_CONTEXT_REF = "mapping-context-ref";

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.springframework.data.repository.config.RepositoryConfigurationExtension
	 * #getRepositoryFactoryClassName()
	 */
	@Override
	public String getRepositoryFactoryClassName() {
		return GemfireRepositoryFactoryBean.class.getName();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.springframework.data.repository.config.
	 * RepositoryConfigurationExtensionSupport#getModulePrefix()
	 */
	@Override
	protected String getModulePrefix() {
		return "gemfire";
	}

	/**
	 *
	 * @see org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport
	 * 	#postProcess(BeanDefinitionBuilder, AnnotationRepositoryConfigurationSource)
	 */
	@Override
	public void postProcess(BeanDefinitionBuilder builder, AnnotationRepositoryConfigurationSource config) {
		String mappingContextRef = config.getAttribute(ANNOTATION_MAPPING_CONTEXT_REF);

		if (StringUtils.hasText(mappingContextRef)) {
			builder.addPropertyReference(MAPPING_CONTEXT_PROPERTY_NAME, mappingContextRef);
		}
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport
	 * 	#postProcess(BeanDefinitionBuilder, XmlRepositoryConfigurationSource)
	 */
	@Override
	public void postProcess(BeanDefinitionBuilder builder, XmlRepositoryConfigurationSource config) {
		String mappingContextRef = config.getElement().getAttribute(XML_MAPPING_CONTEXT_REF);

		if (StringUtils.hasText(mappingContextRef)) {
			builder.addPropertyReference(MAPPING_CONTEXT_PROPERTY_NAME, mappingContextRef);
		}
	}

}
