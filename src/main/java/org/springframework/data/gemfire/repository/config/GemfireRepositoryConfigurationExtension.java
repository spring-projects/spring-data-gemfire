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

import java.lang.annotation.Annotation;
import java.util.Collection;
import java.util.Collections;

import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.RootBeanDefinition;
import org.springframework.data.gemfire.mapping.GemfireMappingContext;
import org.springframework.data.gemfire.mapping.Region;
import org.springframework.data.gemfire.repository.GemfireRepository;
import org.springframework.data.gemfire.repository.support.GemfireRepositoryFactoryBean;
import org.springframework.data.repository.config.AnnotationRepositoryConfigurationSource;
import org.springframework.data.repository.config.RepositoryConfigurationExtension;
import org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport;
import org.springframework.data.repository.config.RepositoryConfigurationSource;
import org.springframework.data.repository.config.XmlRepositoryConfigurationSource;
import org.springframework.util.StringUtils;

/**
 * {@link RepositoryConfigurationExtension} implementation handling GemFire specific extensions to the Repository XML
 * namespace and annotation-based configuration meta-data.
 *
 * @author Oliver Gierke
 * @author John Blum
 * @see org.springframework.data.repository.config.RepositoryConfigurationExtension
 */
public class GemfireRepositoryConfigurationExtension extends RepositoryConfigurationExtensionSupport {

	private static final String GEMFIRE_MODULE_PREFIX = "gemfire";
	private static final String MAPPING_CONTEXT_PROPERTY_NAME = "gemfireMappingContext";
	private static final String MAPPING_CONTEXT_REF_ATTRIBUTE_NAME = "mappingContextRef";

	static final String DEFAULT_MAPPING_CONTEXT_BEAN_NAME = String.format("%1$s.%2$s",
			GemfireMappingContext.class.getName(), "DEFAULT");

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport#getIdentifyingAnnotations()
	 */
	@Override
	protected Collection<Class<? extends Annotation>> getIdentifyingAnnotations() {
		return Collections.<Class<? extends Annotation>>singleton(Region.class);
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport#getIdentifyingTypes()
	 */
	@Override
	protected Collection<Class<?>> getIdentifyingTypes() {
		return Collections.<Class<?>>singleton(GemfireRepository.class);
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport#getModulePrefix()
	 */
	@Override
	protected String getModulePrefix() {
		return GEMFIRE_MODULE_PREFIX;
	}

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
	 * @see org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport#postProcess(org.springframework.beans.factory.support.BeanDefinitionBuilder, org.springframework.data.repository.config.AnnotationRepositoryConfigurationSource)
	 */
	@Override
	public void postProcess(BeanDefinitionBuilder builder, AnnotationRepositoryConfigurationSource configurationSource) {
		builder.addPropertyReference(MAPPING_CONTEXT_PROPERTY_NAME, resolveMappingContextBeanName(
			configurationSource.getAttribute(MAPPING_CONTEXT_REF_ATTRIBUTE_NAME)));
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport#postProcess(org.springframework.beans.factory.support.BeanDefinitionBuilder, org.springframework.data.repository.config.XmlRepositoryConfigurationSource)
	 */
	@Override
	public void postProcess(BeanDefinitionBuilder builder, XmlRepositoryConfigurationSource configurationSource) {
		builder.addPropertyReference(MAPPING_CONTEXT_PROPERTY_NAME, resolveMappingContextBeanName(
			configurationSource.getAttribute(MAPPING_CONTEXT_REF_ATTRIBUTE_NAME)));
	}

	/* (non-Javadoc) */
	private static String resolveMappingContextBeanName(String source) {
		return (StringUtils.hasText(source) ? source : DEFAULT_MAPPING_CONTEXT_BEAN_NAME);
	}

	/*
	 * (non-Javadoc)
	 * @see org.springframework.data.repository.config.RepositoryConfigurationExtensionSupport#registerBeansForRoot(org.springframework.beans.factory.support.BeanDefinitionRegistry, org.springframework.data.repository.config.RepositoryConfigurationSource)
	 */
	@Override
	public void registerBeansForRoot(BeanDefinitionRegistry registry, RepositoryConfigurationSource configurationSource) {

		super.registerBeansForRoot(registry, configurationSource);

		if (!StringUtils.hasText(configurationSource.getAttribute(MAPPING_CONTEXT_REF_ATTRIBUTE_NAME))) {
			registry.registerBeanDefinition(DEFAULT_MAPPING_CONTEXT_BEAN_NAME,
				new RootBeanDefinition(GemfireMappingContext.class));
		}
	}
}
