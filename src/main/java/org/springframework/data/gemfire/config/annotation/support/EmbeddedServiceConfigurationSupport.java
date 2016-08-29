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
 *
 */

package org.springframework.data.gemfire.config.annotation.support;

import java.util.Map;
import java.util.Properties;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.type.AnnotationMetadata;
import org.springframework.data.gemfire.config.annotation.AbstractCacheConfiguration;
import org.springframework.data.gemfire.util.CollectionUtils;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * The EmbeddedServiceConfigurationSupport class is an abstract base class supporting the configuration
 * of Pivotal GemFire and Apache Geode embedded services.
 *
 * @author John Blum
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @since 1.9.0
 */
public abstract class EmbeddedServiceConfigurationSupport implements ImportBeanDefinitionRegistrar {

	public static final String DEFAULT_HOST = "localhost";

	@Autowired
	@SuppressWarnings("all")
	private AbstractCacheConfiguration cacheConfiguration;

	/* (non-Javadoc) */
	@SuppressWarnings("unchecked")
	protected <T extends AbstractCacheConfiguration> T cacheConfiguration() {
		Assert.state(cacheConfiguration != null, "AbstractCacheConfiguration was not properly initialized");
		return (T) this.cacheConfiguration;
	}

	/* (non-Javadoc) */
	protected abstract Class getAnnotationType();

	/* (non-Javadoc) */
	protected String getAnnotationTypeName() {
		return getAnnotationType().getName();
	}

	/* (non-Javadoc) */
	protected String getAnnotationTypeSimpleName() {
		return getAnnotationType().getSimpleName();
	}

	/* (non-Javadoc) */
	@Override
	public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {
		if (isAnnotationPresent(importingClassMetadata)) {
			Map<String, Object> annotationAttributes = getAnnotationAttributes(importingClassMetadata);

			Properties customGemFireProperties = toGemFireProperties(annotationAttributes);

			if (hasProperties(customGemFireProperties)) {
				try {
					cacheConfiguration().add(customGemFireProperties);
				}
				catch (Exception ignore) {
					registerGemFirePropertiesBeanPostProcessor(registry, customGemFireProperties);
				}
			}
		}
	}

	/* (non-Javadoc) */
	protected abstract Properties toGemFireProperties(Map<String, Object> annotationAttributes);

	/* (non-Javadoc) */
	protected boolean isAnnotationPresent(AnnotationMetadata importingClassMetadata) {
		return importingClassMetadata.hasAnnotation(getAnnotationTypeName());
	}

	/* (non-Javadoc) */
	protected boolean hasProperties(Properties properties) {
		return !CollectionUtils.isEmpty(properties);
	}

	/* (non-Javadoc) */
	protected Map<String, Object> getAnnotationAttributes(AnnotationMetadata importingClassMetadata) {
		return importingClassMetadata.getAnnotationAttributes(getAnnotationTypeName());
	}

	/* (non-Javadoc) */
	protected void registerGemFirePropertiesBeanPostProcessor(BeanDefinitionRegistry registry,
			Properties customGemFireProperties) {

		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(
			GemFirePropertiesBeanPostProcessor.class);

		builder.addConstructorArgValue(customGemFireProperties);

		BeanDefinitionReaderUtils.registerBeanDefinition(newBeanDefinitionHolder(builder), registry);
	}

	/* (non-Javadoc) */
	protected BeanDefinitionHolder newBeanDefinitionHolder(BeanDefinitionBuilder builder) {
		return new BeanDefinitionHolder(builder.getBeanDefinition(), generateBeanName());
	}

	/* (non-Javadoc) */
	protected String generateBeanName() {
		return String.format("%1$s.%2$s", getClass().getName(), getAnnotationTypeSimpleName());
	}

	/* (non-Javadoc) */
	protected String resolveHost(String hostname) {
		return resolveHost(hostname, DEFAULT_HOST);
	}

	/* (non-Javadoc) */
	protected String resolveHost(String hostname, String defaultHostname) {
		return (StringUtils.hasText(hostname) ? hostname : defaultHostname);
	}

	/* (non-Javadoc) */
	protected Integer resolvePort(Integer port, Integer defaultPort) {
		return (port != null ? port : defaultPort);
	}

	/**
	 * Spring {@link BeanPostProcessor} used to post process before initialization in GemFire System properties
	 * Spring bean defined in the application context.
	 *
	 * @see org.springframework.beans.factory.config.BeanPostProcessor
	 */
	protected static class GemFirePropertiesBeanPostProcessor implements BeanPostProcessor {

		protected static final String GEMFIRE_PROPERTIES_BEAN_NAME = "gemfireProperties";

		private final Properties gemfireProperties;

		protected GemFirePropertiesBeanPostProcessor(Properties gemfireProperties) {
			Assert.notEmpty(gemfireProperties, "GemFire Properties must not be null or empty");
			this.gemfireProperties = gemfireProperties;
		}

		@Override
		public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
			if (bean instanceof Properties && GEMFIRE_PROPERTIES_BEAN_NAME.equals(beanName)) {
				Properties gemfirePropertiesBean = (Properties) bean;
				gemfirePropertiesBean.putAll(gemfireProperties);
			}

			return bean;
		}

		@Override
		public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
			return bean;
		}
	}
}
