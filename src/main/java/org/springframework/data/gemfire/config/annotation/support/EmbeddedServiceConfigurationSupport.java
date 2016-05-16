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
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.annotation.ImportBeanDefinitionRegistrar;
import org.springframework.core.type.AnnotationMetadata;
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

	/* (non-Javadoc) */
	protected static Properties setProperty(Properties properties, String key, Object value) {
		if (value != null) {
			setProperty(properties, key, value.toString());
		}

		return properties;
	}

	/* (non-Javadoc) */
	protected static Properties setProperty(Properties properties, String key, String value) {
		Assert.notNull(properties, "Properties must not be null");
		Assert.hasText(key, "Key must not be null or empty");

		if (StringUtils.hasText(value)) {
			properties.setProperty(key, value);
		}

		return properties;
	}

	/* (non-Javadoc) */
	@Override
	public void registerBeanDefinitions(AnnotationMetadata importingClassMetadata, BeanDefinitionRegistry registry) {

		if (importingClassMetadata.hasAnnotation(getAnnotationTypeName())) {
			Map<String, Object> annotationAttributes =
				importingClassMetadata.getAnnotationAttributes(getAnnotationTypeName());

			Properties customizedGemFireProperties = toGemFireProperties(annotationAttributes);

			if (!CollectionUtils.isEmpty(customizedGemFireProperties)) {
				registerGemFirePropertiesBeanPostProcessor(registry, customizedGemFireProperties);
			}
		}
	}

	/* (non-Javadoc) */
	protected String getAnnotationTypeName() {
		return getAnnotationType().getName();
	}

	/* (non-Javadoc) */
	protected String getAnnotationTypeSimpleName() {
		return getAnnotationType().getSimpleName();
	}

	/* (non-Javadoc) */
	protected abstract Class getAnnotationType();

	/* (non-Javadoc) */
	protected String getBeanName() {
		return String.format("%1$s.%2$s", getClass().getName(), getAnnotationTypeSimpleName());
	}

	/* (non-Javadoc) */
	protected BeanDefinitionHolder newBeanDefinitionHolder(BeanDefinitionBuilder builder) {
		return new BeanDefinitionHolder(builder.getBeanDefinition(), getBeanName());
	}

	/* (non-Javadoc) */
	protected abstract Properties toGemFireProperties(Map<String, Object> annotationAttributes);

	/* (non-Javadoc) */
	protected void registerGemFirePropertiesBeanPostProcessor(BeanDefinitionRegistry registry,
			Properties customizedGemFireProperties) {

		BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition(
			GemFirePropertiesBeanPostProcessor.class);

		builder.addConstructorArgValue(customizedGemFireProperties);

		BeanDefinitionReaderUtils.registerBeanDefinition(newBeanDefinitionHolder(builder), registry);
	}

	/**
	 * Spring {@link BeanPostProcessor} used to post process before initialization in GemFire System properties
	 * Spring bean defined in the application context.
	 *
	 * @see org.springframework.beans.factory.config.BeanPostProcessor
	 */
	protected static class GemFirePropertiesBeanPostProcessor implements BeanPostProcessor {

		static final String GEMFIRE_PROPERTIES_BEAN_NAME = "gemfireProperties";

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
