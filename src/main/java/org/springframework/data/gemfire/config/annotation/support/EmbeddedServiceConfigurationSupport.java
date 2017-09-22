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

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.util.Map;
import java.util.Optional;
import java.util.Properties;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.beans.factory.config.NamedBeanHolder;
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
 * The {@link EmbeddedServiceConfigurationSupport} class is an abstract base class supporting
 * the configuration of Pivotal GemFire and Apache Geode embedded services.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.config.AutowireCapableBeanFactory
 * @see org.springframework.beans.factory.config.BeanDefinitionHolder
 * @see org.springframework.beans.factory.support.BeanDefinitionBuilder
 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
 * @see org.springframework.context.annotation.ImportBeanDefinitionRegistrar
 * @see org.springframework.core.type.AnnotationMetadata
 * @see org.springframework.data.gemfire.config.annotation.AbstractCacheConfiguration
 * @see org.springframework.data.gemfire.config.annotation.support.AbstractAnnotationConfigSupport
 * @since 1.9.0
 */
@SuppressWarnings("unused")
public abstract class EmbeddedServiceConfigurationSupport extends AbstractAnnotationConfigSupport
		implements ImportBeanDefinitionRegistrar {

	public static final Integer DEFAULT_PORT = 0;
	public static final String DEFAULT_HOST = "localhost";

	@Autowired
	@SuppressWarnings("all")
	private AbstractCacheConfiguration cacheConfiguration;

	/**
	 * Returns a reference to an instance of the {@link AbstractCacheConfiguration} class used to configure
	 * a GemFire (Singleton, client or peer) cache instance along with it's associated, embedded services.
	 *
	 * @param <T> {@link Class} type extension of {@link AbstractCacheConfiguration}.
	 * @return a reference to a single {@link AbstractCacheConfiguration} instance.
	 * @throws IllegalStateException if the {@link AbstractCacheConfiguration} reference was not configured.
	 * @see org.springframework.data.gemfire.config.annotation.AbstractCacheConfiguration
	 */
	@SuppressWarnings("unchecked")
	protected <T extends AbstractCacheConfiguration> T cacheConfiguration() {
		return Optional.ofNullable((T) this.cacheConfiguration)
			.orElseThrow(() -> newIllegalStateException("AbstractCacheConfiguration is required"));
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public final void registerBeanDefinitions(AnnotationMetadata importingClassMetadata,
			BeanDefinitionRegistry registry) {

		if (isAnnotationPresent(importingClassMetadata)) {
			Map<String, Object> annotationAttributes = getAnnotationAttributes(importingClassMetadata);
			registerBeanDefinitions(importingClassMetadata, annotationAttributes, registry);
			setGemFireProperties(importingClassMetadata, annotationAttributes, registry);
		}
	}

	/* (non-Javadoc) */
	@SuppressWarnings("unused")
	protected void registerBeanDefinitions(AnnotationMetadata importingClassMetaData,
			Map<String, Object> annotationAttributes, BeanDefinitionRegistry registry) {
	}

	/* (non-Javadoc) */
	protected void setGemFireProperties(AnnotationMetadata importingClassMetadata,
			Map<String, Object> annotationAttributes, BeanDefinitionRegistry registry) {

		Properties gemfireProperties = toGemFireProperties(annotationAttributes);

		if (hasProperties(gemfireProperties)) {
			try {
				cacheConfiguration().add(gemfireProperties);
			}
			catch (Exception ignore) {
				registerGemFirePropertiesBeanPostProcessor(registry, gemfireProperties);
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
	protected void registerGemFirePropertiesBeanPostProcessor(BeanDefinitionRegistry registry,
			Properties customGemFireProperties) {

		BeanDefinitionBuilder builder =
			BeanDefinitionBuilder.genericBeanDefinition(GemFirePropertiesBeanPostProcessor.class);

		builder.addConstructorArgValue(customGemFireProperties);

		BeanDefinitionReaderUtils.registerBeanDefinition(newBeanDefinitionHolder(builder), registry);
	}

	/* (non-Javadoc) */
	protected BeanDefinitionHolder newBeanDefinitionHolder(BeanDefinitionBuilder builder) {
		return new BeanDefinitionHolder(builder.getBeanDefinition(), generateBeanName());
	}

	/* (non-Javadoc) */
	protected String generateBeanName() {
		return generateBeanName(getAnnotationType());
	}

	/* (non-Javadoc) */
	protected String generateBeanName(Class<?> typeQualifier) {
		return generateBeanName(typeQualifier.getSimpleName());
	}

	/* (non-Javadoc) */
	protected String generateBeanName(String nameQualifier) {
		return String.format("%1$s.%2$s", getClass().getName(), nameQualifier);
	}

	/**
	 * Resolves a Spring managed bean with the given {@link Class} type from the Spring {@link BeanFactory}.
	 *
	 * It is assumed that the given typed bean is the only bean of this {@link Class} type.  If more than 1 bean
	 * of the given {@link Class} type is found, then the Spring {@link BeanFactory} will throw
	 * a {@link org.springframework.beans.factory.NoUniqueBeanDefinitionException}.
	 *
	 * If the {@link BeanFactory} is an instance of {@link AutowireCapableBeanFactory}, then the returned bean
	 * will also be configured.
	 *
	 * @param <T> {@link Class} type of the registered Spring managed bean.
	 * @param beanType required {@link Class} type of the registered Spring managed bean.
	 * @return a Spring managed bean instance for the given, required {@link Class} type, or {@literal null}
	 * if no bean instance of the given, required {@link Class} type could be found.
	 * @throws BeansException if the Spring manage bean of the required {@link Class} type could not be resolved.
	 * @see #beanFactory()
	 */
	@SuppressWarnings("unchecked")
	protected <T> T resolveBean(Class<T> beanType) {

		BeanFactory beanFactory = beanFactory();

		if (beanFactory instanceof AutowireCapableBeanFactory) {
			AutowireCapableBeanFactory autowiringBeanFactory = (AutowireCapableBeanFactory) beanFactory;
			NamedBeanHolder<T> beanHolder = autowiringBeanFactory.resolveNamedBean(beanType);

			return (T) autowiringBeanFactory.configureBean(beanHolder.getBeanInstance(), beanHolder.getBeanName());
		}
		else {
			return beanFactory.getBean(beanType);
		}
	}

	/* (non-Javadoc) */
	protected String resolveHost(String hostname) {
		return resolveHost(hostname, DEFAULT_HOST);
	}

	/* (non-Javadoc) */
	protected String resolveHost(String hostname, String defaultHostname) {
		return Optional.ofNullable(hostname).filter(StringUtils::hasText).orElse(defaultHostname);
	}

	/* (non-Javadoc) */
	protected Integer resolvePort(Integer port) {
		return resolvePort(port, DEFAULT_PORT);
	}

	/* (non-Javadoc) */
	protected Integer resolvePort(Integer port, Integer defaultPort) {
		return Optional.ofNullable(port).orElse(defaultPort);
	}

	/**
	 * Spring {@link BeanPostProcessor} used to process before initialization Pivotal GemFire or Apache Geode
	 * {@link Properties} defined as a bean in the Spring application context.
	 *
	 * @see org.springframework.beans.factory.config.BeanPostProcessor
	 */
	protected static class GemFirePropertiesBeanPostProcessor implements BeanPostProcessor {

		protected static final String GEMFIRE_PROPERTIES_BEAN_NAME = "gemfireProperties";

		private final Properties gemfireProperties;

		/**
		 * Constructs a new instance of the {@link GemFirePropertiesBeanPostProcessor} initialized with
		 * the given GemFire/Geode {@link Properties}.
		 *
		 * @param gemfireProperties {@link Properties} used to configure Pivotal GemFire or Apache Geode.
		 * @throws IllegalArgumentException if {@link Properties} are {@literal null} or empty.
		 * @see java.util.Properties
		 */
		protected GemFirePropertiesBeanPostProcessor(Properties gemfireProperties) {
			Assert.notEmpty(gemfireProperties, "GemFire Properties are required");
			this.gemfireProperties = gemfireProperties;
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {

			if (bean instanceof Properties && GEMFIRE_PROPERTIES_BEAN_NAME.equals(beanName)) {
				Properties gemfirePropertiesBean = (Properties) bean;
				gemfirePropertiesBean.putAll(gemfireProperties);
			}

			return bean;
		}
	}
}
