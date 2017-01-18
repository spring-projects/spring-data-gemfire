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

package org.springframework.data.gemfire.config.annotation.support;

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.util.Optional;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanClassLoaderAware;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.expression.BeanFactoryAccessor;
import org.springframework.context.expression.EnvironmentAccessor;
import org.springframework.context.expression.MapAccessor;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.expression.spel.support.StandardTypeConverter;
import org.springframework.expression.spel.support.StandardTypeLocator;
import org.springframework.util.StringUtils;

/**
 * The {@link AbstractAnnotationConfigSupport} class is an abstract base class encapsulating functionality
 * common to all Annotations and configuration classes used to configure Pivotal GemFire/Apache Geode objects
 * with Spring Data GemFire or Spring Data Geode.
 *
 * @author John Blum
 * @see java.lang.ClassLoader
 * @see org.springframework.beans.factory.BeanClassLoaderAware
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.BeanFactoryAware
 * @see org.springframework.beans.factory.InitializingBean
 * @see org.springframework.beans.factory.config.ConfigurableBeanFactory
 * @see org.springframework.beans.factory.support.AbstractBeanDefinition
 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
 * @see org.springframework.expression.EvaluationContext
 * @since 1.9.0
 */
@SuppressWarnings("unused")
public abstract class AbstractAnnotationConfigSupport
		implements BeanClassLoaderAware, BeanFactoryAware, InitializingBean {

	private BeanFactory beanFactory;

	private ClassLoader beanClassLoader;

	private EvaluationContext evaluationContext;

	/**
	 * Determines whether the given {@link Object} has value.  The {@link Object} is valuable
	 * if it is not {@literal null}.
	 *
	 * @param value {@link Object} to evaluate.
	 * @return a boolean value indicating whether the given {@link Object} has value.
	 */
	protected static boolean hasValue(Object value) {
		return Optional.ofNullable(value).isPresent();
	}

	/**
	 * Determines whether the given {@link Number} has value.  The {@link Number} is valuable
	 * if it is not {@literal null} and is not equal to 0.0d.
	 *
	 * @param value {@link Number} to evaluate.
	 * @return a boolean value indicating whether the given {@link Number} has value.
	 */
	protected static boolean hasValue(Number value) {
		return Optional.ofNullable(value).filter(it -> it.doubleValue() != 0.0d).isPresent();
	}

	/**
	 * Determines whether the given {@link String} has value.  The {@link String} is valuable
	 * if it is not {@literal null} or empty.
	 *
	 * @param value {@link String} to evaluate.
	 * @return a boolean value indicating whether the given {@link String} is valuable.
	 */
	protected static boolean hasValue(String value) {
		return StringUtils.hasText(value);
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		this.evaluationContext = newEvaluationContext();
	}

	/**
	 * Constructs, configures and initializes a new instance of an {@link EvaluationContext}.
	 *
	 * @return a new {@link EvaluationContext}.
	 * @see org.springframework.expression.EvaluationContext
	 * @see #beanFactory()
	 */
	protected EvaluationContext newEvaluationContext() {
		StandardEvaluationContext evaluationContext = new StandardEvaluationContext();

		evaluationContext.addPropertyAccessor(new BeanFactoryAccessor());
		evaluationContext.addPropertyAccessor(new EnvironmentAccessor());
		evaluationContext.addPropertyAccessor(new MapAccessor());
		evaluationContext.setTypeLocator(new StandardTypeLocator(beanClassLoader()));

		Optional.ofNullable(beanFactory())
			.filter(beanFactory -> beanFactory instanceof ConfigurableBeanFactory)
			.map(beanFactory -> ((ConfigurableBeanFactory) beanFactory).getConversionService())
			.ifPresent(conversionService ->
				evaluationContext.setTypeConverter(new StandardTypeConverter(conversionService)));

		return evaluationContext;
	}

	/**
	 * Returns the cache application {@link java.lang.annotation.Annotation} type pertaining to this configuration.
	 *
	 * @return the cache application {@link java.lang.annotation.Annotation} type used by this application.
	 */
	protected abstract Class getAnnotationType();

	/**
	 * Returns the fully-qualified {@link Class#getName() class name} of the cache application
	 * {@link java.lang.annotation.Annotation} type.
	 *
	 * @return the fully-qualified {@link Class#getName() class name} of the cache application
	 * {@link java.lang.annotation.Annotation} type.
	 * @see java.lang.Class#getName()
	 * @see #getAnnotationType()
	 */
	protected String getAnnotationTypeName() {
		return getAnnotationType().getName();
	}

	/**
	 * Returns the simple {@link Class#getName() class name} of the cache application
	 * {@link java.lang.annotation.Annotation} type.
	 *
	 * @return the simple {@link Class#getName() class name} of the cache application
	 * {@link java.lang.annotation.Annotation} type.
	 * @see java.lang.Class#getSimpleName()
	 * @see #getAnnotationType()
	 */
	protected String getAnnotationTypeSimpleName() {
		return getAnnotationType().getSimpleName();
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setBeanClassLoader(ClassLoader beanClassLoader) {
		this.beanClassLoader = beanClassLoader;
	}

	/**
	 * Returns a reference to the {@link ClassLoader} use by the Spring {@link BeanFactory} to load classes
	 * for bean definitions.
	 *
	 * @return the {@link ClassLoader} used by the Spring {@link BeanFactory} to load classes for bean definitions.
	 * @see #setBeanClassLoader(ClassLoader)
	 */
	protected ClassLoader beanClassLoader() {
		return this.beanClassLoader;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	/**
	 * Returns a reference to the Spring {@link BeanFactory} in the current application context.
	 *
	 * @return a reference to the Spring {@link BeanFactory}.
	 * @throws IllegalStateException if the Spring {@link BeanFactory} was not properly configured.
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	protected BeanFactory beanFactory() {
		return Optional.ofNullable(this.beanFactory)
			.orElseThrow(() -> newIllegalStateException("BeanFactory is required"));
	}

	/**
	 *
	 * @return
	 */
	protected EvaluationContext evaluationContext() {
		return this.evaluationContext;
	}

	/**
	 * Registers the {@link AbstractBeanDefinition} with the {@link BeanDefinitionRegistry} using a generated bean name.
	 *
	 * @param beanDefinition {@link AbstractBeanDefinition} to register.
	 * @return the given {@link AbstractBeanDefinition}.
	 * @see org.springframework.beans.factory.BeanFactory
	 * @see org.springframework.beans.factory.support.AbstractBeanDefinition
	 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
	 * @see org.springframework.beans.factory.support.BeanDefinitionReaderUtils#registerWithGeneratedName(AbstractBeanDefinition, BeanDefinitionRegistry)
	 * @see #beanFactory()
	 */
	protected AbstractBeanDefinition register(AbstractBeanDefinition beanDefinition) {

		BeanFactory beanFactory = beanFactory();

		return (beanFactory instanceof BeanDefinitionRegistry
			? register(beanDefinition, (BeanDefinitionRegistry) beanFactory)
			: beanDefinition);
	}

	/**
	 * Registers the {@link AbstractBeanDefinition} with the {@link BeanDefinitionRegistry} using a generated bean name.
	 *
	 * @param beanDefinition {@link AbstractBeanDefinition} to register.
	 * @param registry {@link BeanDefinitionRegistry} used to register the {@link AbstractBeanDefinition}.
	 * @return the given {@link AbstractBeanDefinition}.
	 * @see org.springframework.beans.factory.support.AbstractBeanDefinition
	 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
	 * @see org.springframework.beans.factory.support.BeanDefinitionReaderUtils#registerWithGeneratedName(AbstractBeanDefinition, BeanDefinitionRegistry)
	 */
	protected AbstractBeanDefinition register(AbstractBeanDefinition beanDefinition, BeanDefinitionRegistry registry) {

		Optional.ofNullable(registry).ifPresent(it ->
			BeanDefinitionReaderUtils.registerWithGeneratedName(beanDefinition, it)
		);

		return beanDefinition;
	}
}
