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

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalArgumentException;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Supplier;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanClassLoaderAware;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.context.EnvironmentAware;
import org.springframework.context.expression.BeanFactoryAccessor;
import org.springframework.context.expression.EnvironmentAccessor;
import org.springframework.context.expression.MapAccessor;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.env.Environment;
import org.springframework.core.type.AnnotationMetadata;
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
 * @see org.springframework.beans.factory.config.ConfigurableBeanFactory
 * @see org.springframework.beans.factory.support.AbstractBeanDefinition
 * @see org.springframework.beans.factory.support.BeanDefinitionRegistry
 * @see org.springframework.context.EnvironmentAware
 * @see org.springframework.context.expression.BeanFactoryAccessor
 * @see org.springframework.context.expression.EnvironmentAccessor
 * @see org.springframework.core.env.Environment
 * @see org.springframework.expression.EvaluationContext
 * @since 1.9.0
 */
@SuppressWarnings("unused")
public abstract class AbstractAnnotationConfigSupport
		implements BeanClassLoaderAware, BeanFactoryAware, EnvironmentAware {

	protected static final String SPRING_DATA_GEMFIRE_PROPERTY_PREFIX = "spring.data.gemfire.";

	private BeanFactory beanFactory;

	private ClassLoader beanClassLoader;

	private Environment environment;

	private final EvaluationContext evaluationContext;

	private final Log log;

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

	/**
	 * Constructs a new instance of {@link AbstractAnnotationConfigSupport}.
	 *
	 * @see #AbstractAnnotationConfigSupport(BeanFactory)
	 */
	public AbstractAnnotationConfigSupport() {
		this(null);
	}

	/**
	 * Constructs a new instance of {@link AbstractAnnotationConfigSupport}.
	 *
	 * @param beanFactory reference to the Spring {@link BeanFactory}.
	 * @see org.springframework.beans.factory.BeanFactory
	 * @see #newEvaluationContext(BeanFactory)
	 */
	public AbstractAnnotationConfigSupport(BeanFactory beanFactory) {
		this.evaluationContext = newEvaluationContext(beanFactory);
		this.log = newLog();
	}

	/**
	 * Constructs, configures and initializes a new instance of an {@link EvaluationContext}.
	 *
	 * @param beanFactory reference to the Spring {@link BeanFactory}.
	 * @return a new {@link EvaluationContext}.
	 * @see org.springframework.beans.factory.BeanFactory
	 * @see org.springframework.expression.EvaluationContext
	 * @see #getBeanFactory()
	 */
	protected EvaluationContext newEvaluationContext(BeanFactory beanFactory) {

		StandardEvaluationContext evaluationContext = new StandardEvaluationContext();

		evaluationContext.addPropertyAccessor(new BeanFactoryAccessor());
		evaluationContext.addPropertyAccessor(new EnvironmentAccessor());
		evaluationContext.addPropertyAccessor(new MapAccessor());
		evaluationContext.setTypeLocator(new StandardTypeLocator(getBeanClassLoader()));

		configureTypeConverter(evaluationContext, beanFactory);

		return evaluationContext;
	}

	/* (non-Javadoc) */
	private void configureTypeConverter(EvaluationContext evaluationContext, BeanFactory beanFactory) {

		Optional.ofNullable(evaluationContext)
			.filter(evalContext -> evalContext instanceof StandardEvaluationContext)
			.ifPresent(evalContext ->
				Optional.ofNullable(beanFactory)
					.filter(it -> it instanceof ConfigurableBeanFactory)
					.map(it -> ((ConfigurableBeanFactory) it).getConversionService())
					.ifPresent(conversionService ->
						((StandardEvaluationContext) evalContext).setTypeConverter(
							new StandardTypeConverter(conversionService)))
			);
	}

	/**
	 * Constructs a new instance of {@link Log} to log statements printed by Spring Data GemFire/Geode.
	 *
	 * @return a new instance of {@link Log}.
	 * @see org.apache.commons.logging.LogFactory#getLog(Class)
	 * @see org.apache.commons.logging.Log
	 */
	protected Log newLog() {
		return LogFactory.getLog(getClass());
	}

	/* (non-Javadoc) */
	protected boolean isAnnotationPresent(AnnotationMetadata importingClassMetadata) {
		return isAnnotationPresent(importingClassMetadata, getAnnotationTypeName());
	}

	/* (non-Javadoc) */
	protected boolean isAnnotationPresent(AnnotationMetadata importingClassMetadata, String annotationName) {
		return importingClassMetadata.hasAnnotation(annotationName);
	}

	/* (non-Javadoc) */
	protected AnnotationAttributes getAnnotationAttributes(Annotation annotation) {
		return AnnotationAttributes.fromMap(AnnotationUtils.getAnnotationAttributes(annotation));
	}

	/* (non-Javadoc) */
	protected AnnotationAttributes getAnnotationAttributes(AnnotationMetadata importingClassMetadata) {
		return getAnnotationAttributes(importingClassMetadata, getAnnotationTypeName());
	}

	/* (non-Javadoc) */
	protected AnnotationAttributes getAnnotationAttributes(AnnotationMetadata importingClassMetadata,
			String annotationName) {

		return AnnotationAttributes.fromMap(importingClassMetadata.getAnnotationAttributes(annotationName));
	}

	/**
	 * Returns the cache application {@link java.lang.annotation.Annotation} type pertaining to this configuration.
	 *
	 * @return the cache application {@link java.lang.annotation.Annotation} type used by this application.
	 */
	protected abstract Class<? extends Annotation> getAnnotationType();

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
	protected ClassLoader getBeanClassLoader() {
		return this.beanClassLoader;
	}

	/**
	 * Resolves the {@link ClassLoader bean ClassLoader} to the configured {@link ClassLoader}
	 * or the {@link Thread#getContextClassLoader() Thread Context ClassLoader}.
	 *
	 * @return the configured {@link ClassLoader} or the
	 * {@link Thread#getContextClassLoader() Thread Context ClassLoader}.
	 * @see java.lang.Thread#getContextClassLoader()
	 * @see #getBeanClassLoader()
	 */
	protected ClassLoader resolveBeanClassLoader() {
		return Optional.ofNullable(getBeanClassLoader()).orElseGet(() -> Thread.currentThread().getContextClassLoader());
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
		configureTypeConverter(getEvaluationContext(), beanFactory);
	}

	/**
	 * Returns a reference to the Spring {@link BeanFactory} in the current application context.
	 *
	 * @return a reference to the Spring {@link BeanFactory}.
	 * @throws IllegalStateException if the Spring {@link BeanFactory} was not properly configured.
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	protected BeanFactory getBeanFactory() {
		return Optional.ofNullable(this.beanFactory)
			.orElseThrow(() -> newIllegalStateException("BeanFactory is required"));
	}

	/**
	 * Sets a reference to the Spring {@link Environment}.
	 *
	 * @param environment Spring {@link Environment}.
	 * @see org.springframework.context.EnvironmentAware#setEnvironment(Environment)
	 * @see org.springframework.core.env.Environment
	 */
	@Override
	public void setEnvironment(Environment environment) {
		this.environment = environment;
	}

	/**
	 * Returns a reference to the Spring {@link Environment}.
	 *
	 * @return a reference to the Spring {@link Environment}.
	 * @see org.springframework.core.env.Environment
	 */
	protected Environment getEnvironment() {
		return this.environment;
	}

	/**
	 * Returns a reference to the {@link EvaluationContext} used to evaluate SpEL expressions.
	 *
	 * @return a reference to the {@link EvaluationContext} used to evaluate SpEL expressions.
	 * @see org.springframework.expression.EvaluationContext
	 */
	protected EvaluationContext getEvaluationContext() {
		return this.evaluationContext;
	}

	/**
	 * Returns a reference to the {@link Log} used by this class to log {@link String messages}.
	 *
	 * @return a reference to the {@link Log} used by this class to log {@link String messages}.
	 * @see org.apache.commons.logging.Log
	 */
	protected Log getLog() {
		return this.log;
	}

	/**
	 * Logs the {@link String message} formatted with the array of {@link Object arguments} at debug level.
	 *
	 * @param message {@link String} containing the message to log.
	 * @param args array of {@link Object arguments} used to format the {@code message}.
	 * @see #logDebug(Supplier)
	 */
	protected void logDebug(String message, Object... args) {
		logDebug(() -> String.format(message, args));
	}

	/**
	 * Logs the {@link String message} supplied by the given {@link Supplier} at debug level.
	 *
	 * @param message {@link Supplier} containing the {@link String message} and arguments to log.
	 * @see org.apache.commons.logging.Log#isDebugEnabled()
	 * @see org.apache.commons.logging.Log#debug(Object)
	 * @see #getLog()
	 */
	protected void logDebug(Supplier<String> message) {
		Optional.ofNullable(getLog())
			.filter(Log::isDebugEnabled)
			.ifPresent(log -> log.debug(message.get()));
	}

	/**
	 * Logs the {@link String message} formatted with the array of {@link Object arguments} at info level.
	 *
	 * @param message {@link String} containing the message to log.
	 * @param args array of {@link Object arguments} used to format the {@code message}.
	 * @see #logInfo(Supplier)
	 */
	protected void logInfo(String message, Object... args) {
		logInfo(() -> String.format(message, args));
	}

	/**
	 * Logs the {@link String message} supplied by the given {@link Supplier} at info level.
	 *
	 * @param message {@link Supplier} containing the {@link String message} and arguments to log.
	 * @see org.apache.commons.logging.Log#isInfoEnabled()
	 * @see org.apache.commons.logging.Log#info(Object)
	 * @see #getLog()
	 */
	protected void logInfo(Supplier<String> message) {
		Optional.ofNullable(getLog())
			.filter(Log::isInfoEnabled)
			.ifPresent(log -> log.info(message.get()));
	}

	/**
	 * Logs the {@link String message} formatted with the array of {@link Object arguments} at warn level.
	 *
	 * @param message {@link String} containing the message to log.
	 * @param args array of {@link Object arguments} used to format the {@code message}.
	 * @see #logWarning(Supplier)
	 */
	protected void logWarning(String message, Object... args) {
		logWarning(() -> String.format(message, args));
	}

	/**
	 * Logs the {@link String message} supplied by the given {@link Supplier} at warning level.
	 *
	 * @param message {@link Supplier} containing the {@link String message} and arguments to log.
	 * @see org.apache.commons.logging.Log#isWarnEnabled()
	 * @see org.apache.commons.logging.Log#warn(Object)
	 * @see #getLog()
	 */
	protected void logWarning(Supplier<String> message) {
		Optional.ofNullable(getLog())
			.filter(Log::isWarnEnabled)
			.ifPresent(log -> log.info(message.get()));
	}

	/**
	 * Logs the {@link String message} formatted with the array of {@link Object arguments} at error level.
	 *
	 * @param message {@link String} containing the message to log.
	 * @param args array of {@link Object arguments} used to format the {@code message}.
	 * @see #logError(Supplier)
	 */
	protected void logError(String message, Object... args) {
		logError(() -> String.format(message, args));
	}

	/**
	 * Logs the {@link String message} supplied by the given {@link Supplier} at error level.
	 *
	 * @param message {@link Supplier} containing the {@link String message} and arguments to log.
	 * @see org.apache.commons.logging.Log#isErrorEnabled()
	 * @see org.apache.commons.logging.Log#error(Object)
	 * @see #getLog()
	 */
	protected void logError(Supplier<String> message) {
		Optional.ofNullable(getLog())
			.filter(Log::isWarnEnabled)
			.ifPresent(log -> log.info(message.get()));
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
	 * @see #getBeanFactory()
	 */
	protected AbstractBeanDefinition register(AbstractBeanDefinition beanDefinition) {

		BeanFactory beanFactory = getBeanFactory();

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

	/* (non-Javadoc) */
	protected List<String> arrayOfPropertyNamesFor(String propertyNamePrefix) {
		return arrayOfPropertyNamesFor(propertyNamePrefix, null);
	}

	/* (non-Javadoc) */
	protected List<String> arrayOfPropertyNamesFor(String propertyNamePrefix, String propertyNameSuffix) {

		List<String> propertyNames = new ArrayList<>();

		boolean found = true;

		for (int index = 0; (found && index < Integer.MAX_VALUE); index++) {

			String propertyName = asArrayProperty(propertyNamePrefix, index, propertyNameSuffix);

			found = getEnvironment().containsProperty(propertyName);

			if (found) {
				propertyNames.add(propertyName);
			}
		}

		return propertyNames;
	}

	/* (non-Javadoc) */
	protected String asArrayProperty(String propertyNamePrefix, int index, String propertyNameSuffix) {
		return String.format("%1$s[%2$d]%3$s", propertyNamePrefix, index,
			Optional.ofNullable(propertyNamePrefix).filter(StringUtils::hasText).map("."::concat).orElse(""));
	}

	/* (non-Javadoc) */
	protected String cacheProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", propertyName("cache."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String cacheClientProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", propertyName("cache.client."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String cachePeerProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", propertyName("cache.peer."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String cacheServerProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", propertyName("cache.server."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String namedCacheServerProperty(String name, String propertyNameSuffix) {
		return String.format("%1$s%2$s.%3$s", propertyName("cache.server."), name, propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String diskStoreProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", propertyName("disk.store."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String namedDiskStoreProperty(String name, String propertyNameSuffix) {
		return String.format("%1$s%2$s.%3$s", propertyName("disk.store."), name, propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String locatorProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", propertyName("locator."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String loggingProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", propertyName("logging."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String managerProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", propertyName("manager."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String pdxProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", propertyName("pdx."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String poolProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", propertyName("pool."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String namedPoolProperty(String name, String propertyNameSuffix) {
		return String.format("%1$s%2$s.%3$s", propertyName("pool."), name, propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String securityProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", propertyName("security."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String sslProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", securityProperty("ssl."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String componentSslProperty(String component, String propertyNameSuffix) {
		return String.format("%1$s%2$s.%3$s", securityProperty("ssl."),
			component, propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String statsProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", propertyName("stats."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String serviceProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", propertyName("service."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String redisServiceProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", serviceProperty("redis."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String memcachedServiceProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", serviceProperty("memcached."), propertyNameSuffix);
	}

	/* (non-Javadoc) */
	protected String httpServiceProperty(String propertyNameSuffix) {
		return String.format("%1$s%2$s", serviceProperty("http."), propertyNameSuffix);
	}

	/**
	 * Returns the fully-qualified {@link String property name}.
	 *
	 * The fully qualified {@link String property name} consists of the {@link String property name}
	 * concatenated with the {@code propertyNameSuffix}.
	 *
	 * @param propertyNameSuffix {@link String} containing the property name suffix
	 * concatenated with the {@link String base property name}.
	 * @return the fully-qualified {@link String property name}.
	 * @see java.lang.String
	 */
	protected String propertyName(String propertyNameSuffix) {
		return String.format("%1$s%2$s", SPRING_DATA_GEMFIRE_PROPERTY_PREFIX, propertyNameSuffix);
	}

	/**
	 * Resolves the value for the given property identified by {@link String name} from the Spring {@link Environment}
	 * as an instance of the specified {@link Class type}.
	 *
	 * @param <T> {@link Class} type of the {@code propertyName property's} assigned value.
	 * @param propertyName {@link String} containing the name of the required property to resolve.
	 * @param type {@link Class} type of the property's assigned value.
	 * @return the assigned value of the {@link String named} property.
	 * @throws IllegalArgumentException if the property has not been assigned a value.
	 * For {@link String} values, this also means the value cannot be {@link String#isEmpty() empty}.
	 * For non-{@link String} values, this means the value must not be {@literal null}.
	 * @see #resolveProperty(String, Class, Object)
	 */
	protected <T> T requireProperty(String propertyName, Class<T> type) {

		return Optional.of(propertyName)
			.map(it -> resolveProperty(propertyName, type, null))
			.filter(Objects::nonNull)
			.filter(value -> !(value instanceof String) || StringUtils.hasText((String) value))
			.orElseThrow(() -> newIllegalArgumentException("Property [%s] is required", propertyName));
	}

	/**
	 * Attempts to resolve the property with the given {@link String name} from the Spring {@link Environment}
	 * as a {@link Boolean}.
	 *
	 * @param propertyName {@link String name} of the property to resolve.
	 * @param defaultValue default value to return if the property is not defined or not set.
	 * @return the value of the property identified by {@link String name} or default value if the property
	 * is not defined or not set.
	 * @see #resolveProperty(String, Class, Object)
	 */
	protected Boolean resolveProperty(String propertyName, Boolean defaultValue) {
		return resolveProperty(propertyName, Boolean.class, defaultValue);
	}

	/**
	 * Attempts to resolve the property with the given {@link String name} from the Spring {@link Environment}
	 * as an {@link Double}.
	 *
	 * @param propertyName {@link String name} of the property to resolve.
	 * @param defaultValue default value to return if the property is not defined or not set.
	 * @return the value of the property identified by {@link String name} or default value if the property
	 * is not defined or not set.
	 * @see #resolveProperty(String, Class, Object)
	 */
	protected Double resolveProperty(String propertyName, Double defaultValue) {
		return resolveProperty(propertyName, Double.class, defaultValue);
	}

	/**
	 * Attempts to resolve the property with the given {@link String name} from the Spring {@link Environment}
	 * as an {@link Float}.
	 *
	 * @param propertyName {@link String name} of the property to resolve.
	 * @param defaultValue default value to return if the property is not defined or not set.
	 * @return the value of the property identified by {@link String name} or default value if the property
	 * is not defined or not set.
	 * @see #resolveProperty(String, Class, Object)
	 */
	protected Float resolveProperty(String propertyName, Float defaultValue) {
		return resolveProperty(propertyName, Float.class, defaultValue);
	}

	/**
	 * Attempts to resolve the property with the given {@link String name} from the Spring {@link Environment}
	 * as an {@link Integer}.
	 *
	 * @param propertyName {@link String name} of the property to resolve.
	 * @param defaultValue default value to return if the property is not defined or not set.
	 * @return the value of the property identified by {@link String name} or default value if the property
	 * is not defined or not set.
	 * @see #resolveProperty(String, Class, Object)
	 */
	protected Integer resolveProperty(String propertyName, Integer defaultValue) {
		return resolveProperty(propertyName, Integer.class, defaultValue);
	}

	/**
	 * Attempts to resolve the property with the given {@link String name} from the Spring {@link Environment}
	 * as a {@link Long}.
	 *
	 * @param propertyName {@link String name} of the property to resolve.
	 * @param defaultValue default value to return if the property is not defined or not set.
	 * @return the value of the property identified by {@link String name} or default value if the property
	 * is not defined or not set.
	 * @see #resolveProperty(String, Class, Object)
	 */
	protected Long resolveProperty(String propertyName, Long defaultValue) {
		return resolveProperty(propertyName, Long.class, defaultValue);
	}

	/**
	 * Attempts to resolve the property with the given {@link String name} from the Spring {@link Environment}
	 * as a {@link String}.
	 *
	 * @param propertyName {@link String name} of the property to resolve.
	 * @param defaultValue default value to return if the property is not defined or not set.
	 * @return the value of the property identified by {@link String name} or default value if the property
	 * is not defined or not set.
	 * @see #resolveProperty(String, Class, Object)
	 */
	protected String resolveProperty(String propertyName, String defaultValue) {
		return resolveProperty(propertyName, String.class, defaultValue);
	}

	/**
	 * Attempts to resolve the property with the given {@link String name} from the Spring {@link Environment}.
	 *
	 * @param <T> {@link Class} type of the property value.
	 * @param propertyName {@link String name} of the property to resolve.
	 * @param targetType {@link Class} type of the property's value.
	 * @return the value of the property identified by {@link String name} or {@literal null} if the property
	 * is not defined or not set.
	 * @see #resolveProperty(String, Class, Object)
	 */
	protected <T> T resolveProperty(String propertyName, Class<T> targetType) {
		return resolveProperty(propertyName, targetType, null);
	}

	/**
	 * Attempts to resolve the property with the given {@link String name} from the Spring {@link Environment}.
	 *
	 * @param <T> {@link Class} type of the property value.
	 * @param propertyName {@link String name} of the property to resolve.
	 * @param targetType {@link Class} type of the property's value.
	 * @param defaultValue default value to return if the property is not defined or not set.
	 * @return the value of the property identified by {@link String name} or default value if the property
	 * is not defined or not set.
	 * @see #getEnvironment()
	 */
	protected <T> T resolveProperty(String propertyName, Class<T> targetType, T defaultValue) {

		return Optional.ofNullable(getEnvironment())
			.filter(environment -> environment.containsProperty(propertyName))
			.map(environment -> environment.getProperty(environment.resolveRequiredPlaceholders(propertyName),
				targetType, defaultValue))
			.orElse(defaultValue);
	}
}
