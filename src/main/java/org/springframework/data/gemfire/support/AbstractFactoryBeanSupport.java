/*
 * Copyright 2017-2018 the original author or authors.
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

package org.springframework.data.gemfire.support;

import java.util.Optional;
import java.util.function.Supplier;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanClassLoaderAware;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.FactoryBean;

/**
 * The {@link AbstractFactoryBeanSupport} class is an abstract Spring {@link FactoryBean} base class implementation
 * encapsulating operations common to SDG's {@link FactoryBean} implementations.
 *
 * @author John Blum
 * @see org.apache.commons.logging.Log
 * @see org.apache.commons.logging.LogFactory
 * @see org.springframework.beans.factory.BeanClassLoaderAware
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.BeanFactoryAware
 * @see org.springframework.beans.factory.BeanNameAware
 * @see org.springframework.beans.factory.FactoryBean
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractFactoryBeanSupport<T>
		implements FactoryBean<T>, BeanClassLoaderAware, BeanFactoryAware, BeanNameAware {

	protected static final boolean DEFAULT_SINGLETON = true;

	private ClassLoader beanClassLoader;

	private BeanFactory beanFactory;

	private final Log log;

	private String beanName;

	/**
	 * Constructs a new instance of {@link AbstractFactoryBeanSupport} and initialized the logger.
	 *
	 * @see #newLog()
	 */
	protected AbstractFactoryBeanSupport() {
		this.log = newLog();
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

	/**
	 * Sets a reference to the {@link ClassLoader} used by the Spring container to load and create bean classes.
	 *
	 * @param classLoader {@link ClassLoader} used by the Spring container to load and create bean classes.
	 * @see org.springframework.beans.factory.BeanClassLoaderAware#setBeanClassLoader(ClassLoader)
	 * @see java.lang.ClassLoader
	 */
	@Override
	public void setBeanClassLoader(ClassLoader classLoader) {
		this.beanClassLoader = classLoader;
	}

	/**
	 * Returns a reference to the {@link ClassLoader} used by the Spring container to load and create bean classes.
	 *
	 * @return the {@link ClassLoader} used by the Spring container to load and create bean classes.
	 * @see org.springframework.beans.factory.BeanClassLoaderAware#setBeanClassLoader(ClassLoader)
	 * @see java.lang.ClassLoader
	 */
	public ClassLoader getBeanClassLoader() {
		return this.beanClassLoader;
	}

	/**
	 * Sets a reference to the Spring {@link BeanFactory} in which this {@link FactoryBean} was declared.
	 *
	 * @param beanFactory reference to the declaring Spring {@link BeanFactory}.
	 * @see org.springframework.beans.factory.BeanFactoryAware#setBeanFactory(BeanFactory)
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		this.beanFactory = beanFactory;
	}

	/**
	 * Returns a reference to the Spring {@link BeanFactory} in which this {@link FactoryBean} was declared.
	 *
	 * @return a reference to the declaring Spring {@link BeanFactory}.
	 * @see org.springframework.beans.factory.BeanFactoryAware#setBeanFactory(BeanFactory)
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	public BeanFactory getBeanFactory() {
		return this.beanFactory;
	}

	/**
	 * Sets the {@link String bean name} assigned to this {@link FactoryBean} as declared in the Spring container.
	 *
	 * @param name {@link String bean name} assigned to this {@link FactoryBean} as declared in the Spring container.
	 * @see org.springframework.beans.factory.BeanNameAware#setBeanName(String)
	 * @see java.lang.String
	 */
	@Override
	public void setBeanName(String name) {
		this.beanName = name;
	}

	/**
	 * Returns the {@link String bean name} assigned to this {@link FactoryBean} as declared in the Spring container.
	 *
	 * @return the {@link String bean name} assigned to this {@link FactoryBean} as declared in the Spring container.
	 * @see org.springframework.beans.factory.BeanNameAware#setBeanName(String)
	 * @see java.lang.String
	 */
	public String getBeanName() {
		return this.beanName;
	}

	/**
	 * Returns a reference to the {@link Log} used by this {@link FactoryBean} to log {@link String messages}.
	 *
	 * @return a reference to the {@link Log} used by this {@link FactoryBean} to log {@link String messages}.
	 * @see org.apache.commons.logging.Log
	 */
	protected Log getLog() {
		return this.log;
	}

	/**
	 * Indicates that this {@link FactoryBean} produces a single bean instance.
	 *
	 * @return {@literal true} by default.
	 * @see org.springframework.beans.factory.FactoryBean#isSingleton()
	 */
	@Override
	public boolean isSingleton() {
		return DEFAULT_SINGLETON;
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
	 * Logs the {@link String message} supplied by the given {@link Supplier} at warn level.
	 *
	 * @param message {@link Supplier} containing the {@link String message} and arguments to log.
	 * @see org.apache.commons.logging.Log#isWarnEnabled()
	 * @see org.apache.commons.logging.Log#warn(Object)
	 * @see #getLog()
	 */
	protected void logWarning(Supplier<String> message) {
		Optional.ofNullable(getLog())
			.filter(Log::isWarnEnabled)
			.ifPresent(log -> log.warn(message.get()));
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
			.filter(Log::isErrorEnabled)
			.ifPresent(log -> log.error(message.get()));
	}
}
