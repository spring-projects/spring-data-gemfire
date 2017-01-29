/*
 * Copyright 2016-2018 the original author or authors.
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

package org.springframework.data.gemfire.support;

import java.util.Properties;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.geode.cache.CacheCallback;
import org.apache.geode.cache.CacheLoader;
import org.apache.geode.cache.Declarable;
import org.apache.geode.cache.LoaderHelper;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationListener;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.util.Assert;

/**
 * The {@link LazyWiringDeclarableSupport} class is an implementation of GemFire's {@link Declarable} interface
 * that enables support for wiring GemFire components with Spring bean dependencies defined in
 * a Spring {@link ApplicationContext}.
 *
 * @author John Blum
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.ApplicationListener
 * @see org.springframework.context.event.ContextRefreshedEvent
 * @see org.springframework.data.gemfire.support.SpringContextBootstrappingInitializer
 * @see org.springframework.data.gemfire.support.WiringDeclarableSupport
 * @see org.apache.geode.cache.Declarable
 * @since 1.3.4
 */
@SuppressWarnings("unused")
public abstract class LazyWiringDeclarableSupport extends WiringDeclarableSupport
		implements ApplicationListener<ContextRefreshedEvent>, DisposableBean {

	// atomic reference to the parameters passed by GemFire when this Declarable object
	// was constructed, configured and its init method called
	private final AtomicReference<Properties> parametersReference = new AtomicReference<>();

	// condition to determine the initialized state of this Declarable object
	volatile boolean initialized = false;

	/**
	 * Constructs a new instance of the {@link LazyWiringDeclarableSupport} class registered with the
	 * {@link SpringContextBootstrappingInitializer} as a Spring {@link ApplicationListener}.
	 *
	 * This {@link Declarable} object will receive notifications from the {@link SpringContextBootstrappingInitializer}
	 * when the Spring context is created and initialized (refreshed).  The notification is necessary in order for
	 * this {@link Declarable} object to be properly configured and initialized with any required,
	 * Spring-defined dependencies.
	 *
	 * @see org.springframework.data.gemfire.support.SpringContextBootstrappingInitializer
	 * 	#register(org.springframework.context.ApplicationListener)
	 */
	public LazyWiringDeclarableSupport() {
		SpringContextBootstrappingInitializer.register(this);
	}

	/**
	 * Asserts that this {@link Declarable} object has been properly configured and initialized by the Spring container
	 * after has GemFire constructed this {@link Declarable} object during startup.
	 *
	 * This method is recommended to be called before any of this {@link Declarable} object's {@link CacheCallback}
	 * methods (e.g. {@link CacheLoader#load(LoaderHelper)} are invoked in order to ensure that this {@link Declarable}
	 * object was properly constructed, configured and initialized by the Spring container before hand.
	 *
	 * @throws IllegalStateException if this {@link Declarable} object was not been properly constructed, configured
	 * and initialized by the Spring container.
	 * @see #init(java.util.Properties)
	 * @see #isInitialized()
	 */
	protected void assertInitialized() {
		Assert.state(isInitialized(), String.format(
			"This Declarable object [%s] has not been properly configured and initialized", getClass().getName()));
	}

	/**
	 * Asserts that this {@link Declarable} object has not yet been used, or activated prior to being fully constructed,
	 * configured and initialized by the Spring container.
	 *
	 * It is possible, though rare, that the {@link #init(Properties)} method might be called multiple times by GemFire
	 * before the Spring container constructs, configures, initializes and generally puts this component to use.
	 *
	 * @throws java.lang.IllegalStateException if the Declarable object has already been configured and initialized
	 * by the Spring container.
	 * @see #init(java.util.Properties)
	 * @see #isNotInitialized()
	 */
	protected void assertUninitialized() {
		Assert.state(isNotInitialized(), String.format(
			"This Declarable object [%s] has already been configured and initialized", getClass().getName()));
	}

	/**
	 * Determines whether this {@link Declarable} object has been properly configured and initialized
	 * by the Spring container.
	 *
	 * @return a boolean value indicating whether this {@link Declarable} object has been properly configured
	 * and initialized by the Spring container.
	 * @see #doInit(BeanFactory, Properties)
	 * @see #assertInitialized()
	 */
	protected boolean isInitialized() {
		return this.initialized;
	}

	/**
	 * Determines whether this {@link Declarable} object has been properly configured and initialized
	 * by the Spring container.
	 *
	 * @return a boolean value indicating whether this {@link Declarable} object has been properly configured
	 * and initialized by the Spring container.
	 * @see #doInit(BeanFactory, Properties)
	 * @see #isInitialized()
	 */
	protected boolean isNotInitialized() {
		return !isInitialized();
	}

	/**
	 * Initialization method called by GemFire with the configured parameters once this {@link Declarable} object
	 * has been constructed by GemFire and the &lt;initalizer&gt; element is parsed
	 * in GemFire's configuration meta-data during startup.
	 *
	 * @param parameters {@link Properties} containing the configured parameters parsed from GemFire's
	 * configuration meta-data (e.g. {@literal cache.xml}) and passed to this {@link Declarable} object.
	 * @see #doInit(BeanFactory, Properties)
	 * @see java.util.Properties
	 */
	@Override
	public final void init(Properties parameters) {
		setParameters(parameters);

		try {
			doInit(locateBeanFactory(), nullSafeGetParameters());
		}
		catch (IllegalStateException ignore) {
			// BeanFactory does not exist, has been closed or the GemfireBeanFactoryLocator is not in use
		}
	}

	/**
	 * Performs the actual configuration and initialization of this {@link Declarable} object before use.
	 *
	 * This method is triggered by the Spring {@link org.springframework.context.ApplicationContext}, Spring application
	 * {@link ContextRefreshedEvent}) indicating that the Spring container (context) has been created and refreshed.
	 *
	 * @param parameters {@link Properties} containing the configured parameters parsed from GemFire's
	 * configuration meta-data (e.g. {@literal cache.xml}) and passed to this {@link Declarable} object.
	 * @throws IllegalArgumentException if the {@literal bean-name} parameter was specified in GemFire's
	 * configuration meta-data but no bean with the specified name could be found in the Spring context.
	 * @see #init(java.util.Properties)
	 * @see #configureThis(BeanFactory, String)
	 * @see #doPostInit(java.util.Properties)
	 * @see java.util.Properties
	 */
	synchronized void doInit(BeanFactory beanFactory, Properties parameters) {
		this.initialized = (isInitialized() || configureThis(beanFactory,
			parameters.getProperty(TEMPLATE_BEAN_NAME_PROPERTY)));

		doPostInit(parameters);
	}

	/**
	 * Performs any post configuration and initialization activities required by the application.
	 *
	 * By default, this method does nothing.
	 *
	 * @param parameters {@link Properties} containing the configured parameters parsed from GemFire's
	 * configuration meta-data (e.g. {@literal cache.xml}) and passed to this {@link Declarable} object.
	 * @see #doInit(BeanFactory, Properties)
	 * @see java.util.Properties
	 */
	protected void doPostInit(Properties parameters) {
	}

	/**
	 * Null-safe operation to return the parameters passed to this {@link Declarable} object when created by GemFire
	 * from it's own configuration meta-data (e.g. {@literal cache.xml}).
	 *
	 * @return a {@link Properties} containing the configured parameters parsed from GemFire's configuration meta-data
	 * (e.g. {@literal cache.xml}) and passed to this {@link Declarable} object.
	 * @see java.util.Properties
	 */
	protected Properties nullSafeGetParameters() {
		Properties parameters = parametersReference.get();
		return (parameters != null ? parameters : new Properties());
	}

	/**
	 * Stores a reference to the {@link Properties parameters} passed to the {@link Declarable#init(Properties)} method.
	 *
	 * @param parameters {@link Properties} containing the configured parameters parsed from GemFire's
	 * configuration meta-data (e.g. {@literal cache.xml}) and passed to this {@link Declarable} object.
	 * @see java.util.Properties
	 */
	protected void setParameters(Properties parameters) {
		parametersReference.set(parameters);
	}

	/**
	 * Event handler method called when GemFire has created and initialized (refreshed)
	 * the Spring {@link ApplicationContext} using the {@link SpringContextBootstrappingInitializer}.
	 *
	 * @param event {@link ContextRefreshedEvent} published by the Spring {@link ApplicationContext} after it is
	 * successfully created and initialized by GemFire.
	 * @see org.springframework.context.event.ContextRefreshedEvent
	 * @see #doInit(BeanFactory, Properties)
	 * @see #nullSafeGetParameters()
	 */
	@Override
	@SuppressWarnings("all")
	public final void onApplicationEvent(ContextRefreshedEvent event) {
		ApplicationContext applicationContext = event.getApplicationContext();

		Assert.isTrue(applicationContext instanceof ConfigurableApplicationContext, String.format(
			"The Spring ApplicationContext [%s] must be an instance of ConfigurableApplicationContext",
				applicationContext));

		ConfigurableListableBeanFactory beanFactory =
			((ConfigurableApplicationContext) applicationContext).getBeanFactory();

		doInit(beanFactory, nullSafeGetParameters());
	}

	/**
	 * When this {@link Declarable} object/bean gets destroyed by the Spring container, {@code destroy()} will
	 * make sure this component gets unregistered from the {@link SpringContextBootstrappingInitializer} properly.
	 *
	 * @throws Exception if bean destruction is unsuccessful.
	 * @see org.springframework.data.gemfire.support.SpringContextBootstrappingInitializer
	 * 	#unregister(org.springframework.context.ApplicationListener)
	 * @see #setParameters(Properties)
	 */
	@Override
	public void destroy() throws Exception {
		SpringContextBootstrappingInitializer.unregister(this);
		setParameters(null);
		this.initialized = false;
	}
}
