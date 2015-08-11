/*
 * Copyright 2010-2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.data.gemfire;

import java.util.Properties;
import java.util.concurrent.atomic.AtomicReference;

import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.access.BeanFactoryReference;
import org.springframework.beans.factory.wiring.BeanConfigurerSupport;
import org.springframework.beans.factory.wiring.BeanWiringInfo;
import org.springframework.beans.factory.wiring.BeanWiringInfoResolver;
import org.springframework.context.ApplicationListener;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.data.gemfire.support.SpringContextBootstrappingInitializer;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.Declarable;

/**
 * The LazyWiringDeclarableSupport class is an implementation of the GemFire Declarable interface that enables support
 * for wiring GemFire components with Spring bean dependencies defined in the Spring context.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.wiring.BeanConfigurerSupport
 * @see org.springframework.beans.factory.wiring.BeanWiringInfo
 * @see org.springframework.beans.factory.wiring.BeanWiringInfoResolver
 * @see org.springframework.context.ApplicationListener
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.event.ContextRefreshedEvent
 * @see org.springframework.data.gemfire.DeclarableSupport
 * @see org.springframework.data.gemfire.WiringDeclarableSupport
 * @see org.springframework.data.gemfire.support.SpringContextBootstrappingInitializer
 * @see com.gemstone.gemfire.cache.Declarable
 * @since 1.3.4
 */
@SuppressWarnings("unused")
public abstract class LazyWiringDeclarableSupport implements ApplicationListener<ContextRefreshedEvent>, Declarable,
		DisposableBean {

	// name of the template bean defined in the Spring context for wiring this Declarable instance.
	protected static final String BEAN_NAME_PARAMETER = "bean-name";

	// atomic reference to the parameter passed by GemFire when this Declarable was constructed
	// and the init method was called.
	private final AtomicReference<Properties> parametersReference = new AtomicReference<Properties>();

	// condition determining the initialization state of this Declarable
	volatile boolean initialized = false;

	private BeanFactoryReference beanFactoryReference = null;

	private String factoryKey = null;

	/**
	 * Constructs an instance of the LazyWiringDeclarableSupport class registered with the
	 * SpringContextBootstrappingInitializer.  This Declarable will receive notifications from the
	 * SpringContextBootstrappingInitializer when the Spring context is created and initialized (refreshed).
	 * The notification is necessary in order for this Declarable component to be configured and properly initialized
	 * with any required Spring bean dependencies.
	 *
	 * @see org.springframework.data.gemfire.support.SpringContextBootstrappingInitializer
	 * 	#register(org.springframework.context.ApplicationListener)
	 */
	public LazyWiringDeclarableSupport() {
		SpringContextBootstrappingInitializer.register(this);
	}

	/**
	 * Set the key used to locate (lookup) the Spring BeanFactory.
	 *
	 * @param factoryKey the key used to locate the Spring BeanFactory.
	 * @see org.springframework.beans.factory.BeanFactory
	 * @see org.springframework.data.gemfire.GemfireBeanFactoryLocator
	 * @see #getFactoryKey()
	 */
	public final void setFactoryKey(final String factoryKey) {
		this.factoryKey = factoryKey;
	}

	/**
	 * Gets the key used to locate (lookup) the Spring BeanFactory.
	 *
	 * @return the key used to locate the Spring BeanFactory.
	 * @see org.springframework.beans.factory.BeanFactory
	 * @see org.springframework.data.gemfire.GemfireBeanFactoryLocator
	 * @see #setFactoryKey(String)
	 */
	protected String getFactoryKey() {
		return factoryKey;
	}

	/**
	 * Asserts that this Declarable object has been properly configured and initialized by the Spring container
	 * after GemFire has constructed this Declarable object during startup.  It is recommended to call this method
	 * in any GemFire CacheCallback/Declarable object operational method (e.g. CacheLoader.load(..)) before use
	 * in order to ensure that this Declarable was properly constructed, configured and initialized.
	 *
	 * @throws IllegalStateException if the Declarable object has not been properly configured or initialized
	 * by the Spring container.
	 * @see #init(java.util.Properties)
	 * @see #isInitialized()
	 */
	protected void assertInitialized() {
		Assert.state(isInitialized(), String.format(
			"This Declarable object (%1$s) has not been properly configured and initialized",
				getClass().getName()));
	}

	/**
	 * Asserts that this Declarable object has not yet been used, or activated prior to being fully configured
	 * and initialized.  It is possible, though rare, that the init(:Properties) might be called multiple times
	 * by GemFire before the Spring container configure, initializes and puts this component to use.
	 *
	 * @throws java.lang.IllegalStateException if the Declarable object has already been configured and initialized
	 * by the Spring container.
	 * @see #init(java.util.Properties)
	 * @see #isInitialized()
	 */
	protected void assertUninitialized() {
		Assert.state(!isInitialized(), String.format(
			"This Declarable object (%1$s) has already been configured and initialized",
			getClass().getName()));
	}

	/**
	 * Performs the actual configuration and initialization of this Declarable object before use.  This method
	 * is triggered by an ApplicationEvent (specifically, the ContextRefreshedEvent) indicating that the Spring context
	 * has been created and refreshed.
	 *
	 * @param beanFactory the ConfigurableListableBeanFactory used to configure and initialize this Declarable GemFire
	 * component.
	 * @param parameters Properties instance containing the parameters from GemFire's configuration file
	 * (e.g. cache.xml) to configure and initialize this Declarable object.
	 * @throws IllegalArgumentException if the bean-name parameter was specified in GemFire configuration meta-data
	 * but no bean with the specified name could be found in the Spring context.
	 * @see #init(java.util.Properties)
	 * @see #doPostInit(java.util.Properties)
	 * @see org.springframework.beans.factory.wiring.BeanConfigurerSupport
	 * @see org.springframework.beans.factory.wiring.BeanWiringInfo
	 * @see org.springframework.beans.factory.wiring.BeanWiringInfoResolver
	 */
	void doInit(final BeanFactory beanFactory, final Properties parameters) {
		synchronized (this) {
			if (isNotInitialized()) {
				BeanConfigurerSupport beanConfigurer = new BeanConfigurerSupport();

				beanConfigurer.setBeanFactory(beanFactory);

				final String templateBeanName = parameters.getProperty(BEAN_NAME_PARAMETER);

				if (StringUtils.hasText(templateBeanName)) {
					if (beanFactory.containsBean(templateBeanName)) {
						beanConfigurer.setBeanWiringInfoResolver(new BeanWiringInfoResolver() {
							@Override public BeanWiringInfo resolveWiringInfo(final Object beanInstance) {
								return new BeanWiringInfo(templateBeanName);
							}
						});
					}
					else {
						throw new IllegalArgumentException(String.format(
							"No bean with name '%1$s' was found in the Spring context '%2$s'.", templateBeanName, beanFactory));
					}
				}

				beanConfigurer.afterPropertiesSet();
				beanConfigurer.configureBean(this);
				beanConfigurer.destroy();

				initialized = true;
			}
		}

		doPostInit(parameters);
	}

	/**
	 * Default no operation method performed post initialization of this Declarable GemFire component to be overridden
	 * by subclasses for application specific extension and behavior.
	 *
	 * @param parameters Properties instance containing the parameters from GemFire's configuration file
	 * (e.g. cache.xml) to configure and initialize this Declarable object.
	 * @see #doInit(BeanFactory, Properties)
	 */
	protected void doPostInit(final Properties parameters) {
	}

	/**
	 * Initialization method called by GemFire with configured parameters once this Declarable object has been
	 * constructed during GemFire startup using an &lt;initalizer&gt; element in GemFire's configuration meta-data.
	 *
	 * @param parameters the configured parameters passed from the GemFire configuration (e.g. cache.xml) to this
	 * Declarable as a Properties instance.
	 * @throws IllegalStateException if this Declarable object has already been configured/initialized
	 * by the Spring container and is currently active.
	 * @see #doInit(BeanFactory, Properties)
	 * @see java.util.Properties
	 */
	@Override
	public final void init(final Properties parameters) {
		parametersReference.set(parameters);

		try {
			doInit(locateBeanFactory(getFactoryKey()), nullSafeGetParameters());
		}
		catch (IllegalStateException ignore) {
			// the BeanFactory does not exist or has been released and or closed, so ignore
		}
	}

	/**
	 * Determines whether this Declarable object has been configured and initialized (i.e. the doInit method
	 * has been called) by the Spring container.
	 *
	 * @return a boolean value indicating whether this Declarable object has been configured and initialized
	 * by the Spring container.
	 * @see #assertInitialized()
	 * @see #doInit(BeanFactory, Properties)
	 */
	protected boolean isInitialized() {
		return initialized;
	}

	/**
	 * Determines whether this Declarable object has been configured and initialized (i.e. the doInit method
	 * has been called) by the Spring container.
	 *
	 * @return a boolean value indicating whether this Declarable object has been configured and initialized
	 * by the Spring container.
	 * @see #doInit(BeanFactory, Properties)
	 * @see #isInitialized()
	 */
	protected boolean isNotInitialized() {
		return !isInitialized();
	}

	/**
	 * Locates an existing Spring BeanFactory.
	 *
	 * @param factoryKey the key used to locate (lookup) the Spring BeanFactory.
	 * @return a reference to the Spring BeanFactory if it exists.
	 * @throws IllegalStateException if the BeanFactory has already been released or closed.
	 * @see org.springframework.data.gemfire.GemfireBeanFactoryLocator
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	protected BeanFactory locateBeanFactory(String factoryKey) {
		if (beanFactoryReference == null) {
			beanFactoryReference = new GemfireBeanFactoryLocator().useBeanFactory(factoryKey);
		}

		return beanFactoryReference.getFactory();
	}

	/**
	 * Null-safe operation to return the parameters passed to this Declarable object when created by GemFire from it's
	 * configuration meta-data.
	 *
	 * @return a Properties object containing the a parameters specified for this Declarable, or an
	 * empty Properties object if no parameters were supplied.
	 * @see java.util.Properties
	 */
	protected Properties nullSafeGetParameters() {
		Properties parameters = parametersReference.get();
		return (parameters != null ? parameters : new Properties());
	}

	/**
	 * Event handler method called when GemFire has created and initialized (refreshed) the Spring ApplicationContext
	 * using the SpringContextBootstrappingInitializer Declarable class.
	 *
	 * @param event the ContextRefreshedEvent published by the Spring ApplicationContext after it is successfully
	 * created and initialized by GemFire.
	 * @throws IllegalArgumentException if the ApplicationContext is not an instance of ConfigurableApplicationContext.
	 * @see #doInit(BeanFactory, Properties)
	 * @see #nullSafeGetParameters()
	 * @see org.springframework.context.ApplicationListener#onApplicationEvent(org.springframework.context.ApplicationEvent)
	 * @see org.springframework.context.event.ContextRefreshedEvent
	 */
	@Override
	public final void onApplicationEvent(final ContextRefreshedEvent event) {
		Assert.isTrue(event.getApplicationContext() instanceof ConfigurableApplicationContext, String.format(
			"The Spring ApplicationContext (%1$s) must be an instance of ConfigurableApplicationContext",
				ObjectUtils.nullSafeClassName(event.getApplicationContext())));

		doInit(((ConfigurableApplicationContext) event.getApplicationContext()).getBeanFactory(),
			nullSafeGetParameters());
	}

	/**
	 * When this bean gets destroyed by the Spring container, make sure this component gets unregistered from the
	 * SpringContextBootstrappingInitializer.
	 *
	 * @throws Exception if bean destruction is unsuccessful.
	 * @see org.springframework.data.gemfire.support.SpringContextBootstrappingInitializer
	 * 	#unregister(org.springframework.context.ApplicationListener)
	 */
	@Override
	public void destroy() throws Exception {
		SpringContextBootstrappingInitializer.unregister(this);
		beanFactoryReference.release();
		parametersReference.set(null);
		initialized = false;
	}

}
