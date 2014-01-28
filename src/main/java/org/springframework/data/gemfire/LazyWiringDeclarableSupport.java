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

import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
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
 * <p/>
 * @author John Blum
 * @see org.springframework.context.ApplicationListener
 * @see org.springframework.context.event.ContextRefreshedEvent
 * @see org.springframework.data.gemfire.DeclarableSupport
 * @see org.springframework.data.gemfire.WiringDeclarableSupport
 * @see com.gemstone.gemfire.cache.Declarable
 * @since 1.3.4
 */
@SuppressWarnings("unused")
public abstract class LazyWiringDeclarableSupport implements ApplicationListener<ContextRefreshedEvent>, Declarable {

	// The name of the template bean defined in the Spring context for wiring this Declarable instance.
	protected static final String BEAN_NAME_PARAMETER = "bean-name";

	// atomic reference to the parameter passed by GemFire when this Declared was constructed
	// and the init method was called.
	private final AtomicReference<Properties> parametersReference = new AtomicReference<Properties>();

	protected volatile boolean initialized = false;

	/**
	 * Constructs an instance of the LazyWiringDeclarableSupport class registered with the
	 * SpringContextBootstrappingInitializer to receive notification when the Spring context is created and initialized
	 * (refreshed) by GemFire in order for this Declarable component to be configured and properly initialized with any
	 * required Spring bean dependencies.
	 * <p/>
	 * @see org.springframework.data.gemfire.support.SpringContextBootstrappingInitializer
	 * 	#register(org.springframework.context.ApplicationListener)
	 */
	public LazyWiringDeclarableSupport() {
		SpringContextBootstrappingInitializer.register(this);
	}

	/**
	 * Asserts that this Declarable object has been properly configured and initialized by the Spring container
	 * after GemFire has constructed this Declarable object during startup.  It is recommended that this method
	 * be called in any GemFire CacheCallback/Declarable object operational method (e.g. CacheLoader.load(..))
	 * before use in order to ensure that this Declarable was properly constructed, configured and initialized.
	 * <p/>
	 * @throws IllegalStateException if the Declarable object has not been properly configured or initialized
	 * by the Spring container.
	 * @see #init(java.util.Properties)
	 * @see #isInitialized()
	 */
	protected void assertInitialized() {
		Assert.state(isInitialized(), String.format(
			"This Declarable object (%1$s) has not ben properly configured and initialized!",
				getClass().getName()));
	}

	/**
	 * Performs the actual configuration and initialization of this Declarable object before use.  This method
	 * is triggered by an ApplicationEvent (specifically, the ContextRefreshedEvent) indicating that the Spring context
	 * has been created and refreshed.
	 * <p/>
	 * @param beanFactory the ConfigurableListableBeanFactory used to configure and initialize this Declarable GemFire
	 * component.
	 * @param parameters Properties instance containing the parameters from GemFire's configuration file
	 * (e.g. cache.xml) to configure and initialize this Declarable object.
	 * @throws IllegalArgumentException if the bean-name parameter was specified in GemFire configuration meta-data
	 * but no bean with the specified name could be found in the Spring context.
	 * @see #init(java.util.Properties)
	 * @see org.springframework.beans.factory.wiring.BeanConfigurerSupport
	 * @see org.springframework.beans.factory.wiring.BeanWiringInfo
	 * @see org.springframework.beans.factory.wiring.BeanWiringInfoResolver
	 */
	/* package-private */ void doInit(final ConfigurableListableBeanFactory beanFactory, final Properties parameters) {
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

		doPostInit(parameters);
	}

	/**
	 * Default no operation method performed post initialization of this Declarable GemFire component to be overridden
	 * by subclasses for application specific extension and behavior.
	 * <p/>
	 * @param parameters Properties instance containing the parameters from GemFire's configuration file
	 * (e.g. cache.xml) to configure and initialize this Declarable object.
	 * @see #doInit(org.springframework.beans.factory.config.ConfigurableListableBeanFactory, java.util.Properties)
	 */
	protected void doPostInit(final Properties parameters) {
	}

	/**
	 * Initialization method called by GemFire with the configured parameters once this Declarable object has been
	 * constructed during GemFire startup using an &lt;initalizer&gt; element in GemFire's configuration meta-data.
	 * <p/>
	 * @param parameters the configured parameters passed from the GemFire configuration (e.g. cache.xml) to this
	 * Declarable as a Properties instance.
	 * @throws IllegalStateException if the Declarable object's init method has already been invoked.
	 * @see #doInit(org.springframework.beans.factory.config.ConfigurableListableBeanFactory, java.util.Properties)
	 * @see java.util.Properties
	 */
	@Override
	public final void init(final Properties parameters) {
		Assert.state(parametersReference.compareAndSet(null, parameters), String.format(
			"This Declarable (%1$s) has already been initialized.", getClass().getName()));
	}

	/**
	 * Determines whether this Declarable object has been configured and initialized (i.e. the doInit method
	 * has been called) by the Spring container.
	 * <p/>
	 * @return a boolean value indicating whether this Declarable object has been configured and initialized by
	 * the Spring container.
	 * @see #assertInitialized()
	 * @see #doInit(org.springframework.beans.factory.config.ConfigurableListableBeanFactory, java.util.Properties)
	 */
	protected boolean isInitialized() {
		return initialized;
	}

	/**
	 * Event handler method called when GemFire has created and initialized (refreshed) the Spring ApplicationContext
	 * using the SpringContextBootstrappingInitializer Declarable class.
	 * <p/>
	 * @param event the ContextRefreshedEvent published by the Spring ApplicationContext after it is successfully
	 * created and initialized by GemFire.
	 * @throws IllegalStateException if the parameters have not been passed to this Declarable (i.e. GemFire has not
	 * called this Declarable object's init method yet, which is probably a bug and violates the lifecycle contract
	 * of Declarable GemFire objects).
	 * @throws IllegalArgumentException if the ApplicationContext is not an instance of ConfigurableApplicationContext.
	 * @see #doInit(org.springframework.beans.factory.config.ConfigurableListableBeanFactory, java.util.Properties)
	 * @see org.springframework.context.ApplicationListener#onApplicationEvent(org.springframework.context.ApplicationEvent)
	 * @see org.springframework.context.event.ContextRefreshedEvent
	 */
	@Override
	public final void onApplicationEvent(final ContextRefreshedEvent event) {
		Properties parameters = parametersReference.get();

		Assert.state(parameters != null, String.format(
			"This Declarable object's (%1$s) init method has not been invoked!", getClass().getName()));

		Assert.isTrue(event.getApplicationContext() instanceof ConfigurableApplicationContext,
			String.format("The Spring ApplicationContext (%1$s) must be an instance of ConfigurableApplicationContext.",
				ObjectUtils.nullSafeClassName(event.getApplicationContext())));

		doInit(((ConfigurableApplicationContext) event.getApplicationContext()).getBeanFactory(), parameters);
	}

}
