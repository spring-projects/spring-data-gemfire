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

package org.springframework.data.gemfire.support;

import java.util.Arrays;
import java.util.Properties;

import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationListener;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.event.ApplicationEventMulticaster;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.SimpleApplicationEventMulticaster;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.Declarable;

/**
 * The SpringContextBootstrappingInitializer class is a GemFire configuration initializer used to bootstrap a Spring
 * ApplicationContext inside a GemFire Server JVM-based process.  This enables a GemFire Cache Server resources to be
 * mostly configured with Spring Data GemFire's XML namespace.  The Cache itself is the only resource that cannot be
 * configured and initialized in a Spring context since the initializer is not invoked until after GemFire creates
 * and initializes the Cache for use.
 * <p/>
 * @author John Blum
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.support.ClassPathXmlApplicationContext
 * @see com.gemstone.gemfire.cache.Declarable
 * @since 1.3.4
 * @link http://pubs.vmware.com/vfabric53/topic/com.vmware.vfabric.gemfire.7.0/basic_config/the_cache/setting_cache_initializer.html
 */
@SuppressWarnings("unused")
public class SpringContextBootstrappingInitializer implements Declarable, ApplicationListener<ContextRefreshedEvent> {

	public static final String CONTEXT_CONFIG_LOCATIONS_PARAMETER = "contextConfigLocations";

	private static ConfigurableApplicationContext applicationContext;

	// TODO consider whether I should register a TaskExecutor to perform the event notifications in a separate Thread???
	private static ApplicationEventMulticaster eventNotifier = new SimpleApplicationEventMulticaster();

	/**
	 * Gets a reference to the Spring ApplicationContext constructed, configured and initialized inside the GemFire
	 * Server-based JVM process.
	 * <p/>
	 * @return a reference to the Spring ApplicationContext bootstrapped by GemFire.
	 * @see org.springframework.context.ConfigurableApplicationContext
	 */
	public static synchronized ConfigurableApplicationContext getApplicationContext() {
		Assert.state(applicationContext != null, "The Spring ApplicationContext has not been created!");
		return applicationContext;
	}

	/**
	 * Registers a Spring ApplicationListener to be notified when the Spring ApplicationContext is created by GemFire
	 * when instantiating and initializing declared Initializers from the GemFire native configuration file
	 * (e.g. cache.xml).
	 * <p/>
	 * @param listener the ApplicationListener to register for ContextStartedEvents by this
	 * SpringContextBootstrappingInitializer.
	 * @see org.springframework.context.ApplicationListener
	 * @see org.springframework.context.event.SimpleApplicationEventMulticaster
	 * 	#addApplicationListener(org.springframework.context.ApplicationListener)
	 */
	public static void register(ApplicationListener listener) {
		eventNotifier.addApplicationListener(listener);
	}

	/**
	 * Gets the the ID of the Spring ApplicationContext in a null-safe manner.
	 * <p/>
	 * @param applicationContext the Spring ApplicationContext to retrieve the ID of.
	 * @return the ID of the given Spring ApplicationContext or null if the ApplicationContext reference is null.
	 * @see org.springframework.context.ApplicationContext#getId()
	 */
	protected static String nullSafeGetApplicationContextId(final ApplicationContext applicationContext) {
		return (applicationContext != null ? applicationContext.getId() : null);
	}

	/**
	 * Initializes a Spring ApplicationContext with the given parameters from a GemFire Initializer in GemFire native
	 * configuration meta-data (e.g. cache.xml).
	 * <p/>
	 * @param parameters a Properties object containing the configuration parameters and settings defined in the
	 * GemFire cache.xml &gt;initializer/&lt; element.
	 * @see java.util.Properties
	 */
	@Override
	public void init(final Properties parameters) {
		String contextConfigLocations = parameters.getProperty(CONTEXT_CONFIG_LOCATIONS_PARAMETER);

		Assert.hasText(contextConfigLocations, "The contextConfigLocations parameter is required.");

		String[] configLocations = contextConfigLocations.split(",");

		StringUtils.trimArrayElements(configLocations);

		synchronized (SpringContextBootstrappingInitializer.class) {
			Assert.state(applicationContext == null, String.format(
				"A Spring application context with ID (%1$s) has already been created.",
					nullSafeGetApplicationContextId(applicationContext)));

			applicationContext = new ClassPathXmlApplicationContext(configLocations, false);
			applicationContext.addApplicationListener(this);
			applicationContext.registerShutdownHook();
			applicationContext.refresh();

			Assert.state(applicationContext.isActive(), String.format(
				"The Spring application context has failed to be properly initialized with the following config files (%1$s)!",
					Arrays.toString(configLocations)));
		}
	}

	/**
	 * Gets notified when the Spring ApplicationContext gets created and refreshed by GemFire.  The handler method
	 * proceeds in notifying any other GemFire components that need to be aware that the Spring ApplicationContext
	 * now exists and is ready for use, such as other Declarable GemFire objects requiring auto-wiring support, etc.
	 * <p/>
	 * @param event the ContextRefreshedEvent signaling that the Spring ApplicationContext has been created
	 * and refreshed by GemFire.
	 * @see org.springframework.context.event.ContextRefreshedEvent
	 * @see org.springframework.context.event.ApplicationEventMulticaster
	 * 	#multicastEvent(org.springframework.context.ApplicationEvent)
	 */
	@Override
	public void onApplicationEvent(final ContextRefreshedEvent event) {
		eventNotifier.multicastEvent(event);
	}

}
