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
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.event.ApplicationEventMulticaster;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.SimpleApplicationEventMulticaster;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;
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
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.ApplicationListener
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.annotation.AnnotationConfigApplicationContext
 * @see org.springframework.context.event.ApplicationEventMulticaster
 * @see org.springframework.context.event.ContextRefreshedEvent
 * @see org.springframework.context.event.SimpleApplicationEventMulticaster
 * @see org.springframework.context.support.ClassPathXmlApplicationContext
 * @see com.gemstone.gemfire.cache.Declarable
 * @since 1.3.4
 * @link http://pubs.vmware.com/vfabric53/topic/com.vmware.vfabric.gemfire.7.0/basic_config/the_cache/setting_cache_initializer.html
 * @link https://jira.springsource.org/browse/SGF-248
 */
@SuppressWarnings("unused")
public class SpringContextBootstrappingInitializer implements Declarable, ApplicationListener<ContextRefreshedEvent> {

	public static final String BASE_PACKAGES_PARAMETER = "basePackages";
	public static final String CONTEXT_CONFIG_LOCATIONS_PARAMETER = "contextConfigLocations";

	protected static final String CHARS_TO_DELETE = " \n\t";
	protected static final String COMMA_DELIMITER = ",";

	/* package-private */ static volatile ConfigurableApplicationContext applicationContext;

	// TODO consider whether I should register a TaskExecutor to perform the event notifications in a separate Thread???
	private static final ApplicationEventMulticaster eventNotifier = new SimpleApplicationEventMulticaster();

	private static ContextRefreshedEvent contextRefreshedEvent;

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
	 * @param <T> the Class type of the Spring ApplicationListener.
	 * @param listener the ApplicationListener to register for ContextRefreshedEvents by this
	 * SpringContextBootstrappingInitializer.
	 * @return the reference to the ApplicationListener for method call chaining purposes.
	 * @see #unregister(org.springframework.context.ApplicationListener)
	 * @see org.springframework.context.ApplicationListener
	 * @see org.springframework.context.event.ContextRefreshedEvent
	 * @see org.springframework.context.event.SimpleApplicationEventMulticaster
	 * 	#addApplicationListener(org.springframework.context.ApplicationListener)
	 */
	public static <T extends ApplicationListener<ContextRefreshedEvent>> T register(final T listener) {
		synchronized (eventNotifier) {
			eventNotifier.addApplicationListener(listener);
			notifyListenerOfExistingContextRefreshedEvent(listener);
		}

		return listener;
	}

	/**
	 * Notifies any Spring ApplicationListeners of a current and existing ContextRefreshedEvent if the
	 * ApplicationContext was previously created, initialized and refreshed before any ApplicationListeners interested
	 * in ContextRefreshedEvents get registered so that application components (such as LazyWiringDeclarableSupport
	 * objects) arriving late to the game that also require configuration (auto-wiring) get wired accordingly too.
	 * <p/>
	 * @param listener a Spring ApplicationListener requiring notification of any ContextRefreshedEvents after the
	 * ApplicationContext has already been created, initialized and/or refreshed.
	 * @see org.springframework.context.ApplicationListener#onApplicationEvent(org.springframework.context.ApplicationEvent)
	 * @see org.springframework.context.event.ContextRefreshedEvent
	 */
	protected static void notifyListenerOfExistingContextRefreshedEvent(
			final ApplicationListener<ContextRefreshedEvent> listener) {
		synchronized (eventNotifier) {
			// NOTE the null check on the ApplicationContext is not absolutely necessary, but is an extra safety
			// precaution none-the-less.
			if (applicationContext != null && contextRefreshedEvent != null) {
				listener.onApplicationEvent(contextRefreshedEvent);
			}
		}
	}

	/**
	 * Unregisters the Spring ApplicationListener from this SpringContextBootstrappingInitializer in order to stop
	 * receiving ApplicationEvents on Spring context refreshes.
	 * <p/>
	 * @param <T> the Class type of the Spring ApplicationListener.
	 * @param listener the ApplicationListener to unregister from receiving ContextRefreshedEvents by this
	 * SpringContextBootstrappingInitializer.
	 * @return the reference to the ApplicationListener for method call chaining purposes.
	 * @see #register(org.springframework.context.ApplicationListener)
	 * @see org.springframework.context.ApplicationListener
	 * @see org.springframework.context.event.ContextRefreshedEvent
	 * @see org.springframework.context.event.SimpleApplicationEventMulticaster
	 * 	#removeApplicationListener(org.springframework.context.ApplicationListener)
	 */
	public static <T extends ApplicationListener<ContextRefreshedEvent>> T unregister(final T listener) {
		synchronized (eventNotifier) {
			eventNotifier.removeApplicationListener(listener);
		}

		return listener;
	}

	/**
	 * Creates (constructs and configures) a ConfigurableApplicationContext instance based on the specified locations
	 * of the context configuration meta-data files.  The created ConfigurableApplicationContext is not automatically
	 * "refreshed" and therefore must be "refreshed" by the caller manually.
	 * <p/>
	 * @param configLocations a String array indicating the locations of the context configuration meta-data files
	 * used to configure the ConfigurableApplicationContext instance.
	 * @return a newly constructed and configured instance of the ConfigurableApplicationContext class.  Note, the
	 * "refresh" method must be called manually before using the context.
	 * @throws IllegalArgumentException if the configLocations parameter argument is null or empty.
	 * @see #createApplicationContext(String[], String[])
	 * @see org.springframework.context.support.ClassPathXmlApplicationContext
	 */
	protected ConfigurableApplicationContext createApplicationContext(final String[] configLocations) {
		Assert.notEmpty(configLocations, "'configLocations' must be specified to construct an instance"
			+ " of the ClassPathXmlApplicationContext.");
		return createApplicationContext(null, configLocations);
	}

	/**
	 * Creates (constructs and configures) an instance of the ConfigurableApplicationContext based on either the
	 * specified base packages containing @Configuration, @Component or JSR 330 annotated classes to scan, or the
	 * specified locations of context configuration meta-data files used to configure the context.  The created
	 * ConfigurableApplicationContext is not automatically "refreshed" and therefore must be "refreshed"
	 * by the caller manually.
	 * <p/>
	 * When basePackages are specified, an instance of AnnotationConfigApplicationContext is returned; otherwise
	 * an instance of the ClassPathXmlApplicationContext is initialized with the configLocations and returned.
	 * This method prefers the ClassPathXmlApplicationContext to the AnnotationConfigApplicationContext when both
	 * basePackages and configLocations are specified.
	 * <p/>
	 * @param basePackages the base application packages to scan for application @Components and @Configuration classes.	 *
	 * @param configLocations a String array indicating the locations of the context configuration meta-data files
	 * used to configure the ConfigurableApplicationContext instance.
	 * @return an instance of ConfigurableApplicationContext configured and initialized with either configLocations
	 * or the basePackages when configLocations is unspecified.  Note, the "refresh" method must be called manually
	 * before using the context.
	 * @throws IllegalArgumentException if both the basePackages and configLocation parameter arguments
	 * are null or empty.
	 * @see #createApplicationContext(String[])
	 * @see org.springframework.context.annotation.AnnotationConfigApplicationContext
	 * @see org.springframework.context.annotation.AnnotationConfigApplicationContext#scan(String...)
	 * @see org.springframework.context.support.ClassPathXmlApplicationContext
	 */
	protected ConfigurableApplicationContext createApplicationContext(final String[] basePackages,
			final String[] configLocations) {
		if (!ObjectUtils.isEmpty(configLocations)) {
			return new ClassPathXmlApplicationContext(configLocations, false);
		}
		else {
			Assert.notEmpty(basePackages, "Either 'basePackages' or 'configLocations' must be specified"
				+ " to construct an instance of the ConfigurableApplicationContext.");
			AnnotationConfigApplicationContext applicationContext = new AnnotationConfigApplicationContext();
			applicationContext.scan(basePackages);
			return applicationContext;
		}
	}

	/**
	 * Initializes a Spring ApplicationContext with the given parameters from a GemFire Initializer in GemFire native
	 * configuration meta-data (e.g. cache.xml).
	 * <p/>
	 * @param parameters a Properties object containing the configuration parameters and settings defined in the
	 * GemFire cache.xml &gt;initializer/&lt; element.
	 * @see #createApplicationContext
	 * @see java.util.Properties
	 */
	@Override
	@SuppressWarnings("null")
	public void init(final Properties parameters) {
		String basePackages = parameters.getProperty(BASE_PACKAGES_PARAMETER);
		String contextConfigLocations = parameters.getProperty(CONTEXT_CONFIG_LOCATIONS_PARAMETER);

		Assert.isTrue(StringUtils.hasText(basePackages) || StringUtils.hasText(contextConfigLocations),
			"Either 'basePackages' or the 'contextConfigLocations' parameter must be specified.");

		String[] basePackagesArray = StringUtils.delimitedListToStringArray(basePackages,
			COMMA_DELIMITER, CHARS_TO_DELETE);

		String[] configLocations = StringUtils.delimitedListToStringArray(contextConfigLocations,
			COMMA_DELIMITER, CHARS_TO_DELETE);

		synchronized (SpringContextBootstrappingInitializer.class) {
			Assert.state(applicationContext == null, String.format(
				"A Spring application context with ID (%1$s) has already been created.",
					nullSafeGetApplicationContextId(applicationContext)));

			applicationContext = createApplicationContext(basePackagesArray, configLocations);
			Assert.notNull(applicationContext, "The 'created' ConfigurableApplicationContext cannot be null!");
			applicationContext.addApplicationListener(this);
			applicationContext.registerShutdownHook();
			applicationContext.refresh();

			Assert.state(applicationContext.isActive(), String.format(
				"The Spring application context (%1$s) has failed to be properly initialized with the following config files (%2$s) or base packages (%3$s)!",
					nullSafeGetApplicationContextId(applicationContext), Arrays.toString(configLocations),
						Arrays.toString(basePackagesArray)));
		}
	}

	/**
	 * Gets the the ID of the Spring ApplicationContext in a null-safe manner.
	 * <p/>
	 * @param applicationContext the Spring ApplicationContext to retrieve the ID for.
	 * @return the ID of the given Spring ApplicationContext or null if the ApplicationContext reference is null.
	 * @see org.springframework.context.ApplicationContext#getId()
	 */
	protected String nullSafeGetApplicationContextId(final ApplicationContext applicationContext) {
		return (applicationContext != null ? applicationContext.getId() : null);
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
		synchronized (eventNotifier) {
			contextRefreshedEvent = event;
			eventNotifier.multicastEvent(event);
		}
	}

}
