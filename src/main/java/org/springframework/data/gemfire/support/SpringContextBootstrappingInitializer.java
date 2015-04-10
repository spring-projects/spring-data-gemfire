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
import java.util.Collection;
import java.util.List;
import java.util.Properties;
import java.util.concurrent.CopyOnWriteArrayList;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextException;
import org.springframework.context.ApplicationListener;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.event.ApplicationContextEvent;
import org.springframework.context.event.ApplicationEventMulticaster;
import org.springframework.context.event.ContextClosedEvent;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.context.event.SimpleApplicationEventMulticaster;
import org.springframework.context.support.ClassPathXmlApplicationContext;
import org.springframework.data.gemfire.util.CollectionUtils;
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
 *
 * @author John Blum
 * @see org.springframework.context.ApplicationContext
 * @see org.springframework.context.ApplicationListener
 * @see org.springframework.context.ConfigurableApplicationContext
 * @see org.springframework.context.annotation.AnnotationConfigApplicationContext
 * @see org.springframework.context.support.ClassPathXmlApplicationContext
 * @see com.gemstone.gemfire.cache.Declarable
 * @see <a href="http://gemfire.docs.pivotal.io/latest/userguide/index.html#basic_config/the_cache/setting_cache_initializer.html">Setting Cache Initializer</a>
 * @see <a href="https://jira.springsource.org/browse/SGF-248">SGF-248</a>
 * @since 1.4.0
 */
@SuppressWarnings("unused")
public class SpringContextBootstrappingInitializer implements Declarable, ApplicationListener<ApplicationContextEvent> {

	public static final String BASE_PACKAGES_PARAMETER = "basePackages";
	public static final String CONTEXT_CONFIG_LOCATIONS_PARAMETER = "contextConfigLocations";

	protected static final String CHARS_TO_DELETE = " \n\t";
	protected static final String COMMA_DELIMITER = ",";

	private static final ApplicationEventMulticaster applicationEventNotifier = new SimpleApplicationEventMulticaster();

	/* package-private */ static volatile ConfigurableApplicationContext applicationContext;

	/* package-private */ static volatile ContextRefreshedEvent contextRefreshedEvent;

	private static final List<Class<?>> registeredAnnotatedClasses = new CopyOnWriteArrayList<Class<?>>();

	protected final Log logger = initLogger();

	/**
	 * Gets a reference to the Spring ApplicationContext constructed, configured and initialized inside the GemFire
	 * Server-based JVM process.
	 *
	 * @return a reference to the Spring ApplicationContext bootstrapped by GemFire.
	 * @see org.springframework.context.ConfigurableApplicationContext
	 */
	public static synchronized ConfigurableApplicationContext getApplicationContext() {
		Assert.state(applicationContext != null, "The Spring ApplicationContext was not configured and initialized properly!");
		return applicationContext;
	}

	/**
	 * Notifies any Spring ApplicationListeners of a current and existing ContextRefreshedEvent if the
	 * ApplicationContext had been previously created, initialized and refreshed before any ApplicationListeners
	 * interested in ContextRefreshedEvents were registered so that application components (such as the
	 * GemFire CacheLoaders extending LazyWiringDeclarableSupport objects) registered late, requiring configuration
	 * (auto-wiring), also get notified and wired accordingly.
	 *
	 * @param listener a Spring ApplicationListener requiring notification of any ContextRefreshedEvents after the
	 * ApplicationContext has already been created, initialized and/or refreshed.
	 * @see org.springframework.context.ApplicationListener#onApplicationEvent(org.springframework.context.ApplicationEvent)
	 * @see org.springframework.context.event.ContextRefreshedEvent
	 */
	protected static void notifyOnExistingContextRefreshedEvent(ApplicationListener<ContextRefreshedEvent> listener) {
		synchronized (applicationEventNotifier) {
			if (contextRefreshedEvent != null) {
				listener.onApplicationEvent(contextRefreshedEvent);
			}
		}
	}

	/**
	 * Registers a Spring ApplicationListener to be notified when the Spring ApplicationContext is created by GemFire
	 * when instantiating and initializing Declarables declared inside the &lt;initializer&gt; block inside GemFire's
	 * cache.xml file.
	 *
	 * @param <T> the Class type of the Spring ApplicationListener.
	 * @param listener the ApplicationListener to register for ContextRefreshedEvents multi-casted by this
	 * SpringContextBootstrappingInitializer.
	 * @return the reference to the ApplicationListener for method call chaining purposes.
	 * @see #notifyOnExistingContextRefreshedEvent(org.springframework.context.ApplicationListener)
	 * @see #unregister(org.springframework.context.ApplicationListener)
	 * @see org.springframework.context.ApplicationListener
	 * @see org.springframework.context.event.ContextRefreshedEvent
	 * @see org.springframework.context.event.SimpleApplicationEventMulticaster
	 * 	#addApplicationListener(org.springframework.context.ApplicationListener)
	 */
	public static <T extends ApplicationListener<ContextRefreshedEvent>> T register(T listener) {
		synchronized (applicationEventNotifier) {
			applicationEventNotifier.addApplicationListener(listener);
			notifyOnExistingContextRefreshedEvent(listener);
		}

		return listener;
	}

	/**
	 * Registers the specified Spring annotated POJO class, which will be used to configure and initialize
	 * the Spring ApplicationContext.
	 *
	 * @param annotatedClass the Spring annotated (@Configuration) POJO class to register.
	 * @return a boolean value indicating whether the Spring annotated POJO class was successfully registered.
	 * @see #unregister(Class)
	 */
	public static boolean register(Class<?> annotatedClass) {
		Assert.notNull(annotatedClass, "the Spring annotated class to register must not be null");
		return registeredAnnotatedClasses.add(annotatedClass);
	}

	/**
	 * Un-registers the Spring ApplicationListener from this SpringContextBootstrappingInitializer in order to stop
	 * receiving ApplicationEvents on Spring context refreshes.
	 *
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
	public static <T extends ApplicationListener<ContextRefreshedEvent>> T unregister(T listener) {
		synchronized (applicationEventNotifier) {
			applicationEventNotifier.removeApplicationListener(listener);
		}

		return listener;
	}

	/**
	 * Un-registers the specified Spring annotated POJO class used to configure and initialize
	 * the Spring ApplicationContext.
	 *
	 * @param annotatedClass the Spring annotated (@Configuration) POJO class to unregister.
	 * @return a boolean value indicating whether the Spring annotated POJO class was successfully un-registered.
	 * @see #register(Class)
	 */
	public static boolean unregister(Class<?> annotatedClass) {
		return registeredAnnotatedClasses.remove(annotatedClass);
	}

	/**
	 * Initialization method for the logger used to log important messages from this initializer.
	 *
	 * @return a Apache Commons Log used to log messages from this initializer
	 * @see org.apache.commons.logging.LogFactory#getLog(Class)
	 * @see org.apache.commons.logging.Log
	 */
	protected Log initLogger() {
		return LogFactory.getLog(getClass());
	}

	/**
	 * Creates (constructs and configures) an instance of the ConfigurableApplicationContext based on either the
	 * specified base packages containing @Configuration, @Component or JSR 330 annotated classes to scan, or the
	 * specified locations of context configuration meta-data files.  The created ConfigurableApplicationContext
	 * is not automatically "refreshed" and therefore must be "refreshed" by the caller manually.
	 *
	 * When basePackages are specified, an instance of AnnotationConfigApplicationContext is constructed and a scan
	 * is performed; otherwise an instance of the ClassPathXmlApplicationContext is initialized with the
	 * configLocations.  This method prefers the ClassPathXmlApplicationContext to the
	 * AnnotationConfigApplicationContext when both basePackages and configLocations are specified.
	 *
	 * @param basePackages the base packages to scan for application @Components and @Configuration classes.
	 * @param configLocations a String array indicating the locations of the context configuration meta-data files
	 * used to configure the ClassPathXmlApplicationContext instance.
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
	protected ConfigurableApplicationContext createApplicationContext(String[] basePackages, String[] configLocations) {
		if (!ObjectUtils.isEmpty(configLocations)) {
			return createApplicationContext(configLocations);
		}
		else {
			Assert.isTrue(isConfigurable(basePackages, configLocations, registeredAnnotatedClasses),
				"'basePackages', 'configLocations' or 'AnnotatedClasses' must be specified in order to"
					+ " construct and configure an instance of the ConfigurableApplicationContext");

			return scanBasePackages(registerAnnotatedClasses((AnnotationConfigApplicationContext) createApplicationContext(null),
				registeredAnnotatedClasses.toArray(new Class<?>[registeredAnnotatedClasses.size()])), basePackages);
		}
	}

	/* (non-Javadoc) - used for testing purposes only */
	ConfigurableApplicationContext createApplicationContext(String[] configLocations) {
		return (ObjectUtils.isEmpty(configLocations) ? new AnnotationConfigApplicationContext()
			: new ClassPathXmlApplicationContext(configLocations, false));
	}

	/**
	 * Initializes the given ApplicationContext by registering this SpringContextBootstrappingInitializer as an
	 * ApplicationListener and registering a runtime shutdown hook.
	 *
	 * @param applicationContext the ConfigurableApplicationContext to initialize.
	 * @return the initialized ApplicationContext.
	 * @see org.springframework.context.ConfigurableApplicationContext
	 * @see org.springframework.context.ConfigurableApplicationContext#addApplicationListener(org.springframework.context.ApplicationListener)
	 * @see org.springframework.context.ConfigurableApplicationContext#registerShutdownHook()
	 * @throws java.lang.IllegalArgumentException if the ApplicationContext reference is null!
	 */
	protected ConfigurableApplicationContext initApplicationContext(ConfigurableApplicationContext applicationContext) {
		Assert.notNull(applicationContext, "The ConfigurableApplicationContext reference must not be null!");
		applicationContext.addApplicationListener(this);
		applicationContext.registerShutdownHook();
		return applicationContext;
	}

	/**
	 * Refreshes the given ApplicationContext making the context active.
	 *
	 * @param applicationContext the ConfigurableApplicationContext to refresh.
	 * @return the refreshed ApplicationContext.
	 * @see org.springframework.context.ConfigurableApplicationContext
	 * @see org.springframework.context.ConfigurableApplicationContext#refresh()
	 * @throws java.lang.IllegalArgumentException if the ApplicationContext reference is null!
	 */
	protected ConfigurableApplicationContext refreshApplicationContext(ConfigurableApplicationContext applicationContext) {
		Assert.notNull(applicationContext, "The ConfigurableApplicationContext reference must not be null!");
		applicationContext.refresh();
		return applicationContext;
	}

	/* (non-Javadoc) */
	private boolean isConfigurable(String[] basePackages, String[] contextConfigLocations,
			Collection<Class<?>> annotatedClasses) {
		return !(ObjectUtils.isEmpty(basePackages) && ObjectUtils.isEmpty(contextConfigLocations)
			&& CollectionUtils.isEmpty(annotatedClasses));
	}

	/**
	 * Null-safe operation used to get the ID of the Spring ApplicationContext.
	 *
	 * @param applicationContext the Spring ApplicationContext from which to get the ID.
	 * @return the ID of the given Spring ApplicationContext or null if the ApplicationContext reference is null.
	 * @see org.springframework.context.ApplicationContext#getId()
	 */
	String nullSafeGetApplicationContextId(ApplicationContext applicationContext) {
		return (applicationContext != null ? applicationContext.getId() : null);
	}

	/**
	 * Registers the given Spring annotated (@Configuration) POJO classes with the specified
	 * AnnotationConfigApplicationContext.
	 *
	 * @param applicationContext the AnnotationConfigApplicationContext used to register the Spring annotated,
	 * POJO classes.
	 * @param annotatedClasses a Class array of Spring annotated (@Configuration) classes used to configure
	 * and initialize the Spring AnnotationConfigApplicationContext.
	 * @return the given AnnotationConfigApplicationContext.
	 * @see org.springframework.context.annotation.AnnotationConfigApplicationContext#register(Class[])
	 */
	AnnotationConfigApplicationContext registerAnnotatedClasses(AnnotationConfigApplicationContext applicationContext,
			Class<?>[] annotatedClasses) {
		if (!ObjectUtils.isEmpty(annotatedClasses)) {
			applicationContext.register(annotatedClasses);
		}

		return applicationContext;
	}

	/**
	 * Configures classpath component scanning using the specified base packages on the specified
	 * AnnotationConfigApplicationContext.
	 *
	 * @param applicationContext the AnnotationConfigApplicationContext to setup with classpath component scanning
	 * using the specified base packages.
	 * @param basePackages an array of Strings indicating the base packages to use in the classpath component scan.
	 * @return the given AnnotationConfigApplicationContext.
	 * @see org.springframework.context.annotation.AnnotationConfigApplicationContext#scan(String...)
	 */
	AnnotationConfigApplicationContext scanBasePackages(AnnotationConfigApplicationContext applicationContext,
			String[] basePackages) {
		if (!ObjectUtils.isEmpty(basePackages)) {
			applicationContext.scan(basePackages);
		}

		return applicationContext;
	}

	/**
	 * Initializes a Spring ApplicationContext with the given parameters specified with a GemFire &lt;initializer&gt;
	 * block in cache.xml.
	 *
	 * @param parameters a Properties object containing the configuration parameters and settings defined in the
	 * GemFire cache.xml &lt;initializer&gt; block for the declared SpringContextBootstrappingInitializer
	 * GemFire Declarable object.
	 * @throws org.springframework.context.ApplicationContextException if the Spring ApplicationContext could not be
	 * successfully created, configured and initialized.
	 * @see #createApplicationContext(String[], String[])
	 * @see #initApplicationContext(org.springframework.context.ConfigurableApplicationContext)
	 * @see #refreshApplicationContext(org.springframework.context.ConfigurableApplicationContext)
	 * @see java.util.Properties
	 */
	@Override
	public void init(final Properties parameters) {
		try {
			synchronized (SpringContextBootstrappingInitializer.class) {
				if (applicationContext == null || !applicationContext.isActive()) {
					String basePackages = parameters.getProperty(BASE_PACKAGES_PARAMETER);
					String contextConfigLocations = parameters.getProperty(CONTEXT_CONFIG_LOCATIONS_PARAMETER);

					String[] basePackagesArray = StringUtils.delimitedListToStringArray(
						StringUtils.trimWhitespace(basePackages), COMMA_DELIMITER, CHARS_TO_DELETE);

					String[] contextConfigLocationsArray = StringUtils.delimitedListToStringArray(
						StringUtils.trimWhitespace(contextConfigLocations), COMMA_DELIMITER, CHARS_TO_DELETE);

					ConfigurableApplicationContext localApplicationContext = refreshApplicationContext(
						initApplicationContext(createApplicationContext(basePackagesArray, contextConfigLocationsArray)));

					Assert.state(localApplicationContext.isRunning(), String.format(
						"The Spring ApplicationContext (%1$s) failed to be properly initialized with the context config files (%2$s) or base packages (%3$s)!",
							nullSafeGetApplicationContextId(localApplicationContext), Arrays.toString(contextConfigLocationsArray),
								Arrays.toString(basePackagesArray)));

					applicationContext = localApplicationContext;
				}
			}
		}
		catch (Throwable cause) {
			String message = "Failed to bootstrap the Spring ApplicationContext!";
			logger.error(message, cause);
			throw new ApplicationContextException(message, cause);
		}
	}

	/**
	 * Gets notified when the Spring ApplicationContext gets created and refreshed by GemFire, once the
	 * &lt;initializer&gt; block is processed and the SpringContextBootstrappingInitializer Declarable component
	 * is initialized.  This handler method proceeds in notifying any other GemFire components that need to be aware
	 * that the Spring ApplicationContext now exists and is ready for use, such as other Declarable GemFire objects
	 * requiring auto-wiring support, etc.
	 *
	 * In addition, this method handles the ContextClosedEvent by removing the ApplicationContext reference.
	 *
	 * @param event the ApplicationContextEvent signaling that the Spring ApplicationContext has been created
	 * and refreshed by GemFire, or closed when the JVM process exits.
	 * @see org.springframework.context.event.ContextClosedEvent
	 * @see org.springframework.context.event.ContextRefreshedEvent
	 * @see org.springframework.context.event.ApplicationEventMulticaster
	 *  #multicastEvent(org.springframework.context.ApplicationEvent)
	 */
	@Override
	public void onApplicationEvent(final ApplicationContextEvent event) {
		if (event instanceof ContextRefreshedEvent) {
			synchronized (applicationEventNotifier) {
				contextRefreshedEvent = (ContextRefreshedEvent) event;
				applicationEventNotifier.multicastEvent(event);
			}
		}
		else if (event instanceof ContextClosedEvent) {
			synchronized (applicationEventNotifier) {
				contextRefreshedEvent = null;
			}
		}
	}

}
