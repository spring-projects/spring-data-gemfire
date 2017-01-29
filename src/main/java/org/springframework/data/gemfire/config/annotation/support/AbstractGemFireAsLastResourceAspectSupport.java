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

package org.springframework.data.gemfire.config.annotation.support;

import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newIllegalStateException;
import static org.springframework.data.gemfire.util.RuntimeExceptionFactory.newRuntimeException;

import java.util.Hashtable;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

import javax.naming.Context;
import javax.naming.InitialContext;
import javax.naming.NamingException;
import javax.resource.ResourceException;

import org.apache.geode.cache.GemFireCache;
import org.apache.geode.ra.GFConnection;
import org.apache.geode.ra.GFConnectionFactory;
import org.aspectj.lang.annotation.Pointcut;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.event.Level;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.Ordered;
import org.springframework.data.gemfire.GemfireUtils;
import org.springframework.util.StringUtils;

/**
 * {@link AbstractGemFireAsLastResourceAspectSupport} is an abstract base class encapsulating functionality common
 * to all AOP Aspect extensions/implementations involving the GemFire JCA ResourceAdapter object registered in
 * the JNDI context of a managed environment.
 *
 * @author John Blum
 * @see javax.naming.Context
 * @see org.apache.geode.cache.GemFireCache
 * @see org.apache.geode.ra.GFConnection
 * @see org.slf4j.Logger
 * @see org.springframework.core.Ordered
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractGemFireAsLastResourceAspectSupport implements Ordered {

	protected static final boolean DEFAULT_THROW_ON_ERROR = false;

	protected static final int DEFAULT_ORDER = Integer.MAX_VALUE;

	protected static final Consumer<String> NO_OP_LOGGER = message -> {};

	protected static final String DEFAULT_GEMFIRE_JCA_RESOURCE_ADAPTER_JNDI_NAME = "gfe/jca";

	private boolean throwOnError = DEFAULT_THROW_ON_ERROR;

	private int order = DEFAULT_ORDER;

	@Autowired(required = false)
	private Context context;

	@Autowired(required = false)
	private GemFireCache gemfireCache;

	private final Logger logger = newLogger();

	@Value("${spring.data.gemfire.jca.resource-adapter.jndi.name:"
		+ DEFAULT_GEMFIRE_JCA_RESOURCE_ADAPTER_JNDI_NAME + "}")
	private String gemfireJcaResourceAdapterJndiName;

	@Value("${spring.data.gemfire.naming.context.factory:}")
	private String initialContextFactory;

	@Value("${spring.data.gemfire.naming.context.provider-url:}")
	private String providerUrl;

	/**
	 * Returns a reference to the naming {@link Context}.
	 *
	 * @return a reference to the naming {@link Context}.
	 * @see javax.naming.Context
	 */
	protected synchronized Context getContext() {
		return this.context;
	}

	/**
	 * Returns a reference to the {@link GemFireCache} used to interact with GemFire.
	 *
	 * @param <T> {@link Class} sub-type of the {@link GemFireCache} in use.
	 * @return a reference to the {@link GemFireCache}.
	 * @see org.apache.geode.cache.GemFireCache
	 */
	@SuppressWarnings("unchecked")
	protected synchronized <T extends GemFireCache> T getGemFireCache() {
		return (T) this.gemfireCache;
	}

	/**
	 * Returns the configured reference to GemFire's JCA ResourceAdapter registered in the managed environment's
	 * JNDI context.
	 *
	 * @return a {@link String} containing the configured reference to GemFire's JCA ResourceAdapter
	 * registered in the managed environment's JNDI context.
	 */
	public String getGemFireJcaResourceAdapterJndiName() {
		return this.gemfireJcaResourceAdapterJndiName;
	}

	/**
	 * Returns the configured, fully-qualified classname of the {@link javax.naming.spi.InitialContextFactory}
	 * used to construct the {@link InitialContext} that is then used to lookup managed objects registered
	 * in the JNDI context of the managed environment.
	 *
	 * @return the configured, fully-qualified classname of the {@link javax.naming.spi.InitialContextFactory}
	 * used to construct the {@link InitialContext}.
	 */
	public String getInitialContextFactory() {
		return this.initialContextFactory;
	}

	/**
	 * Returns a reference to the {@link Logger} used to record debug, info, warning and error messages
	 * logged by the application.
	 *
	 * @return a reference to the configured {@link Logger} used by the application for logging purposes.
	 * @see org.slf4j.Logger
	 */
	protected Logger getLogger() {
		return this.logger;
	}

	/**
	 * Sets the order of this AOP Aspect relative to other Aspects in the chain of Aspects configured
	 * in Spring's Transaction Management.
	 *
	 * @param order int value specifying the relative order of this Aspect.
	 * @see org.springframework.core.Ordered
	 */
	public void setOrder(int order) {
		this.order = order;
	}

	/**
	 * Returns the order of this AOP Aspect relative to other Aspects in the chain of Aspects configured
	 * in Spring's Transaction Management.
	 *
	 * @return an int value specifying the relative order of this Aspect.
	 * @see org.springframework.core.Ordered
	 */
	@Override
	public int getOrder() {

		return Optional.of(this.order)
			.filter(order -> !(order == Integer.MAX_VALUE || order == Integer.MIN_VALUE))
			.orElseGet(this::getDefaultOrder);
	}

	/**
	 * Returns the default order used by this AOP Aspect in the chain of Aspects configured
	 * in Spring's Transaction Management.
	 *
	 * @return an int value specifying the default order used by this AOP Aspect in the chain of Aspects
	 * configured in Spring's Transaction Management.
	 */
	protected Integer getDefaultOrder() {
		return DEFAULT_ORDER;
	}

	/**
	 * Returns the URL of the Naming Context Provider as a {@link String}.
	 *
	 * @return the URL of the Naming Context Provider.
	 */
	public String getProviderUrl() {
		return this.providerUrl;
	}

	/**
	 * Determines whether an Exception should be thrown when this Aspect is unable to perform its function.
	 *
	 * Alternatively (and by default) the error condition is simply logged.
	 *
	 * Defaults to {@literal false}.
	 *
	 * @return a boolean value indicating whether this Aspect should throw an Exception when an error occurs
	 * preventing this Aspect from performing its normal function.
	 */
	public boolean isThrowOnError() {
		return this.throwOnError;
	}

	/**
	 * Defines an AOP Pointcut identifying Join Points during the execution of the application's components
	 * on which this Aspect should be applied.
	 *
	 * This particular Pointcut identifies {@link org.springframework.transaction.annotation.Transactional} annotated
	 * application service {@link Class types}.  That is, the {@link org.springframework.transaction.annotation.Transactional}
	 * annotation is used at the class level.
	 *
	 * @see org.aspectj.lang.annotation.Pointcut
	 */
	@Pointcut("@within(org.springframework.transaction.annotation.Transactional)")
	protected void atTransactionalType() {}

	/**
	 * Defines an AOP Pointcut identifying Join Points during the execution of the application's components
	 * on which this Aspect should be applied.
	 *
	 * This particular Pointcut identifies {@link org.springframework.transaction.annotation.Transactional} annotated
	 * application service {@link java.lang.reflect.Method methods}.  That is, the
	 * {@link org.springframework.transaction.annotation.Transactional} annotation is used at the service class,
	 * service {@link java.lang.reflect.Method method} level.
	 *
	 * @see org.aspectj.lang.annotation.Pointcut
	 */
	@Pointcut("@annotation(org.springframework.transaction.annotation.Transactional)")
	protected void atTransactionalMethod() {}

	/**
	 * Formats the given {@link String message} with the provided array of {@link Object arguments}.
	 *
	 * @param message {@link String} containing the message to format.
	 * @param args array of {@link Object arguments} used to format the message.
	 * @return the {@link String message} formatted with the provided array of {@link Object arguments}.
	 * @see java.lang.String#format(String, Object...)
	 */
	protected String format(String message, Object... args) {
		return String.format(message, args);
	}

	/**
	 * Logs the given {@link String message} formatted with the given array of {@link Object arguments}
	 * at {@link Level#DEBUG} level when debugging is enabled.
	 *
	 * @param <T> {@link Class} type extension of {@link AbstractGemFireAsLastResourceAspectSupport}.
	 * @param message {@link String} containing the message to log.
	 * @param args array of {@link Object arguments} used to format the message.
	 * @return this aspect.
	 * @see org.slf4j.Logger#isDebugEnabled()
	 * @see org.slf4j.Logger#debug(String, Object...)
	 * @see #format(String, Object...)
	 */
	@SuppressWarnings("unchecked")
	protected <T extends AbstractGemFireAsLastResourceAspectSupport> T logDebugInfo(String message, Object... args) {

		Logger logger = getLogger();

		if (logger.isDebugEnabled()) {
			logger.debug(format(message, args), args);
		}

		return (T) this;
	}

	/**
	 * Logs the given {@link String message} formatted with the given array of {@link Object arguments}
	 * at {@link Level#INFO} level when info logging is enabled.
	 *
	 * @param <T> {@link Class} type extension of {@link AbstractGemFireAsLastResourceAspectSupport}.
	 * @param message {@link String} containing the message to log.
	 * @param args array of {@link Object arguments} used to format the message.
	 * @return this aspect.
	 * @see org.slf4j.Logger#isInfoEnabled()
	 * @see org.slf4j.Logger#info(String, Object...)
	 * @see #format(String, Object...)
	 */
	@SuppressWarnings("unchecked")
	protected <T extends AbstractGemFireAsLastResourceAspectSupport> T logInfo(String message, Object... args) {

		Logger logger = getLogger();

		if (logger.isInfoEnabled()) {
			logger.info(format(message, args), args);
		}

		return (T) this;
	}

	/**
	 * Logs the given {@link String message} formatted with the given array of {@link Object arguments}
	 * at {@link Level#TRACE} level when tracing is enabled.
	 *
	 * @param <T> {@link Class} type extension of {@link AbstractGemFireAsLastResourceAspectSupport}.
	 * @param message {@link String} containing the message to log.
	 * @param args array of {@link Object arguments} used to format the message.
	 * @return this aspect.
	 * @see java.util.function.Supplier
	 * @see #logTraceInfo(Supplier)
	 */
	protected <T extends AbstractGemFireAsLastResourceAspectSupport> T logTraceInfo(String message, Object... args) {
		return logTraceInfo(() -> format(message, args));
	}

	/**
	 * Logs the given {@link Supplier log message}  at {@link Level#TRACE} level when tracing is enabled.
	 *
	 * @param <T> {@link Class} type extension of {@link AbstractGemFireAsLastResourceAspectSupport}.
	 * @param logMessage {@link Supplier} of the message to log.
	 * @return this aspect.
	 * @see java.util.function.Supplier
	 * @see org.slf4j.Logger#isTraceEnabled()
	 * @see org.slf4j.Logger#trace(String)
	 */
	@SuppressWarnings("unchecked")
	protected <T extends AbstractGemFireAsLastResourceAspectSupport> T logTraceInfo(Supplier<String> logMessage) {

		Logger logger = getLogger();

		if (logger.isTraceEnabled()) {
			logger.trace(logMessage.get());
		}

		return (T) this;
	}

	/**
	 * Logs the given {@link String message} formatted with the given array of {@link Object arguments}
	 * at {@link Level#WARN} level when warnings are enabled.
	 *
	 * @param <T> {@link Class} type extension of {@link AbstractGemFireAsLastResourceAspectSupport}.
	 * @param message {@link String} containing the message to log.
	 * @param args array of {@link Object arguments} used to format the message.
	 * @return this aspect.
	 * @see org.slf4j.Logger#isWarnEnabled()
	 * @see org.slf4j.Logger#warn(String, Object...)
	 * @see #format(String, Object...)
	 */
	@SuppressWarnings("unchecked")
	protected <T extends AbstractGemFireAsLastResourceAspectSupport> T logWarning(String message, Object... args) {

		Logger logger = getLogger();

		if (logger.isWarnEnabled()) {
			logger.warn(format(message, args), args);
		}

		return (T) this;
	}

	/**
	 * Logs the given {@link String message} formatted with the given array of {@link Object arguments}
	 * at {@link Level#ERROR} level when error logging is enabled.
	 *
	 * @param <T> {@link Class} type extension of {@link AbstractGemFireAsLastResourceAspectSupport}.
	 * @param message {@link String} containing the message to log.
	 * @param args array of {@link Object arguments} used to format the message.
	 * @return this aspect.
	 * @see org.slf4j.Logger#isErrorEnabled()
	 * @see org.slf4j.Logger#error(String, Object...)
	 * @see #format(String, Object...)
	 */
	@SuppressWarnings("unchecked")
	protected <T extends AbstractGemFireAsLastResourceAspectSupport> T logError(String message, Object... args) {

		Logger logger = getLogger();

		if (logger.isErrorEnabled()) {
			logger.error(format(message, args), args);
		}

		return (T) this;
	}

	/**
	 * Constructs a new instance of the {@link InitialContext} configured with the given {@link Hashtable environment}.
	 *
	 * @param environment {@link Hashtable} containing environment configuration meta-data used to access
	 * the JNDI context in a managed environment.
	 * @return a new instance of the {@link InitialContext} configured with the given {@link Hashtable environment}.
	 * @throws NamingException if the {@link InitialContext} could not be initialized with
	 * the provided {@link Hashtable environment}.
	 * @see javax.naming.InitialContext
	 * @see java.util.Hashtable
	 */
	protected InitialContext newInitialContext(Hashtable<?, ?> environment) throws NamingException {
		return new InitialContext(environment);
	}

	/**
	 * Constructs a new instance of {@link Logger} used by this application to log messages.
	 *
	 * @return a new, configured instance of {@link Logger}.
	 */
	protected Logger newLogger() {
		return LoggerFactory.getLogger(getClass());
	}

	/**
	 * Resolves the {@link Context} used to perform lookups of registered, managed objects in a management environment.
	 *
	 * @return the {@link Context} used to perform lookups of registered, managed objects in a managed environment.
	 * @throws IllegalStateException if the {@link Context} could not be resolved.
	 * @see org.apache.geode.cache.GemFireCache#getJNDIContext()
	 * @see #newInitialContext(Hashtable)
	 * @see #resolveEnvironment()
	 * @see #resolveGemFireCache()
	 * @see #getContext()
	 * @see javax.naming.Context
	 */
	protected synchronized Context resolveContext() {

		Context context = getContext();

		if (context == null) {

			Hashtable<?, ?> resolvedEnvironment = resolveEnvironment();

			try {
				context = this.context = newInitialContext(resolvedEnvironment);
			}
			catch (NamingException cause) {
				context = this.context = Optional.ofNullable(resolveGemFireCache()).map(GemFireCache::getJNDIContext)
					.orElseThrow(() -> newIllegalStateException(cause,
						"Failed to initialize an %1$s with the provided Environment configuration ['%2$s']",
							InitialContext.class.getName(), resolvedEnvironment));
			}
		}

		return context;
	}

	/**
	 * Resolves the {@link Hashtable environment} used by the application to configure the {@link InitialContext}.
	 *
	 * @return the resolved {@link Hashtable environment} used to configure the {@link InitialContext}.
	 * @see #getInitialContextFactory()
	 * @see #getProviderUrl()
	 * @see java.util.Hashtable
	 */
	protected Hashtable<?, ?> resolveEnvironment() {

		Hashtable<Object, Object> environment = new Hashtable<>();

		environment.put(Context.INITIAL_CONTEXT_FACTORY, getInitialContextFactory());
		environment.put(Context.PROVIDER_URL, getProviderUrl());

		return environment;
	}

	/**
	 * Resolves a reference to the {@link GemFireCache} required by this Aspect to perform its function.
	 *
	 * This method either returns the configured {@link GemFireCache} instance or looks up
	 * the {@link GemFireCache} instance using GemFire's API.
	 *
	 * @return a reference to the resolved {@link GemFireCache} instance.
	 * @see org.springframework.data.gemfire.GemfireUtils#resolveGemFireCache()
	 * @see org.apache.geode.cache.GemFireCache
	 * @see #getGemFireCache()
	 */
	protected synchronized GemFireCache resolveGemFireCache() {

		GemFireCache gemfireCache = getGemFireCache();

		if (gemfireCache == null) {
			gemfireCache = this.gemfireCache = GemfireUtils.resolveGemFireCache();
		}

		return gemfireCache;
	}

	/**
	 * Resolves the configured JNDI name used to lookup and resolve the GemFire JCA ResourceAdapter object
	 * from the JNDI context in the managed environment.
	 *
	 * @return a {@link String} containing the JNDI name used to lookup and resolve the GemFire JCA ResourceAdapter
	 * object from the JNDI context in the managed environment.
	 * @see #getGemFireJcaResourceAdapterJndiName()
	 */
	protected String resolveGemFireJcaResourceAdapterJndiName() {

		String gemfireJcaResourceAdapterJndiName = getGemFireJcaResourceAdapterJndiName();

		return (StringUtils.hasText(gemfireJcaResourceAdapterJndiName) ? gemfireJcaResourceAdapterJndiName
			: DEFAULT_GEMFIRE_JCA_RESOURCE_ADAPTER_JNDI_NAME);
	}

	/**
	 * Builder method used to set the {@link #isThrowOnError() throwOnError} property.
	 *
	 * The {@link #isThrowOnError() throwOnError} property is used to indicate whether an Exception should be thrown
	 * when an error occurs during the normal operation and function of this Aspect.  By default, the error condition
	 * is simply logged.
	 *
	 * @param <T> {@link Class sub-type} of this Aspect.
	 * @param throwOnError boolean value used to set the {@link #isThrowOnError() throwOnError} property.
	 * @return this Aspect.
	 * @see #isThrowOnError()
	 */
	@SuppressWarnings("unchecked")
	public <T extends AbstractGemFireAsLastResourceAspectSupport> T withThrowOnError(boolean throwOnError) {
		this.throwOnError = throwOnError;
		return (T) this;
	}

	protected static class GemFireConnectionHolder {

		private static final ThreadLocal<GFConnection> gemfireConnection = new ThreadLocal<>();

		public static GFConnection acquire(GFConnectionFactory connectionFactory) {
			return acquire(connectionFactory, DEFAULT_THROW_ON_ERROR, NO_OP_LOGGER);
		}

		public static GFConnection acquire(GFConnectionFactory connectionFactory, boolean throwOnError,
				Consumer<String> logger) {

			try {
				return of(connectionFactory.getConnection());
			}
			catch (ResourceException cause) {

				String message =
					String.format("Failed to acquire GemFire Connection from GemFire's JCA ResourceAdapter: %s",
						cause.getMessage());

				if (throwOnError) {
					throw newRuntimeException(cause, message);
				}
				else {
					logger.accept(message);
					return null;
				}
			}
		}

		/* (non-Javadoc) */
		public static GFConnection of(GFConnection connection) {
			gemfireConnection.set(connection);
			return connection;
		}

		/* (non-Javadoc) */
		public static Optional<GFConnection> get() {
			return Optional.ofNullable(gemfireConnection.get());
		}

		public static void close() {
			close(DEFAULT_THROW_ON_ERROR, NO_OP_LOGGER);
		}

		/* (non-Javadoc) */
		public static void close(boolean throwOnError, Consumer<String> logger) {

			get().ifPresent(connection -> {
				try {
					connection.close();
				}
				catch (ResourceException cause) {

					String message = String.format("Failed to close GemFire Connection: %s", cause.getMessage());

					if (throwOnError) {
						throw newRuntimeException(cause, message);
					}
					else {
						logger.accept(message);
					}
				}
			});
		}
	}
}
