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

import java.lang.annotation.Annotation;
import java.util.concurrent.atomic.AtomicReference;

import org.apache.geode.cache.CustomExpiry;
import org.apache.geode.cache.ExpirationAction;
import org.apache.geode.cache.ExpirationAttributes;
import org.apache.geode.cache.Region;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.expression.BeanFactoryAccessor;
import org.springframework.context.expression.BeanFactoryResolver;
import org.springframework.context.expression.EnvironmentAccessor;
import org.springframework.context.expression.MapAccessor;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.convert.ConversionService;
import org.springframework.data.gemfire.ExpirationActionConverter;
import org.springframework.data.gemfire.ExpirationActionType;
import org.springframework.expression.EvaluationContext;
import org.springframework.expression.EvaluationException;
import org.springframework.expression.Expression;
import org.springframework.expression.ParseException;
import org.springframework.expression.spel.standard.SpelExpressionParser;
import org.springframework.expression.spel.support.StandardEvaluationContext;
import org.springframework.expression.spel.support.StandardTypeConverter;
import org.springframework.expression.spel.support.StandardTypeLocator;
import org.springframework.util.Assert;
import org.springframework.util.ObjectUtils;

/**
 * The AnnotationBasedExpiration class is an implementation of GemFire's CustomExpiry interface that determines
 * the Time-To-Live (TTL) or Idle-Timeout (TTI) expiration policy of a Region entry by introspecting the Region
 * entry's class type and reflecting on any Region entries annotated with Spring Data GemFire's Expiration-based
 * Annotations.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.BeanFactoryAware
 * @see org.springframework.data.gemfire.ExpirationActionType
 * @see org.springframework.data.gemfire.support.Expiration
 * @see org.springframework.data.gemfire.support.IdleTimeoutExpiration
 * @see org.springframework.data.gemfire.support.TimeToLiveExpiration
 * @see org.apache.geode.cache.CustomExpiry
 * @see org.apache.geode.cache.ExpirationAction
 * @see org.apache.geode.cache.ExpirationAttributes
 * @see org.apache.geode.cache.Region
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public class AnnotationBasedExpiration<K, V> implements BeanFactoryAware, CustomExpiry<K, V> {

	protected static final AtomicReference<BeanFactory> BEAN_FACTORY_REFERENCE = new AtomicReference<BeanFactory>(null);

	protected static final AtomicReference<StandardEvaluationContext> EVALUATION_CONTEXT_REFERENCE
		= new AtomicReference<StandardEvaluationContext>(null);

	//private ExpirationAttributes defaultExpirationAttributes = ExpirationAttributes.DEFAULT;
	private ExpirationAttributes defaultExpirationAttributes;

	/**
	 * Constructs a new instance of the AnnotationBasedExpiration class with no default expiration policy.
	 */
	public AnnotationBasedExpiration() {
		this(null);
	}

	/**
	 * Constructs a new instance of the AnnotationBasedExpiration class initialized with a specific, default
	 * expiration policy.
	 *
	 * @param defaultExpirationAttributes expiration settings used as the default expiration policy.
	 * @see org.apache.geode.cache.ExpirationAttributes
	 */
	public AnnotationBasedExpiration(ExpirationAttributes defaultExpirationAttributes) {
		this.defaultExpirationAttributes = defaultExpirationAttributes;
	}

	/**
	 * Constructs an AnnotationBasedExpiration instance with no default ExpirationAttributes to process
	 * Idle Timeout (TTI) Expiration annotated Region Entries.
	 *
	 * @param <K> the class type of the Region Entry Key.
	 * @param <V> the class type of the Region Entry Value.
	 * @return an AnnotationBasedExpiration instance to process Idle Timeout Expiration annotated Region Entries.
	 * @see org.springframework.data.gemfire.support.AnnotationBasedExpiration
	 * @see org.springframework.data.gemfire.support.IdleTimeoutExpiration
	 * @see #forIdleTimeout(org.apache.geode.cache.ExpirationAttributes)
	 */
	public static <K, V> AnnotationBasedExpiration<K, V> forIdleTimeout() {
		return forIdleTimeout(null);
	}

	/**
	 * Constructs an AnnotationBasedExpiration instance with default ExpirationAttributes to process
	 * Idle Timeout (TTI) Expiration annotated Region Entries.
	 *
	 * @param <K> the class type of the Region Entry Key.
	 * @param <V> the class type of the Region Entry Value.
	 * @param defaultExpirationAttributes ExpirationAttributes used by default if no Expiration policy was specified
	 * on the Region Entry.
	 * @return an AnnotationBasedExpiration instance to process Idle Timeout Expiration annotated Region Entries.
	 * @see org.springframework.data.gemfire.support.AnnotationBasedExpiration
	 * @see org.springframework.data.gemfire.support.IdleTimeoutExpiration
	 */
	public static <K, V> AnnotationBasedExpiration<K, V> forIdleTimeout(ExpirationAttributes defaultExpirationAttributes) {
		return new AnnotationBasedExpiration<K, V>(defaultExpirationAttributes) {
			@Override protected ExpirationMetaData getExpirationMetaData(final Region.Entry<K, V> entry) {
				return (isIdleTimeoutConfigured(entry) ? ExpirationMetaData.from(getIdleTimeout(entry))
					: super.getExpirationMetaData(entry));
			}
		};
	}

	/**
	 * Constructs an AnnotationBasedExpiration instance with no default ExpirationAttributes to process
	 * Time-To-Live (TTL) Expiration annotated Region Entries.
	 *
	 * @param <K> the class type of the Region Entry Key.
	 * @param <V> the class type of the Region Entry Value.
	 * @return an AnnotationBasedExpiration instance to process Time-To-Live Expiration annotated Region Entries.
	 * @see org.springframework.data.gemfire.support.AnnotationBasedExpiration
	 * @see org.springframework.data.gemfire.support.TimeToLiveExpiration
	 * @see #forTimeToLive(org.apache.geode.cache.ExpirationAttributes)
	 */
	public static <K, V> AnnotationBasedExpiration<K, V> forTimeToLive() {
		return forTimeToLive(null);
	}

	/**
	 * Constructs an AnnotationBasedExpiration instance with default ExpirationAttributes to process
	 * Time-To-Live (TTL) Expiration annotated Region Entries.
	 *
	 * @param <K> the class type of the Region Entry Key.
	 * @param <V> the class type of the Region Entry Value.
	 * @param defaultExpirationAttributes ExpirationAttributes used by default if no Expiration policy was specified
	 * on the Region Entry.
	 * @return an AnnotationBasedExpiration instance to process Time-To-Live Expiration annotated Region Entries.
	 * @see org.springframework.data.gemfire.support.AnnotationBasedExpiration
	 * @see org.springframework.data.gemfire.support.TimeToLiveExpiration
	 */
	public static <K, V> AnnotationBasedExpiration<K, V> forTimeToLive(ExpirationAttributes defaultExpirationAttributes) {
		return new AnnotationBasedExpiration<K, V>(defaultExpirationAttributes) {
			@Override protected ExpirationMetaData getExpirationMetaData(final Region.Entry<K, V> entry) {
				return (isTimeToLiveConfigured(entry) ? ExpirationMetaData.from(getTimeToLive(entry))
					: super.getExpirationMetaData(entry));
			}
		};
	}

	/* (non-Javadoc) */
	protected void initEvaluationContext() {
		BeanFactory beanFactory = getBeanFactory();

		if (EVALUATION_CONTEXT_REFERENCE.compareAndSet(null, newEvaluationContext())) {
			StandardEvaluationContext evaluationContext = EVALUATION_CONTEXT_REFERENCE.get();

			evaluationContext.addPropertyAccessor(new BeanFactoryAccessor());
			evaluationContext.addPropertyAccessor(new EnvironmentAccessor());
			evaluationContext.addPropertyAccessor(new MapAccessor());

			if (beanFactory instanceof ConfigurableBeanFactory) {
				ConfigurableBeanFactory configurableBeanFactory = (ConfigurableBeanFactory) beanFactory;
				ConversionService conversionService = configurableBeanFactory.getConversionService();
				if (conversionService != null) {
					evaluationContext.setTypeConverter(new StandardTypeConverter(conversionService));
				}
				evaluationContext.setTypeLocator(new StandardTypeLocator(configurableBeanFactory.getBeanClassLoader()));
			}
		}

		EVALUATION_CONTEXT_REFERENCE.get().setBeanResolver(new BeanFactoryResolver(beanFactory));
	}

	/* (non-Javadoc) */
	StandardEvaluationContext newEvaluationContext() {
		return new StandardEvaluationContext();
	}

	/**
	 * Sets the BeanFactory managing this AnnotationBasedExpiration bean in the Spring context.
	 *
	 * @param beanFactory the Spring BeanFactory to which this bean belongs.
	 * @throws BeansException if the BeanFactory reference cannot be initialized.
	 * @see org.springframework.beans.factory.BeanFactory
	 * @see #initEvaluationContext()
	 */
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		BEAN_FACTORY_REFERENCE.set(beanFactory);
		initEvaluationContext();
	}

	/**
	 * Gets a reference to the BeanFactory in which this AnnotationBasedExpiration Bean belongs.
	 *
	 * @return a reference to the Spring BeanFactory.
	 * @throws java.lang.IllegalStateException if the BeanFactory reference was not properly initialized.
	 * @see org.springframework.beans.factory.BeanFactory
	 */
	protected BeanFactory getBeanFactory() {
		BeanFactory localBeanFactory = BEAN_FACTORY_REFERENCE.get();
		Assert.state(localBeanFactory != null, "beanFactory was not properly initialized");
		return localBeanFactory;
	}

	/**
	 * Sets the expiration policy to use by default when no application domain object specific expiration meta-data
	 * has been specified.
	 *
	 * @param defaultExpirationAttributes expiration settings used as the default expiration policy.
	 * @see #getDefaultExpirationAttributes()
	 * @see org.apache.geode.cache.ExpirationAttributes
	 */
	public void setDefaultExpirationAttributes(ExpirationAttributes defaultExpirationAttributes) {
		this.defaultExpirationAttributes = defaultExpirationAttributes;
	}

	/**
	 * Gets the expiration policy used by default when no application domain object specific expiration meta-data
	 * has been specified.
	 *
	 * @return an instance of ExpirationAttributes with expiration settings defining the default expiration policy.
	 * @see #setDefaultExpirationAttributes(org.apache.geode.cache.ExpirationAttributes)
	 * @see org.apache.geode.cache.ExpirationAttributes
	 */
	protected ExpirationAttributes getDefaultExpirationAttributes() {
		//return (defaultExpirationAttributes != null ? defaultExpirationAttributes : ExpirationAttributes.DEFAULT);
		return defaultExpirationAttributes;
	}

	/**
	 * Calculate the expiration for a given entry. Returning null indicates that the default for the Region
	 * should be used. The entry parameter should not be used after this method invocation completes.
	 *
	 * @param entry the entry to calculate the expiration for.
	 * @return the expiration to be used, null if the Region's defaults should be used.
	 * @see org.apache.geode.cache.ExpirationAttributes
	 */
	@Override
	public ExpirationAttributes getExpiry(Region.Entry<K, V> entry) {
		return newExpirationAttributes(getExpirationMetaData(entry));
	}

	/**
	 * Gets custom expiration (Annotation-based) meta-data for the given Region entry.
	 *
	 * @param entry the Region entry used as the source of the expiration meta-data.
	 * @return ExpirationMetaData extracted from the Region entry.
	 * @throws NullPointerException if the Region.Entry, Region or the Region's attributes are null.
	 * @see org.springframework.data.gemfire.support.AnnotationBasedExpiration.ExpirationMetaData
	 */
	protected ExpirationMetaData getExpirationMetaData(Region.Entry<K, V> entry) {
		return (isExpirationConfigured(entry) ? ExpirationMetaData.from(getExpiration(entry)) : null);
	}

	/**
	 * Constructs a new instance of ExpirationAttributes configured with the application domain object specific
	 * expiration policy.  If the application domain object type has not been annotated with custom expiration
	 * meta-data, then the default expiration settings are used.
	 *
	 * @param expirationMetaData application domain object specific expiration policy meta-data used to construct
	 * the ExpirationAttributes.
	 * @return a custom ExpirationAttributes configured with the application domain object specific expiration policy
	 * or the default expiration settings if the application domain object has not been annotated with custom
	 * expiration meta-data.
	 * @see org.springframework.data.gemfire.support.AnnotationBasedExpiration.ExpirationMetaData
	 * @see org.apache.geode.cache.ExpirationAttributes
	 * @see #getDefaultExpirationAttributes()
	 */
	protected ExpirationAttributes newExpirationAttributes(ExpirationMetaData expirationMetaData) {
		return (expirationMetaData != null ? expirationMetaData.toExpirationAttributes()
			: getDefaultExpirationAttributes());
	}

	/**
	 * Determines whether the Region Entry has been annotated with the Expiration Annotation.
	 *
	 * @param entry the Region.Entry to evaluate for the presence of the Expiration Annotation.
	 * @return a boolean value indicating whether the Region Entry has been annotated with @Expiration.
	 * @see org.springframework.data.gemfire.support.Expiration
	 * @see #isAnnotationPresent(Object, Class)
	 */
	protected boolean isExpirationConfigured(Region.Entry<K, V> entry) {
		return isAnnotationPresent(entry.getValue(), Expiration.class);
	}

	/**
	 * Gets the Expiration Annotation meta-data from the Region Entry.
	 *
	 * @param entry the Region.Entry from which to extract the Expiration Annotation meta-data.
	 * @return the Expiration Annotation meta-data for the given Region Entry or {@code null}
	 * if the Region Entry has not been annotated with @Expiration.
	 * @see org.springframework.data.gemfire.support.Expiration
	 * @see #getAnnotation(Object, Class)
	 */
	protected Expiration getExpiration(Region.Entry<K, V> entry) {
		return getAnnotation(entry.getValue(), Expiration.class);
	}

	/**
	 * Determines whether the Region Entry has been annotated with the IdleTimeoutExpiration Annotation.
	 *
	 * @param entry the Region.Entry to evaluate for the presence of the IdleTimeoutExpiration Annotation.
	 * @return a boolean value indicating whether the Region Entry has been annotated with @IdleTimeoutExpiration.
	 * @see org.springframework.data.gemfire.support.IdleTimeoutExpiration
	 * @see #isAnnotationPresent(Object, Class)
	 */
	protected boolean isIdleTimeoutConfigured(Region.Entry<K, V> entry) {
		return isAnnotationPresent(entry.getValue(), IdleTimeoutExpiration.class);
	}

	/**
	 * Gets the IdleTimeoutExpiration Annotation meta-data from the Region Entry.
	 *
	 * @param entry the Region.Entry from which to extract the IdleTimeoutExpiration Annotation meta-data.
	 * @return the IdleTimeoutExpiration Annotation meta-data for the given Region Entry or {@code null}
	 * if the Region Entry has not been annotated with @IdleTimeoutExpiration.
	 * @see org.springframework.data.gemfire.support.IdleTimeoutExpiration
	 * @see #getAnnotation(Object, Class)
	 */
	protected IdleTimeoutExpiration getIdleTimeout(Region.Entry<K, V> entry) {
		return getAnnotation(entry.getValue(), IdleTimeoutExpiration.class);
	}

	/**
	 * Determines whether the Region Entry has been annotated with the TimeToLiveExpiration Annotation.
	 *
	 * @param entry the Region.Entry to evaluate for the presence of the TimeToLiveExpiration Annotation.
	 * @return a boolean value indicating whether the Region Entry has been annotated with @TimeToLiveExpiration.
	 * @see org.springframework.data.gemfire.support.TimeToLiveExpiration
	 * @see #isAnnotationPresent(Object, Class)
	 */
	protected boolean isTimeToLiveConfigured(Region.Entry<K, V> entry) {
		return isAnnotationPresent(entry.getValue(), TimeToLiveExpiration.class);
	}

	/**
	 * Gets the TimeToLiveExpiration Annotation meta-data from the Region Entry.
	 *
	 * @param entry the Region.Entry from which to extract the TimeToLiveExpiration Annotation meta-data.
	 * @return the TimeToLiveExpiration Annotation meta-data for the given Region Entry or {@code null}
	 * if the Region Entry has not been annotated with @TimeToLiveExpiration.
	 * @see org.springframework.data.gemfire.support.TimeToLiveExpiration
	 * @see #getAnnotation(Object, Class)
	 */
	protected TimeToLiveExpiration getTimeToLive(Region.Entry<K, V> entry) {
		return getAnnotation(entry.getValue(), TimeToLiveExpiration.class);
	}

	/* (non-Javadoc) */
	private <T extends Annotation> boolean isAnnotationPresent(Object obj, Class<T> annotationType) {
		return (obj != null && obj.getClass().isAnnotationPresent(annotationType));
	}

	/* (non-Javadoc) */
	private <T extends Annotation> T getAnnotation(Object obj, Class<T> annotationType) {
		return AnnotationUtils.getAnnotation(obj.getClass(), annotationType);
	}

	/**
	 * Called when the Region containing this callback is closed or destroyed, when the Cache is closed,
	 * or when a callback is removed from a Region using an AttributesMutator.
	 */
	@Override
	public void close() {
	}

	/**
	 * The ExpirationMetaData class encapsulates the settings constituting the expiration policy including
	 * the expiration timeout and the action performed when expiration occurs.
	 *
	 * @see org.apache.geode.cache.ExpirationAttributes
	 */
	protected static class ExpirationMetaData {

		private static final ExpirationActionConverter EXPIRATION_ACTION_CONVERTER = new ExpirationActionConverter();

		private final int timeout;

		private final ExpirationActionType action;

		protected ExpirationMetaData(int timeout, ExpirationActionType action) {
			this.timeout = timeout;
			this.action = action;
		}

		protected static ExpirationMetaData from(ExpirationAttributes expirationAttributes) {
			return new ExpirationMetaData(expirationAttributes.getTimeout(), ExpirationActionType.valueOf(
				expirationAttributes.getAction()));
		}

		protected static ExpirationMetaData from(Expiration expiration) {
			return new ExpirationMetaData(parseTimeout(expiration.timeout()), parseAction(expiration.action()));
		}

		protected static ExpirationMetaData from(IdleTimeoutExpiration expiration) {
			return new ExpirationMetaData(parseTimeout(expiration.timeout()), parseAction(expiration.action()));
		}

		protected static ExpirationMetaData from(TimeToLiveExpiration expiration) {
			return new ExpirationMetaData(parseTimeout(expiration.timeout()), parseAction(expiration.action()));
		}

		public ExpirationAttributes toExpirationAttributes() {
			return new ExpirationAttributes(timeout(), expirationAction());
		}

		protected static int parseTimeout(String timeout) {
			try {
				return Integer.parseInt(timeout);
			}
			catch (NumberFormatException cause) {
				try {
					// Next, try to parse the 'timeout' as a Spring Expression using SpEL.
					return new SpelExpressionParser().parseExpression(timeout).getValue(
						EVALUATION_CONTEXT_REFERENCE.get(), Integer.TYPE);
				}
				catch (ParseException e) {
					// Finally, try to process the 'timeout' as a Spring Property Placeholder.
					if (BEAN_FACTORY_REFERENCE.get() instanceof ConfigurableBeanFactory) {
						return Integer.parseInt(((ConfigurableBeanFactory) BEAN_FACTORY_REFERENCE.get())
							.resolveEmbeddedValue(timeout));
					}

					throw cause;
				}
			}
		}

		protected static ExpirationActionType parseAction(String action) {
			try {
				return ExpirationActionType.valueOf(EXPIRATION_ACTION_CONVERTER.convert(action));
			}
			catch (IllegalArgumentException cause) {
				// Next, try to parse the 'action' as a Spring Expression using SpEL.
				EvaluationException evaluationException = new EvaluationException(String.format(
					"'%1$s' is not resolvable as a valid ExpirationAction(Type)", action), cause);

				EvaluationContext evaluationContext = EVALUATION_CONTEXT_REFERENCE.get();

				try {
					Expression expression = new SpelExpressionParser().parseExpression(action);
					Class<?> valueType = expression.getValueType(evaluationContext);

					if (String.class.equals(valueType)) {
						return ExpirationActionType.valueOf(EXPIRATION_ACTION_CONVERTER.convert(expression.getValue(
							evaluationContext, String.class)));
					}
					else if (ExpirationAction.class.equals(valueType)) {
						return ExpirationActionType.valueOf(expression.getValue(evaluationContext, ExpirationAction.class));
					}
					else if (ExpirationActionType.class.equals(valueType)) {
						return expression.getValue(evaluationContext, ExpirationActionType.class);
					}

					throw evaluationException;
				}
				catch (ParseException e) {
					// Finally, try to process the 'action' as a Spring Property Placeholder.
					if (BEAN_FACTORY_REFERENCE.get() instanceof ConfigurableBeanFactory) {
						try {
							String resolvedValue = ((ConfigurableBeanFactory) BEAN_FACTORY_REFERENCE.get())
								.resolveEmbeddedValue(action);

							return ExpirationActionType.valueOf(EXPIRATION_ACTION_CONVERTER.convert(resolvedValue));
						}
						catch (IllegalArgumentException ignore) {
						}
					}

					throw evaluationException;
				}
			}
		}

		public ExpirationActionType action() {
			return action;
		}

		public ExpirationAction expirationAction() {
			return action().getExpirationAction();
		}

		public int timeout() {
			return timeout;
		}

		@Override
		public boolean equals(final Object obj) {
			if (obj == this) {
				return true;
			}

			if (!(obj instanceof ExpirationMetaData)) {
				return false;
			}

			ExpirationMetaData that = (ExpirationMetaData) obj;

			return (this.timeout() == that.timeout()
				&& ObjectUtils.nullSafeEquals(this.action(), that.action()));
		}

		@Override
		public int hashCode() {
			int hashValue = 17;
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(timeout());
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(action());
			return hashValue;
		}

		@Override
		public String toString() {
			return String.format("{ @type = %1$s, timeout = %2$d, action = %3$s }", getClass().getName(),
				timeout(), action());
		}
	}

}
