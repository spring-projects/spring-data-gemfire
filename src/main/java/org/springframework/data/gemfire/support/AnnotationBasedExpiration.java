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

import com.gemstone.gemfire.cache.CustomExpiry;
import com.gemstone.gemfire.cache.ExpirationAction;
import com.gemstone.gemfire.cache.ExpirationAttributes;
import com.gemstone.gemfire.cache.Region;

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
 * The {@link AnnotationBasedExpiration} class is an implementation of the {@link CustomExpiry} interface
 * that determines the Time-To-Live (TTL) or Idle-Timeout (TTI) expiration policy of a {@link Region} entry
 * by introspecting the {@link Region} entry's class type and reflecting on any {@link Region} entries annotated
 * with SDG's Expiration-based Annotations.
 *
 * @author John Blum
 * @see java.lang.annotation.Annotation
 * @see org.springframework.beans.factory.BeanFactory
 * @see org.springframework.beans.factory.BeanFactoryAware
 * @see org.springframework.data.gemfire.ExpirationActionType
 * @see org.springframework.data.gemfire.support.Expiration
 * @see org.springframework.data.gemfire.support.IdleTimeoutExpiration
 * @see org.springframework.data.gemfire.support.TimeToLiveExpiration
 * @see com.gemstone.gemfire.cache.CustomExpiry
 * @see com.gemstone.gemfire.cache.ExpirationAction
 * @see com.gemstone.gemfire.cache.ExpirationAttributes
 * @see com.gemstone.gemfire.cache.Region
 * @see <a href="http://docs.spring.io/spring-data-gemfire/docs/current/reference/html/#bootstrap:region:expiration:annotation">Annotation-based Data Expiration</a>
 * @since 1.7.0
 */
@SuppressWarnings("unused")
public class AnnotationBasedExpiration<K, V> implements BeanFactoryAware, CustomExpiry<K, V> {

	protected static final AtomicReference<BeanFactory> BEAN_FACTORY_REFERENCE =
		new AtomicReference<BeanFactory>(null);

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
	 * Constructs a new instance of {@link AnnotationBasedExpiration} initialized with a specific, default
	 * expiration policy.
	 *
	 * @param defaultExpirationAttributes expiration settings used as the default expiration policy.
	 * @see com.gemstone.gemfire.cache.ExpirationAttributes
	 */
	public AnnotationBasedExpiration(ExpirationAttributes defaultExpirationAttributes) {
		this.defaultExpirationAttributes = defaultExpirationAttributes;
	}

	/**
	 * Factory method used to construct an instance of {@link AnnotationBasedExpiration} having no default
	 * {@link ExpirationAttributes} to process expired annotated {@link Region} entries
	 * using Idle Timeout (TTI) Expiration.
	 *
	 * @param <K> {@link Class} type of the {@link Region} entry key.
	 * @param <V> {@link Class} type of the {@link Region} entry value.
	 * @return an {@link AnnotationBasedExpiration} instance to process expired annotated {@link Region} entries
	 * using Idle Timeout expiration.
	 * @see org.springframework.data.gemfire.support.AnnotationBasedExpiration
	 * @see org.springframework.data.gemfire.support.IdleTimeoutExpiration
	 * @see #forIdleTimeout(com.gemstone.gemfire.cache.ExpirationAttributes)
	 */
	public static <K, V> AnnotationBasedExpiration<K, V> forIdleTimeout() {
		return forIdleTimeout(null);
	}

	/**
	 * Factory method used to construct an instance of {@link AnnotationBasedExpiration} initialized with
	 * default {@link ExpirationAttributes} to process expired annotated {@link Region} entries
	 * using Idle Timeout (TTI) expiration.
	 *
	 * @param <K> {@link Class} type of the {@link Region} entry key.
	 * @param <V> {@link Class} type of the {@link Region} entry value.
	 * @param defaultExpirationAttributes {@link ExpirationAttributes} used by default if no expiration policy
	 * was specified on the {@link Region}.
	 * @return an {@link AnnotationBasedExpiration} instance to process expired annotated {@link Region} entries
	 * using Idle Timeout expiration.
	 * @see org.springframework.data.gemfire.support.AnnotationBasedExpiration
	 * @see org.springframework.data.gemfire.support.IdleTimeoutExpiration
	 * @see #AnnotationBasedExpiration(ExpirationAttributes)
	 */
	public static <K, V> AnnotationBasedExpiration<K, V> forIdleTimeout(ExpirationAttributes defaultExpirationAttributes) {
		return new AnnotationBasedExpiration<K, V>(defaultExpirationAttributes) {
			@Override protected ExpirationMetaData getExpirationMetaData(Region.Entry<K, V> entry) {
				return (isIdleTimeoutConfigured(entry) ? ExpirationMetaData.from(getIdleTimeout(entry))
					: super.getExpirationMetaData(entry));
			}
		};
	}

	/**
	 * Factory method used to construct an instance of {@link AnnotationBasedExpiration} having no default
	 * {@link ExpirationAttributes} to process expired annotated {@link Region} entries
	 * using Time-To-Live (TTL) Expiration.
	 *
	 * @param <K> {@link Class} type of the {@link Region} entry key.
	 * @param <V> {@link Class} type of the {@link Region} entry value.
	 * @return an {@link AnnotationBasedExpiration} instance to process expired annotated {@link Region} entries
	 * using Time-To-Live expiration.
	 * @see org.springframework.data.gemfire.support.AnnotationBasedExpiration
	 * @see org.springframework.data.gemfire.support.TimeToLiveExpiration
	 * @see #forTimeToLive(ExpirationAttributes)
	 */
	public static <K, V> AnnotationBasedExpiration<K, V> forTimeToLive() {
		return forTimeToLive(null);
	}

	/**
	 * Factory method used to construct an instance of {@link AnnotationBasedExpiration} initialized with
	 * default {@link ExpirationAttributes} to process expired annotated {@link Region} entries
	 * using Time-To-Live (TTL) expiration.
	 *
	 * @param <K> {@link Class} type of the {@link Region} entry key.
	 * @param <V> {@link Class} type of the {@link Region} entry value.
	 * @param defaultExpirationAttributes {@link ExpirationAttributes} used by default if no expiration policy
	 * was specified on the {@link Region}.
	 * @return an {@link AnnotationBasedExpiration} instance to process expired annotated {@link Region} entries
	 * using Time-To-Live expiration.
	 * @see org.springframework.data.gemfire.support.AnnotationBasedExpiration
	 * @see org.springframework.data.gemfire.support.TimeToLiveExpiration
	 * @see #AnnotationBasedExpiration(ExpirationAttributes)
	 */
	public static <K, V> AnnotationBasedExpiration<K, V> forTimeToLive(ExpirationAttributes defaultExpirationAttributes) {
		return new AnnotationBasedExpiration<K, V>(defaultExpirationAttributes) {
			@Override protected ExpirationMetaData getExpirationMetaData(Region.Entry<K, V> entry) {
				return (isTimeToLiveConfigured(entry) ? ExpirationMetaData.from(getTimeToLive(entry))
					: super.getExpirationMetaData(entry));
			}
		};
	}

	/**
	 * Initializes the Spring Expression Language (SpEL) {@link EvaluationContext} used to parse property placeholder
	 * and SpEL expressions in the Expiration annotation attribute values.
	 */
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
	 * Sets the {@link BeanFactory} managing this {@link AnnotationBasedExpiration} bean in the Spring context.
	 *
	 * @param beanFactory the Spring {@link BeanFactory} to which this bean belongs.
	 * @throws BeansException if the {@link BeanFactory} reference cannot be initialized.
	 * @see org.springframework.beans.factory.BeanFactory
	 * @see #initEvaluationContext()
	 */
	@Override
	public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
		BEAN_FACTORY_REFERENCE.set(beanFactory);
		initEvaluationContext();
	}

	/**
	 * Gets a reference to the Spring {@link BeanFactory} in which this {@link AnnotationBasedExpiration} bean
	 * is managed.
	 *
	 * @return a reference to the Spring {@link BeanFactory}.
	 * @throws java.lang.IllegalStateException if the {@link BeanFactory} reference was not properly initialized.
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
	 * @see com.gemstone.gemfire.cache.ExpirationAttributes
	 */
	public void setDefaultExpirationAttributes(ExpirationAttributes defaultExpirationAttributes) {
		this.defaultExpirationAttributes = defaultExpirationAttributes;
	}

	/**
	 * Gets the expiration policy used by default when no application domain object specific expiration meta-data
	 * has been specified.
	 *
	 * @return an instance of ExpirationAttributes with expiration settings defining the default expiration policy.
	 * @see #setDefaultExpirationAttributes(com.gemstone.gemfire.cache.ExpirationAttributes)
	 * @see com.gemstone.gemfire.cache.ExpirationAttributes
	 */
	protected ExpirationAttributes getDefaultExpirationAttributes() {
		//return (defaultExpirationAttributes != null ? defaultExpirationAttributes : ExpirationAttributes.DEFAULT);
		return defaultExpirationAttributes;
	}

	/**
	 * Calculate the expiration for a given entry. Returning {@literal null} indicates that the default
	 * for the {@link Region} should be used. The entry parameter should not be used after this method
	 * invocation completes.
	 *
	 * @param entry the entry used to determine the appropriate expiration policy.
	 * @return the expiration configuration to be used or {@literal null} if the Region's defaults should be used.
	 * @see com.gemstone.gemfire.cache.ExpirationAttributes
	 * @see com.gemstone.gemfire.cache.Region
	 * @see #getExpirationMetaData(Region.Entry)
	 * @see #newExpirationAttributes(ExpirationMetaData)
	 */
	@Override
	public ExpirationAttributes getExpiry(Region.Entry<K, V> entry) {
		return newExpirationAttributes(getExpirationMetaData(entry));
	}

	/**
	 * Gets custom expiration (Annotation-based) policy meta-data for the given {@link Region} entry.
	 *
	 * @param entry {@link Region} entry used as the source of the expiration policy meta-data.
	 * @return {@link ExpirationMetaData} extracted from the {@link Region} entry or {@literal null}
	 * if the expiration policy meta-data could not be determined from the {@link Region} entry.
	 * @see org.springframework.data.gemfire.support.AnnotationBasedExpiration.ExpirationMetaData
	 */
	protected ExpirationMetaData getExpirationMetaData(Region.Entry<K, V> entry) {
		return (isExpirationConfigured(entry) ? ExpirationMetaData.from(getExpiration(entry)) : null);
	}

	/**
	 * Constructs a new instance of {@link ExpirationAttributes} configured with the application domain object
	 * specific expiration policy.  If the application domain object type has not been annotated with
	 * custom expiration meta-data, then the default expiration settings are used.
	 *
	 * @param expirationMetaData application domain object specific expiration policy meta-data used to construct
	 * the {@link ExpirationAttributes}.
	 * @return custom {@link ExpirationAttributes} configured from the application domain object specific
	 * expiration policy or the default expiration settings if the application domain object has not been
	 * annotated with custom expiration meta-data.
	 * @see org.springframework.data.gemfire.support.AnnotationBasedExpiration.ExpirationMetaData
	 * @see com.gemstone.gemfire.cache.ExpirationAttributes
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
		return (entry != null && isExpirationConfigured(entry.getValue()));
	}

	/* (non-Javadoc) */
	private boolean isExpirationConfigured(Object obj) {
		return isAnnotationPresent(obj, Expiration.class);
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
		return getExpiration(entry.getValue());
	}

	/* (non-Javadoc) */
	private Expiration getExpiration(Object obj) {
		return getAnnotation(obj, Expiration.class);
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
		return (entry != null && isIdleTimeoutConfigured(entry.getValue()));
	}

	/* (non-Javadoc) */
	private boolean isIdleTimeoutConfigured(Object obj) {
		return isAnnotationPresent(obj, IdleTimeoutExpiration.class);
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
		return getIdleTimeout(entry.getValue());
	}

	/* (non-Javadoc) */
	private IdleTimeoutExpiration getIdleTimeout(Object obj) {
		return getAnnotation(obj, IdleTimeoutExpiration.class);
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
		return (entry != null && isTimeToLiveConfigured(entry.getValue()));
	}

	/* (non-Javadoc) */
	private boolean isTimeToLiveConfigured(Object value) {
		return isAnnotationPresent(value, TimeToLiveExpiration.class);
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
		return getTimeToLive(entry.getValue());
	}

	/* (non-Javadoc) */
	private TimeToLiveExpiration getTimeToLive(Object obj) {
		return getAnnotation(obj, TimeToLiveExpiration.class);
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
	 * @see com.gemstone.gemfire.cache.ExpirationAttributes
	 */
	protected static class ExpirationMetaData {

		private static final ExpirationActionConverter EXPIRATION_ACTION_CONVERTER = new ExpirationActionConverter();

		private final int timeout;

		private final ExpirationActionType action;

		/* (non-Javadoc) */
		protected ExpirationMetaData(int timeout, ExpirationActionType action) {
			this.timeout = timeout;
			this.action = action;
		}

		/* (non-Javadoc) */
		protected static ExpirationMetaData from(ExpirationAttributes expirationAttributes) {
			return new ExpirationMetaData(expirationAttributes.getTimeout(), ExpirationActionType.valueOf(
				expirationAttributes.getAction()));
		}

		/* (non-Javadoc) */
		protected static ExpirationMetaData from(Expiration expiration) {
			return new ExpirationMetaData(parseTimeout(expiration.timeout()), parseAction(expiration.action()));
		}

		/* (non-Javadoc) */
		protected static ExpirationMetaData from(IdleTimeoutExpiration expiration) {
			return new ExpirationMetaData(parseTimeout(expiration.timeout()), parseAction(expiration.action()));
		}

		/* (non-Javadoc) */
		protected static ExpirationMetaData from(TimeToLiveExpiration expiration) {
			return new ExpirationMetaData(parseTimeout(expiration.timeout()), parseAction(expiration.action()));
		}

		/* (non-Javadoc) */
		public ExpirationAttributes toExpirationAttributes() {
			return new ExpirationAttributes(timeout(), expirationAction());
		}

		/* (non-Javadoc) */
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

		/* (non-Javadoc) */
		protected static ExpirationActionType parseAction(String action) {
			try {
				return ExpirationActionType.valueOf(EXPIRATION_ACTION_CONVERTER.convert(action));
			}
			catch (IllegalArgumentException cause) {
				// Next, try to parse the 'action' as a Spring Expression using SpEL.
				EvaluationException evaluationException = new EvaluationException(String.format(
					"[%s] is not resolvable as an ExpirationAction(Type)", action), cause);

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

		/* (non-Javadoc) */
		public ExpirationActionType action() {
			return action;
		}

		/* (non-Javadoc) */
		public ExpirationAction expirationAction() {
			return action().getExpirationAction();
		}

		/* (non-Javadoc) */
		public int timeout() {
			return timeout;
		}

		/**
		 * @inheritDoc
		 */
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

		/**
		 * @inheritDoc
		 */
		@Override
		public int hashCode() {
			int hashValue = 17;
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(timeout());
			hashValue = 37 * hashValue + ObjectUtils.nullSafeHashCode(action());
			return hashValue;
		}

		/**
		 * @inheritDoc
		 */
		@Override
		public String toString() {
			return String.format("{ @type = %1$s, timeout = %2$d, action = %3$s }", getClass().getName(),
				timeout(), action());
		}
	}
}
