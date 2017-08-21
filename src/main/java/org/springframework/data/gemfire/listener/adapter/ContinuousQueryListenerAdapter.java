/*
 * Copyright 2011-2012-2012 the original author or authors.
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

package org.springframework.data.gemfire.listener.adapter;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.geode.cache.Operation;
import org.apache.geode.cache.query.CqEvent;
import org.apache.geode.cache.query.CqQuery;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.data.gemfire.listener.ContinuousQueryListener;
import org.springframework.data.gemfire.listener.GemfireListenerExecutionFailedException;
import org.springframework.util.Assert;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;

/**
 * Event listener adapter that delegates the handling of messages to target listener methods via reflection,
 * with flexible event type conversion.
 *
 * Allows listener methods to operate on event content types, completely independent from the GemFire/Geode API.
 *
 * <p>Modeled as much as possible after the JMS MessageListenerAdapter in the core Spring Framework.
 *
 * <p>By default, the content of incoming GemFire/Geode CQ events gets extracted before being passed into
 * the target listener method, to let the target method operate on event content types such as Object or Operation
 * instead of the raw {@link CqEvent}.</p>
 *
 * <p>Find below some examples of method signatures compliant with this adapter class.
 *
 * This first example handles all <code>CqEvent</code> types and gets passed the contents of each
 * <code>event</code> type as an argument.</p>
 *
 * <pre class="code">public interface PojoListener {
 *    void handleEvent(CqEvent event);
 *    void handleEvent(Operation baseOp);
 *    void handleEvent(Object key);
 *    void handleEvent(Object key, Object newValue);
 *    void handleEvent(Throwable cause);
 *    void handleEvent(CqEvent event, Operation baseOp, byte[] deltaValue);
 *    void handleEvent(CqEvent event, Operation baseOp, Operation queryOp, Object key, Object newValue);
 * }</pre>
 *
 * @author Juergen Hoeller
 * @author Costin Leau
 * @author Oliver Gierke
 * @author John Blum
 * @see java.lang.reflect.Method
 * @see org.apache.geode.cache.Operation
 * @see org.apache.geode.cache.query.CqEvent
 * @see org.apache.geode.cache.query.CqQuery
 * @see org.springframework.data.gemfire.listener.ContinuousQueryListener
 * @since 1.1.0
 */
public class ContinuousQueryListenerAdapter implements ContinuousQueryListener {

	// Out-of-the-box value for the default listener handler method "handleEvent".
	public static final String DEFAULT_LISTENER_METHOD_NAME = "handleEvent";

	protected final Log logger = LogFactory.getLog(getClass());

	private MethodInvoker invoker;

	private Object delegate;

	private String defaultListenerMethod = DEFAULT_LISTENER_METHOD_NAME;

	/**
	 * Constructs a new instance of {@link ContinuousQueryListenerAdapter} with default settings.
	 */
	public ContinuousQueryListenerAdapter() {
		setDelegate(this);
	}

	/**
	 * Constructs a new instance of {@link ContinuousQueryListenerAdapter} initialized with
	 * the given {@link Object delegate}.
	 *
	 * @param delegate {@link Object delegate} of the listener method event callback.
	 * @see #setDelegate(Object)
	 */
	public ContinuousQueryListenerAdapter(Object delegate) {
		setDelegate(delegate);
	}

	/**
	 * Sets the target object to which CQ events are delegated.
	 *
	 * Specified listener methods have to be present on this target object.  If no explicit delegate object
	 * has been configured then listener methods are expected to present on this adapter instance.
	 * In other words, on a custom subclass of this adapter, defining listener methods.
	 *
	 * @param delegate {@link Object} to delegate listening for CQ events.
	 * @throws IllegalArgumentException if {@link Object delegate} is {@literal null}.
	 */
	public final void setDelegate(Object delegate) {

		Assert.notNull(delegate, "Delegate is required");

		this.delegate = delegate;
		this.invoker = null;
	}

	/**
	 * Returns a reference to the target object used to listen for and handle CQ events.
	 *
	 * @return a reference to the target object used to listen for and handle CQ events.
	 */
	public Object getDelegate() {
		return this.delegate;
	}

	/**
	 * Specify the name of the default listener method to delegate to in the case where no specific listener method
	 * has been determined.  Out-of-the-box value is {@link #DEFAULT_LISTENER_METHOD_NAME "handleEvent}.
	 *
	 * @param defaultListenerMethod the name of the default listener method to invoke.
	 * @see #getListenerMethodName
	 */
	public void setDefaultListenerMethod(String defaultListenerMethod) {
		this.defaultListenerMethod = defaultListenerMethod;
		this.invoker = null;
	}

	/**
	 * Return the name of the default listener method to delegate to.
	 *
	 * @return the name of the default listener method to invoke on CQ events.
	 */
	protected String getDefaultListenerMethod() {
		return this.defaultListenerMethod;
	}

	/**
	 * Determine the name of the listener method that is supposed to
	 * handle the given event.
	 * <p>The default implementation simply returns the configured
	 * default listener method, if any.
	 * @param event the GemFire event
	 * @return the name of the listener method (never <code>null</code>)
	 * @see #setDefaultListenerMethod
	 */
	@SuppressWarnings("unused")
	protected String getListenerMethodName(CqEvent event) {
		return getDefaultListenerMethod();
	}

	/**
	 * Standard {@link ContinuousQueryListener} callback method for handling CQ events.
	 *
	 * <p>Delegates the CQ event to the target listener method, with appropriate conversion of the event arguments.
	 * In case of an exception, the {@link #handleListenerException(Throwable)} method will be invoked.
	 *
	 * @param event incoming {@link CqEvent CQ event}.
	 * @see #handleListenerException
	 */
	@Override
	public void onEvent(CqEvent event) {

		try {
			// Determine whether the delegate is a ContinuousQueryListener implementation;
			// If so, this adapter will simply act as a pass-through
			if (this.delegate != this && this.delegate instanceof ContinuousQueryListener) {
				((ContinuousQueryListener) this.delegate).onEvent(event);
			}
			// Else, find the listener method handler reflectively
			else {

				String methodName = Optional.ofNullable(getListenerMethodName(event))
					.filter(StringUtils::hasText)
					.orElseThrow(() -> new InvalidDataAccessApiUsageException("No default listener method specified;"
						+ " Either specify a non-null value for the 'defaultListenerMethod' property"
						+ " or override the 'getListenerMethodName' method."));

				this.invoker = Optional.ofNullable(this.invoker)
					.orElseGet(() -> new MethodInvoker(this.delegate, methodName));

				invokeListenerMethod(event, methodName);
			}

		}
		catch (Throwable cause) {
			handleListenerException(cause);
		}
	}

	/**
	 * Handle the given exception that arose during listener execution.
	 * The default implementation logs the exception at error level.
	 * @param cause the exception to handle
	 */
	protected void handleListenerException(Throwable cause) {
		logger.error("Listener method execution failed", cause);
	}

	/**
	 * Invoke the specified listener method.
	 * @param event the event arguments to be passed in
	 * @param methodName the method to invoke
	 * @see #getListenerMethodName
	 */
	protected void invokeListenerMethod(CqEvent event, String methodName) {
		try {
			this.invoker.invoke(event);
		}
		catch (InvocationTargetException cause) {
			if (cause.getTargetException() instanceof DataAccessException) {
				throw (DataAccessException) cause.getTargetException();
			}
			else {
				throw new GemfireListenerExecutionFailedException(
					String.format("Listener method [%s] threw Exception...", methodName), cause.getTargetException());
			}
		}
		catch (Throwable cause) {
			throw new GemfireListenerExecutionFailedException(
				String.format("Failed to invoke the target listener method [%s]", methodName), cause);
		}
	}

	private class MethodInvoker {

		private final Object delegate;

		private final List<Method> methods;

		MethodInvoker(Object delegate, String methodName) {

			Class<?> delegateType = delegate.getClass();

			this.delegate = delegate;
			this.methods = new ArrayList<>();

			ReflectionUtils.doWithMethods(delegateType, method -> {
				ReflectionUtils.makeAccessible(method);
				this.methods.add(method);
			}, method -> isValidEventMethodSignature(method, methodName));

			Assert.isTrue(!this.methods.isEmpty(), String.format("Cannot find a suitable method named [%1$s#%2$s];"
				+ " Is the method public and does it have the proper arguments?",
					delegateType.getName(), methodName));
		}

		@SuppressWarnings("all")
		private boolean isValidEventMethodSignature(Method method, String methodName) {

			if (isEventHandlerMethod(method, methodName)) {

				Class<?>[] parameterTypes = method.getParameterTypes();

				int objects = 0;
				int operations = 0;

				if (parameterTypes.length > 0) {
					for (Class<?> parameterType : parameterTypes) {
						if (Object.class.equals(parameterType)) {
							if (++objects > 2) {
								return false;
							}
						}
						else if (Operation.class.equals(parameterType)) {
							if (++operations > 2) {
								return false;
							}
						}
						else if (byte[].class.equals(parameterType)) {
						}
						else if (CqEvent.class.equals(parameterType)) {
						}
						else if (CqQuery.class.equals(parameterType)) {
						}
						else if (Throwable.class.equals(parameterType)) {
						}
						else {
							return false;
						}
					}

					return true;
				}
			}

			return false;
		}

		private boolean isEventHandlerMethod(Method method, String methodName) {

			return Optional.ofNullable(method)
				.filter(it -> Modifier.isPublic(it.getModifiers()))
				.filter(it -> method.getName().equals(methodName))
				.isPresent();
		}

		void invoke(CqEvent event) throws IllegalAccessException, InvocationTargetException {

			for (Method method : this.methods) {
				method.invoke(this.delegate, getMethodArguments(method, event));
			}
		}

		private Object[] getMethodArguments(Method method, CqEvent event) {

			Class<?>[] parameterTypes = method.getParameterTypes();

			Object[] args = new Object[parameterTypes.length];

			boolean query = false;
			boolean value = false;

			for (int index = 0; index < parameterTypes.length; index++) {

				Class<?> parameterType = parameterTypes[index];

				if (Object.class.equals(parameterType)) {
					args[index] = (value ? event.getNewValue() : event.getKey());
					value = true;
				}
				else if (Operation.class.equals(parameterType)) {
					args[index] = (query ? event.getQueryOperation() : event.getBaseOperation());
					query = true;
				}
				else if (byte[].class.equals(parameterType)) {
					args[index] = event.getDeltaValue();
				}
				else if (CqEvent.class.equals(parameterType)) {
					args[index] = event;
				}
				else if (CqQuery.class.equals(parameterType)) {
					args[index] = event.getCq();
				}
				else if (Throwable.class.equals(parameterType)) {
					args[index] = event.getThrowable();
				}
			}

			return args;
		}
	}
}
