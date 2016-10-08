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
import org.springframework.util.ReflectionUtils.MethodCallback;
import org.springframework.util.ReflectionUtils.MethodFilter;

/**
 * Event listener adapter that delegates the handling of messages to target
 * listener methods via reflection, with flexible event type conversion.
 * Allows listener methods to operate on event content types, completely
 * independent from the GemFire API.
 *
 * <p>Modeled as much as possible after the JMS MessageListenerAdapter in
 * Spring Framework.
 *
 * <p>By default, the content of incoming GemFire events gets extracted before
 * being passed into the target listener method, to let the target method
 * operate on event content types such as Object or Operation instead of
 * the raw {@link CqEvent}.</p>
 *
 * <p>Find below some examples of method signatures compliant with this
 * adapter class. This first example handles all <code>CqEvent</code> types
 * and gets passed the contents of each <code>event</code> type as an
 * argument.</p>
 *
 * <pre class="code">public interface PojoListener {
 *    void handleEvent(CqEvent event);
 *    void handleEvent(Operation baseOp);
 *    void handleEvent(Object key);
 *    void handleEvent(Object key, Object newValue);
 *    void handleEvent(Throwable th);
 *    void handleEvent(CqEvent event, Operation baseOp, byte[] deltaValue);
 *    void handleEvent(CqEvent event, Operation baseOp, Operation queryOp, Object key, Object newValue);
 * }</pre>
 *
 * @author Juergen Hoeller
 * @author Costin Leau
 * @author Oliver Gierke
 * @author John Blum
 */
public class ContinuousQueryListenerAdapter implements ContinuousQueryListener {

	// Out-of-the-box value for the default listener handler method "handleEvent".
	public static final String DEFAULT_LISTENER_METHOD_NAME = "handleEvent";

	protected final Log logger = LogFactory.getLog(getClass());

	private MethodInvoker invoker;

	private Object delegate;

	private String defaultListenerMethod = DEFAULT_LISTENER_METHOD_NAME;

	/**
	 * Create a new {@link ContinuousQueryListenerAdapter} with default settings.
	 */
	public ContinuousQueryListenerAdapter() {
		setDelegate(this);
	}

	/**
	 * Create a new {@link ContinuousQueryListenerAdapter} for the given delegate.
	 *
	 * @param delegate the delegate object
	 */
	public ContinuousQueryListenerAdapter(Object delegate) {
		setDelegate(delegate);
	}

	/**
	 * Set a target object to delegate events listening to.
	 * Specified listener methods have to be present on this target object.
	 * <p>If no explicit delegate object has been specified, listener
	 * methods are expected to present on this adapter instance, that is,
	 * on a custom subclass of this adapter, defining listener methods.
	 *
	 * @param delegate delegate object
	 */
	public void setDelegate(Object delegate) {
		Assert.notNull(delegate, "'delegate' must not be null");
		this.delegate = delegate;
		this.invoker = null;
	}

	/**
	 * Returns the target object to delegate event listening to.
	 *
	 * @return event listening delegation
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
	 * Standard {@link ContinuousQueryListener} entry point.
	 * <p>Delegates the event to the target listener method, with appropriate
	 * conversion of the event argument. In case of an exception, the
	 * {@link #handleListenerException(Throwable)} method will be invoked.
	 *
	 * @param event the incoming GemFire event
	 * @see #handleListenerException
	 */
	public void onEvent(CqEvent event) {
		try {
			// Check whether the delegate is a ContinuousQueryListener implementation itself.
			// If so, this adapter will simply act as a pass-through.
			if (delegate != this && delegate instanceof ContinuousQueryListener) {
				((ContinuousQueryListener) delegate).onEvent(event);
			}
			// Else... find the listener handler method reflectively.
			else {
				String methodName = getListenerMethodName(event);

				if (methodName == null) {
					throw new InvalidDataAccessApiUsageException("No default listener method specified."
						+ " Either specify a non-null value for the 'defaultListenerMethod' property"
						+ " or override the 'getListenerMethodName' method.");
				}

				invoker = (invoker != null ? invoker : new MethodInvoker(delegate, methodName));

				invokeListenerMethod(event, methodName);
			}

		}
		catch (Throwable cause) {
			handleListenerException(cause);
		}
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
	 * Handle the given exception that arose during listener execution.
	 * The default implementation logs the exception at error level.
	 * @param cause the exception to handle
	 */
	protected void handleListenerException(Throwable cause) {
		logger.error("Listener execution failed...", cause);
	}

	/**
	 * Invoke the specified listener method.
	 * @param event the event arguments to be passed in
	 * @param methodName the method to invoke
	 * @see #getListenerMethodName
	 */
	protected void invokeListenerMethod(CqEvent event, String methodName) {
		try {
			invoker.invoke(event);
		}
		catch (InvocationTargetException e) {
			if (e.getTargetException() instanceof DataAccessException) {
				throw (DataAccessException) e.getTargetException();
			}
			else {
				throw new GemfireListenerExecutionFailedException(
					String.format("Listener method [%1$s] threw Exception...", methodName), e.getTargetException());
			}
		}
		catch (Throwable e) {
			throw new GemfireListenerExecutionFailedException(
				String.format("Failed to invoke the target listener method [%1$s]", methodName), e);
		}
	}

	private class MethodInvoker {

		private final Object delegate;

		List<Method> methods;

		MethodInvoker(Object delegate, final String methodName) {
			Class<?> c = delegate.getClass();

			this.delegate = delegate;
			methods = new ArrayList<Method>();

			ReflectionUtils.doWithMethods(c, new MethodCallback() {
					public void doWith(Method method) throws IllegalArgumentException, IllegalAccessException {
						ReflectionUtils.makeAccessible(method);
						methods.add(method);
					}
				}, new MethodFilter() {
					public boolean matches(Method method) {
						return isValidEventMethodSignature(method, methodName);
					}
				});

			Assert.isTrue(!methods.isEmpty(), String.format(
				"Cannot find a suitable method named [%1$s#%2$s] - is the method public and does it have the proper arguments?",
					c.getName(), methodName));
		}

		@SuppressWarnings("all")
		boolean isValidEventMethodSignature(Method method, String methodName) {
			if (Modifier.isPublic(method.getModifiers()) && methodName.equals(method.getName())) {
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

		void invoke(CqEvent event) throws InvocationTargetException, IllegalAccessException {
			for (Method method : methods) {
				method.invoke(delegate, getMethodArguments(method, event));
			}
		}

		Object[] getMethodArguments(Method method, CqEvent event) {
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
