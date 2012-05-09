/*
 * Copyright 2011-2012 the original author or authors.
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
import org.springframework.dao.DataAccessException;
import org.springframework.dao.InvalidDataAccessApiUsageException;
import org.springframework.data.gemfire.listener.ContinuousQueryListener;
import org.springframework.data.gemfire.listener.GemfireListenerExecutionFailedException;
import org.springframework.util.Assert;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.ReflectionUtils.MethodCallback;
import org.springframework.util.ReflectionUtils.MethodFilter;

import com.gemstone.gemfire.cache.Operation;
import com.gemstone.gemfire.cache.query.CqEvent;
import com.gemstone.gemfire.cache.query.CqQuery;

/**
 * Event listener adapter that delegates the handling of messages to target
 * listener methods via reflection, with flexible event type conversion.
 * Allows listener methods to operate on event content types, completely
 * independent from the GemFire API.
 * 
 * <p/>Modeled as much as possible after the JMS MessageListenerAdapter in
 * Spring Framework.
 *
 * <p>By default, the content of incoming GemFire events gets extracted before
 * being passed into the target listener method, to let the target method
 * operate on event content types such as Object or Operation instead of
 * the raw {@link CqEvent}.
 *
 * <p>Find below some examples of method signatures compliant with this
 * adapter class. This first example handles all <code>CqEvent</code> types
 * and gets passed the contents of each <code>event</code> type as an
 * argument.
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
 * @see org.springframework.jms.listener.adapter.MessageListenerAdapter
 */
public class ContinuousQueryListenerAdapter implements ContinuousQueryListener {

	private class MethodInvoker {
		private final Object delegate;
		List<Method> methods;

		MethodInvoker(Object delegate, final String methodName) {
			this.delegate = delegate;

			Class<?> c = delegate.getClass();

			methods = new ArrayList<Method>();

			ReflectionUtils.doWithMethods(c, new MethodCallback() {

				public void doWith(Method method) throws IllegalArgumentException, IllegalAccessException {
					ReflectionUtils.makeAccessible(method);
					methods.add(method);
				}

			}, new MethodFilter() {
				public boolean matches(Method method) {
					if (Modifier.isPublic(method.getModifiers()) && methodName.equals(method.getName())) {

						// check out the arguments
						Class<?>[] parameterTypes = method.getParameterTypes();
						int objects = 0;
						int operations = 0;

						if (parameterTypes.length > 0) {
							for (Class<?> paramType : parameterTypes) {

								if (Object.class.equals(paramType)) {
									objects++;
									if (objects > 2) {
										return false;
									}
								}
								else if (Operation.class.equals(paramType)) {
									operations++;
									if (operations > 2) {
										return false;
									}
								}
								else if (CqEvent.class.equals(paramType)) {
								}
								else if (Throwable.class.equals(paramType)) {
								}
								else if (byte[].class.equals(paramType)) {
								}
								else if (CqQuery.class.equals(paramType)) {
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
			});

			Assert.isTrue(!methods.isEmpty(), "Cannot find a suitable method named [" + c.getName() + "#" + methodName
					+ "] - is the method public and has the proper arguments?");
		}

		void invoke(CqEvent event) throws InvocationTargetException, IllegalAccessException {

			for (Method m : methods) {
				Class<?>[] types = m.getParameterTypes();
				Object[] args = new Object[types.length];

				boolean value = false;
				boolean query = false;

				for (int i = 0; i < types.length; i++) {
					Class<?> paramType = types[i];

					if (Object.class.equals(paramType)) {
						args[i] = (!value ? event.getKey() : event.getNewValue());
						value = true;
					}
					else if (Operation.class.equals(paramType)) {
						args[i] = (!query ? event.getBaseOperation() : event.getQueryOperation());
						query = true;
					}
					else if (CqEvent.class.equals(paramType)) {
						args[i] = event;
					}
					else if (Throwable.class.equals(paramType)) {
						args[i] = event.getThrowable();
					}
					else if (byte[].class.equals(paramType)) {
						args[i] = event.getDeltaValue();
					}
					else if (CqQuery.class.equals(paramType)) {
						args[i] = event.getCq();
					}
				}

				m.invoke(delegate, args);
			}
		}
	}


	/**
	 * Out-of-the-box value for the default listener method: "handleEvent".
	 */
	public static final String ORIGINAL_DEFAULT_LISTENER_METHOD = "handleEvent";


	/** Logger available to subclasses */
	protected final Log logger = LogFactory.getLog(getClass());

	private Object delegate;

	private String defaultListenerMethod = ORIGINAL_DEFAULT_LISTENER_METHOD;

	private MethodInvoker invoker;

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
		Assert.notNull(delegate, "Delegate must not be null");
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
	 * Specify the name of the default listener method to delegate to,
	 * for the case where no specific listener method has been determined.
	 * Out-of-the-box value is {@link #ORIGINAL_DEFAULT_LISTENER_METHOD "handleEvent}.
	 * @see #getListenerMethodName
	 */
	public void setDefaultListenerMethod(String defaultListenerMethod) {
		this.defaultListenerMethod = defaultListenerMethod;
		this.invoker = null;
	}

	/**
	 * Return the name of the default listener method to delegate to.
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

			// Check whether the delegate is a ContinuousQueryListener impl itself.
			// In that case, the adapter will simply act as a pass-through.
			if (delegate != this) {
				if (delegate instanceof ContinuousQueryListener) {
					((ContinuousQueryListener) delegate).onEvent(event);
					return;
				}
			}

			// Regular case: find a handler method reflectively.
			String methodName = getListenerMethodName(event);
			if (invoker == null) {
				invoker = new MethodInvoker(delegate, methodName);
			}
			if (methodName == null) {
				throw new InvalidDataAccessApiUsageException("No default listener method specified: "
						+ "Either specify a non-null value for the 'defaultListenerMethod' property or "
						+ "override the 'getListenerMethodName' method.");
			}

			invokeListenerMethod(event, methodName);
		} catch (Throwable th) {
			handleListenerException(th);
		}
	}

	/**
	 * Handle the given exception that arose during listener execution.
	 * The default implementation logs the exception at error level.
	 * @param ex the exception to handle
	 */
	protected void handleListenerException(Throwable ex) {
		logger.error("Listener execution failed", ex);
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
	protected String getListenerMethodName(CqEvent event) {
		return getDefaultListenerMethod();
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
		} catch (InvocationTargetException ex) {
			Throwable targetEx = ex.getTargetException();
			if (targetEx instanceof DataAccessException) {
				throw (DataAccessException) targetEx;
			}
			else {
				throw new GemfireListenerExecutionFailedException("Listener method '" + methodName
						+ "' threw exception", targetEx);
			}
		} catch (Throwable ex) {
			throw new GemfireListenerExecutionFailedException("Failed to invoke target method '" + methodName, ex);
		}
	}
}