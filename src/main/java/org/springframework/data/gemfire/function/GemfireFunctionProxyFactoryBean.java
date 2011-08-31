/*
 * Copyright 2002-2011 the original author or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function;

/**
 * @author David Turanski
 *
 */

import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Set;

import org.aopalliance.intercept.MethodInterceptor;
import org.aopalliance.intercept.MethodInvocation;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.aop.framework.ProxyFactory;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.factory.BeanClassLoaderAware;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;

import com.gemstone.gemfire.cache.execute.FunctionException;

/**
 * 
 * @author David Turanski
 * 
 */
public class GemfireFunctionProxyFactoryBean implements FactoryBean<Object>, MethodInterceptor, BeanClassLoaderAware {
	private volatile ClassLoader beanClassLoader = ClassUtils.getDefaultClassLoader();

	private final Class<?> serviceInterface;

	private volatile Object serviceProxy;

	private volatile boolean initialized;

	private final String delegateClassName;

	private final ThreadLocal<Set<? extends Serializable>> filter;

	private boolean methodInvokingFunctionRegistered;

	private volatile String regionName;

	private final GemfireFunctionTemplate<?> gemfireFunctionTemplate;

	private static Log logger = LogFactory.getLog(GemfireFunctionProxyFactoryBean.class);

	public GemfireFunctionProxyFactoryBean(Class<?> serviceInterface, String delegateClassName,
			GemfireFunctionTemplate<?> gemfireFunctionTemplate) {
		this.delegateClassName = delegateClassName;
		Assert.notNull(serviceInterface, "'serviceInterface' must not be null");
		Assert.isTrue(serviceInterface.isInterface(), "'serviceInterface' must be an interface");
		checkServiceInterfaceTypesAreSerializable(serviceInterface);
		this.serviceInterface = serviceInterface;

		Assert.notNull(gemfireFunctionTemplate);
		this.gemfireFunctionTemplate = gemfireFunctionTemplate;
		this.filter = new ThreadLocal<Set<? extends Serializable>>();
	}

	// @Override
	public void setBeanClassLoader(ClassLoader classLoader) {
		beanClassLoader = classLoader;
	}

	@SuppressWarnings("unchecked")
	// @Override
	public Object invoke(MethodInvocation invocation) throws Throwable {

		if (AopUtils.isToStringMethod(invocation.getMethod())) {
			return "Gemfire function proxy for service interface [" + this.serviceInterface + "]";
		}

		logger.debug("invoking method " + invocation.getMethod().getName());

		if (isSetFilterMethod(invocation.getMethod())) {
			setFilter((Set<? extends Serializable>) invocation.getArguments()[0]);
			return getObject();
		}

		RemoteMethodInvocation remoteInvocation = new RemoteMethodInvocation(this.delegateClassName, invocation
				.getMethod().getName(), invocation.getArguments());

		List<?> results = null;

		if (this.methodInvokingFunctionRegistered) {
			if (this.regionName != null) {
				results = this.gemfireFunctionTemplate.executeOnRegion(MethodInvokingFunction.FUNCTION_ID,
						this.regionName, this.getFilter(), remoteInvocation);
			}
			else {
				if (this.getFilter() != null ) {
					logger.warn("No region specified. Filter has no effect on a data independent function exectution");
				}
				results = this.gemfireFunctionTemplate.executeOnServers(MethodInvokingFunction.FUNCTION_ID,
						remoteInvocation);
			}

		}
		else {
			if (this.regionName != null) {
				results = this.gemfireFunctionTemplate.executeOnRegion(new MethodInvokingFunction(), this.regionName,
						this.getFilter(), remoteInvocation);
			}
			else {
				if (this.getFilter() != null ) {
					logger.warn("No region specified. Filter has no effect on a data independent function exectution");
				}
				results = this.gemfireFunctionTemplate.executeOnServers(new MethodInvokingFunction(), remoteInvocation);
			}
		}

		return extractResult(results, invocation.getMethod().getReturnType());
	}

	private Object extractResult(List<?> results, Class<?> returnType) {
		Object result = null;
		if (List.class.isAssignableFrom(returnType)) {
			result = results;
		}
		else {
			int nonNullItems = 0;
			for (Object obj : results) {
				if (obj != null) {
					if (++nonNullItems > 1) {
						throw new FunctionException("multiple results found for single valued return type");
					}
					else {
						result = obj;
					}
				}
			}
		}
		if (logger.isDebugEnabled()) {
			logger.debug("returning result as " + result.getClass().getName());
		}
		return result;
	}

	private static boolean isSetFilterMethod(Method method) {
		try {
			Method setFilterMethod = FilterAware.class.getMethod("setFilter", Set.class);
			return method.equals(setFilterMethod);
		}
		catch (SecurityException e) {
		}
		catch (NoSuchMethodException e) {
		}
		return false;
	}

	// @Override
	public Object getObject() throws Exception {
		if (this.serviceProxy == null) {
			this.onInit();
			Assert.notNull(this.serviceProxy, "failed to initialize proxy");
		}
		return this.serviceProxy;
	}

	// @Override
	public Class<?> getObjectType() {
		return (this.serviceInterface != null ? this.serviceInterface : null);
	}

	// @Override
	public boolean isSingleton() {
		return true;
	}

	protected Set<?> getFilter() {
		return filter.get();
	}

	protected void setFilter(Set<? extends Serializable> filter) {
		this.filter.set(filter);
	}

	public void setMethodInvokingFunctionRegistered(boolean methodInvokingFunctionRegistered) {
		this.methodInvokingFunctionRegistered = methodInvokingFunctionRegistered;
	}

	public void setRegionName(String regionName) {
		this.regionName = regionName;
	}

	// TODO: Use something like cglib to implement setFilter() directly
	// to eliminate the need to cast the proxy to FilterAware
	protected void onInit() {
		if (this.initialized) {
			return;
		}
		ProxyFactory proxyFactory = new ProxyFactory(serviceInterface, this);
		proxyFactory.addInterface(FilterAware.class);
		this.serviceProxy = proxyFactory.getProxy(this.beanClassLoader);
		this.initialized = true;
	}

	private void checkServiceInterfaceTypesAreSerializable(Class<?> serviceInterface) {
		for (Method method : serviceInterface.getMethods()) {
			Class<?> returnType = method.getReturnType();
			if (!method.getName().equals("setFilter")) {
				Assert.isTrue(
						Void.class.isAssignableFrom(returnType) || Serializable.class.isAssignableFrom(returnType),
						"return type must be Serializable on method " + method.getName());
				for (Class<?> parameterType : method.getParameterTypes()) {
					Assert.isTrue(Serializable.class.isAssignableFrom(parameterType),
							"parameter types must be Serializable on method " + method.getName());
				}
			}
		}
	}

}
