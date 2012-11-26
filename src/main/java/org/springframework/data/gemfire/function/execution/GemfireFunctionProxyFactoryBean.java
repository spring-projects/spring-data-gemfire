/*
 * Copyright 2002-2012 the original author or authors.
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
package org.springframework.data.gemfire.function.execution;

import java.lang.reflect.Method;

import org.aopalliance.intercept.MethodInterceptor;
import org.aopalliance.intercept.MethodInvocation;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.aop.framework.ProxyFactory;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.factory.BeanClassLoaderAware;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.data.gemfire.function.config.FunctionId;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;

import com.gemstone.gemfire.cache.execute.FunctionException;

/**
 * A proxy Factory Bean for all non-region function execution interfaces
 *  
 * @author David Turanski
 *
 */
public class GemfireFunctionProxyFactoryBean implements FactoryBean<Object>, MethodInterceptor, BeanClassLoaderAware, InitializingBean {

	protected volatile ClassLoader beanClassLoader = ClassUtils.getDefaultClassLoader();

	protected final Class<?> serviceInterface;

	protected volatile Object serviceProxy;

	private volatile boolean initialized;

	protected String functionId;

	protected Log logger = LogFactory.getLog(this.getClass());

	protected final GemfireFunctionOperations gemfireFunctionOperations;

	private FunctionExecutionMethodMetadata<MethodMetadata> methodMetadata;

	/**
	 * @param serviceInterface the proxied interface
	 * @param functionId the associated function id (must be a function registered by this id with the GemFire {@link FunctionService}
	 * @param gemfireFunctionOperations an interface used to delegate the function invocation (typically a GemFire function template)
	 */
	public GemfireFunctionProxyFactoryBean(Class<?> serviceInterface,
			GemfireFunctionOperations gemfireFunctionOperations) {
		Assert.notNull(serviceInterface, "'serviceInterface' must not be null");
		Assert.isTrue(serviceInterface.isInterface(), "'serviceInterface' must be an interface");
		this.serviceInterface = serviceInterface;
		this.gemfireFunctionOperations = gemfireFunctionOperations;
		this.methodMetadata = new DefaultFunctionExecutionMethodMetadata(serviceInterface);
	}



	protected Iterable<?> invokeFunction(Method method, Object[] args) {
		MethodMetadata mmd = this.methodMetadata.getMethodMetadata(method);
		return this.gemfireFunctionOperations.execute(mmd.getFunctionId(), args);
	}

	@Override
	public void setBeanClassLoader(ClassLoader classLoader) {
		beanClassLoader = classLoader;
	}

	@Override
	public Object invoke(MethodInvocation invocation) throws Throwable {

		if (AopUtils.isToStringMethod(invocation.getMethod())) {
			return "Gemfire function proxy for service interface [" + this.serviceInterface + "]";
		}

		if (logger.isDebugEnabled()) {
			logger.debug("invoking method " + invocation.getMethod().getName());
		}

		Iterable<?> results = invokeFunction(invocation.getMethod(), invocation.getArguments());

		return extractResult(results, invocation.getMethod().getReturnType());
	}


	@Override
	public Object getObject() throws Exception {
		if (this.serviceProxy == null) {
			this.onInit();
			Assert.notNull(this.serviceProxy, "failed to initialize proxy");
		}
		return this.serviceProxy;
	}

	@Override
	public Class<?> getObjectType() {
		return (this.serviceInterface != null ? this.serviceInterface : null);
	}

	@Override
	public boolean isSingleton() {
		return true;
	}

	protected void onInit() {
		if (this.initialized) {
			return;
		}
		ProxyFactory proxyFactory = new ProxyFactory(serviceInterface, this);
		this.serviceProxy = proxyFactory.getProxy(this.beanClassLoader);
		this.initialized = true;
	}
	
	protected String annotatedFunctionId(Method method) {
		FunctionId functionIdAnnotation = method.getAnnotation(FunctionId.class);
		return (functionIdAnnotation == null) ? null: functionIdAnnotation.value();  
	}
	

	/**
	 * Optional to set a default function Id for a single method interface with no {code}@FunctionId{code} annotations
	 * @param functionId
	 */
	protected void setFunctionId(String functionId) {
		this.functionId = functionId;
	}

	/*
	 * Match the result to the declared return type
	 */
	private Object extractResult(Iterable<?> results, Class<?> returnType) {
		Object result = null;
		if (results != null) {
			if (Iterable.class.isAssignableFrom(returnType)) {
				result = results;
			} else {
				int nonNullItems = 0;
				for (Object obj : results) {
					if (obj != null) {
						if (++nonNullItems > 1) {
							throw new FunctionException("multiple results found for single valued return type");
						} else {
							result = obj;
						}
					}
				}
			}
			if (logger.isDebugEnabled()) {
				logger.debug("returning result as " + result.getClass().getName());
			}
		}
		return result;
	}

	/* (non-Javadoc)
	 * @see org.springframework.beans.factory.InitializingBean#afterPropertiesSet()
	 */
	@Override
	public void afterPropertiesSet() throws Exception {
		if (this.functionId != null) {
		   Assert.isTrue(this.methodMetadata.isSingletonInterface(), "cannot assign default function id if interface has multiple methods");
		   this.methodMetadata.getSingletonMethodMetada().setFunctionId(this.functionId);
		}
	}
}
