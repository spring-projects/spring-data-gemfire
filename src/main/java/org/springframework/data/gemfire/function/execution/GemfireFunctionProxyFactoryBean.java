/*
 * Copyright 2002-2013 the original author or authors.
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
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;

import com.gemstone.gemfire.cache.execute.FunctionService;

/**
 * A proxy Factory Bean for all non-region function execution interfaces
 *  
 * @author David Turanski
 */
public class GemfireFunctionProxyFactoryBean implements FactoryBean<Object>, MethodInterceptor, BeanClassLoaderAware {

	protected volatile ClassLoader beanClassLoader = ClassUtils.getDefaultClassLoader();

	protected final Class<?> serviceInterface;

	protected volatile Object serviceProxy;

	private volatile boolean initialized;

	//protected String functionId;

	protected Log logger = LogFactory.getLog(this.getClass());

	protected final GemfireFunctionOperations gemfireFunctionOperations;

	private FunctionExecutionMethodMetadata<MethodMetadata> methodMetadata;

	/**
	 * @param serviceInterface the proxied interface
	 * @param gemfireFunctionOperations an interface used to delegate the function invocation (typically a GemFire function template)
	 */
	public GemfireFunctionProxyFactoryBean(Class<?> serviceInterface, GemfireFunctionOperations gemfireFunctionOperations) {
		Assert.notNull(serviceInterface, "'serviceInterface' must not be null");
		Assert.isTrue(serviceInterface.isInterface(), "'serviceInterface' must be an interface");
		this.serviceInterface = serviceInterface;
		this.gemfireFunctionOperations = gemfireFunctionOperations;
		this.methodMetadata = new DefaultFunctionExecutionMethodMetadata(serviceInterface);
	}



	protected Object invokeFunction(Method method, Object[] args) {
		MethodMetadata mmd = this.methodMetadata.getMethodMetadata(method);
		return this.gemfireFunctionOperations.executeAndExtract(mmd.getFunctionId(), args);
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

		return invokeFunction(invocation.getMethod(), invocation.getArguments());
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
}
