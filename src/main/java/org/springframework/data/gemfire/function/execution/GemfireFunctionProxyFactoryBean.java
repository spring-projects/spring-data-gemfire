/*
<<<<<<< Updated upstream
 * Copyright 2002-2018 the original author or authors.
=======
 * Copyright 2002-2013 the original author or authors.
>>>>>>> Stashed changes
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

/**
 * A Proxy FactoryBean for all non-Region Function Execution interfaces.
 *
 * @author David Turanski
 * @author John Blum
 * @see java.lang.reflect.Method
 * @see org.aopalliance.intercept.MethodInterceptor
 * @see org.springframework.beans.factory.BeanClassLoaderAware
 * @see org.springframework.beans.factory.FactoryBean
 */
public class GemfireFunctionProxyFactoryBean implements BeanClassLoaderAware, FactoryBean<Object>, MethodInterceptor {

	private volatile ClassLoader beanClassLoader = ClassUtils.getDefaultClassLoader();

	private volatile boolean initialized;

	private final Class<?> functionExecutionInterface;

	private volatile Object functionExecutionProxy;

	private final GemfireFunctionOperations gemfireFunctionOperations;

	protected Log logger = LogFactory.getLog(this.getClass());

	private FunctionExecutionMethodMetadata<MethodMetadata> methodMetadata;

	/**
	 * @param functionExecutionInterface the proxied interface
	 * @param gemfireFunctionOperations an interface used to delegate the function invocation (typically a GemFire function template)
	 */
	public GemfireFunctionProxyFactoryBean(Class<?> functionExecutionInterface, GemfireFunctionOperations gemfireFunctionOperations) {

		Assert.notNull(functionExecutionInterface, "'functionExecutionInterface' must not be null");
		Assert.isTrue(functionExecutionInterface.isInterface(), "'functionExecutionInterface' must be an interface");

		this.functionExecutionInterface = functionExecutionInterface;
		this.gemfireFunctionOperations = gemfireFunctionOperations;
		this.methodMetadata = new DefaultFunctionExecutionMethodMetadata(functionExecutionInterface);
	}

	protected GemfireFunctionOperations getGemfireFunctionOperations() {
		return this.gemfireFunctionOperations;
	}

	@Override
	public void setBeanClassLoader(ClassLoader classLoader) {
		this.beanClassLoader = classLoader;
	}

	@Override
	public Object invoke(MethodInvocation invocation) throws Throwable {

		if (AopUtils.isToStringMethod(invocation.getMethod())) {
			return "GemFire Function Proxy for service interface [" + this.functionExecutionInterface + "]";
		}

		if (logger.isDebugEnabled()) {
			logger.debug("invoking method " + invocation.getMethod().getName());
		}

		return invokeFunction(invocation.getMethod(), invocation.getArguments());
	}

	protected Object invokeFunction(Method method, Object[] args) {
		return this.gemfireFunctionOperations.executeAndExtract(
			this.methodMetadata.getMethodMetadata(method).getFunctionId(), args);
	}

	@Override
	public Object getObject() throws Exception {

		if (this.functionExecutionProxy == null) {
			onInit();
			Assert.notNull(this.functionExecutionProxy, "failed to initialize proxy");
		}

		return this.functionExecutionProxy;
	}

	@Override
	public Class<?> getObjectType() {
		return this.functionExecutionInterface;
	}

	@Override
	public boolean isSingleton() {
		return true;
	}

	protected void onInit() {

		if (!this.initialized) {

			ProxyFactory proxyFactory = new ProxyFactory(this.functionExecutionInterface, this);

			this.functionExecutionProxy = proxyFactory.getProxy(this.beanClassLoader);
			this.initialized = true;
		}
	}
}
