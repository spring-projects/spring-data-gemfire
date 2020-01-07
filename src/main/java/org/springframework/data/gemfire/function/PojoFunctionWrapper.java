/*
 * Copyright 2002-2020 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 */
package org.springframework.data.gemfire.function;

import static org.springframework.data.gemfire.util.CollectionUtils.asSet;

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Collections;

import org.apache.geode.cache.Region;
import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.execute.FunctionContext;
import org.apache.geode.cache.execute.ResultSender;
import org.apache.geode.management.internal.security.ResourcePermissions;
import org.apache.geode.security.ResourcePermission;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.util.ObjectUtils;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;

/**
 * Invokes a given {@link Object POJO} {@link Method} as a (remote) Pivotal GemFire/Apache Geode {@link Function}.
 *
 * If the {@link Object POJO} has a constructor that takes a {@link java.util.Map}, and the {@link Function} context
 * is a {@link Region}, the {@link Region} will be injected.
 *
 * The delegate {@link Class#getName() class name}, the {@link Method#getName() method name},
 * and {@link Method} arguments are part of the {@link Function} invocation, therefore all arguments
 * must be {@link java.io.Serializable} or an alternate serialization strategy must be used.
 *
 * The delegate {@link Class} must be on the class path of the remote cache(s).
 *
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.execute.Function
 * @since 1.2.0
 */
@SuppressWarnings("serial")
public class PojoFunctionWrapper implements Function {

	private static transient Logger logger = LoggerFactory.getLogger(PojoFunctionWrapper.class);

	private volatile boolean HA;
	private volatile boolean hasResult;
	private volatile boolean optimizeForWrite;

	private volatile int batchSize;

	private Collection<ResourcePermission> requiredPermissions = asSet(ResourcePermissions.DATA_WRITE);

	private final FunctionArgumentResolver functionArgumentResolver;

	private final Method method;

	private final Object target;

	private final String id;

	public PojoFunctionWrapper(Object target, Method method) {
		this(target, method, null);
	}

	public PojoFunctionWrapper(Object target, Method method, String id) {

		this.target = target;
		this.method = method;
		this.id = resolveId(method, id);
		this.functionArgumentResolver = newFunctionArgumentResolver(method);
		this.HA = false;
		this.hasResult = resolveHasResult(method);
		this.optimizeForWrite = false;
	}

	protected FunctionArgumentResolver newFunctionArgumentResolver(Method method) {
		return new FunctionContextInjectingArgumentResolver(method);
	}

	protected boolean resolveHasResult(Method method) {
		return !method.getReturnType().equals(void.class);
	}

	protected String resolveId(Method method, String id) {
		return StringUtils.hasText(id) ? id : method.getName();
	}

	public void setBatchSize(int batchSize) {
		this.batchSize = batchSize;
	}

	public int getBatchSize() {
		return this.batchSize;
	}

	public void setHA(boolean HA) {
		this.HA = HA;
	}

	@Override
	public boolean isHA() {
		return this.HA;
	}

	public void setHasResult(boolean hasResult) {
		this.hasResult = hasResult;
	}

	@Override
	public boolean hasResult() {
		return this.hasResult;
	}

	@Override
	public String getId() {
		return this.id;
	}

	public void setOptimizeForWrite(boolean optimizeForWrite) {
		this.optimizeForWrite = optimizeForWrite;
	}

	@Override
	public boolean optimizeForWrite() {
		return this.optimizeForWrite;
	}

	public void setRequiredPermissions(Collection<ResourcePermission> requiredPermissions) {
		this.requiredPermissions = requiredPermissions;
	}

	@Override
	public Collection<ResourcePermission> getRequiredPermissions(String regionName) {
		return Collections.unmodifiableCollection(this.requiredPermissions);
	}

	@Override
	@SuppressWarnings("unchecked")
	public void execute(FunctionContext functionContext) {

		Object[] args = this.functionArgumentResolver.resolveFunctionArguments(functionContext);

		Object result = invokeTargetMethod(args);

		if (hasResult()) {
			sendResults(functionContext.getResultSender(), result);
		}
	}

	protected final Object invokeTargetMethod(Object[] args) {

		if (logger.isDebugEnabled()) {

			if (logger.isDebugEnabled()) {
				logger.debug("About to invoke method [{}] on class [{}] as Function [{}]",
					this.method.getName(), this.target.getClass().getName(), getId());
			}

			for (Object arg : args) {
				logger.debug("Argument of type [{}] is [{}]", arg.getClass().getName(), arg.toString());
			}
		}

		return ReflectionUtils.invokeMethod(this.method, this.target, (Object[]) args);
	}

	private void sendResults(ResultSender<Object> resultSender, Object result) {

		if (result == null) {
			resultSender.lastResult(null);
		}
		else {
			if (ObjectUtils.isArray(result)) {
				new BatchingResultSender(this.batchSize, resultSender).sendArrayResults(result);
			}
			else if (Iterable.class.isAssignableFrom(result.getClass())) {
				new BatchingResultSender(this.batchSize, resultSender).sendResults((Iterable<?>) result);
			}
			else {
				resultSender.lastResult(result);
			}
		}
	}
}
