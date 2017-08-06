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
package org.springframework.data.gemfire.function;

import java.lang.reflect.Method;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.geode.cache.Region;
import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.execute.FunctionContext;
import org.apache.geode.cache.execute.ResultSender;
import org.springframework.util.ObjectUtils;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;

/**
 * Invokes a given {@link Object POJO} {@link Method} as a (remote) GemFire/Geode {@link Function}.
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

	private static transient Log logger = LogFactory.getLog(PojoFunctionWrapper.class);

	private volatile boolean HA;
	private volatile boolean hasResult;
	private volatile boolean optimizeForWrite;

	private volatile int batchSize;

	private final FunctionArgumentResolver functionArgumentResolver;

	private final Method method;

	private final Object target;

	private final String id;

	public PojoFunctionWrapper(Object target, Method method, String id) {
		this.functionArgumentResolver = new FunctionContextInjectingArgumentResolver(method);
		this.target = target;
		this.method = method;
		this.id = (StringUtils.hasText(id) ? id : method.getName());
		this.HA = false;
		this.hasResult = !method.getReturnType().equals(void.class);
		this.optimizeForWrite = false;
	}

	public void setBatchSize(int batchSize) {
		this.batchSize = batchSize;
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

	@Override
	@SuppressWarnings("unchecked")
	public void execute(final FunctionContext functionContext) {

		Object[] args = this.functionArgumentResolver.resolveFunctionArguments(functionContext);

		Object result = invokeTargetMethod(args);

		if (hasResult()) {
			sendResults(functionContext.getResultSender(), result);
		}
	}

	protected final Object invokeTargetMethod(Object[] args) {

		if (logger.isDebugEnabled()) {

			logger.debug(String.format("About to invoke method [%s] on class [%s] as Function [%s]",
				this.method.getName(), this.target.getClass().getName(), getId()));

			for (Object arg : args) {
				logger.debug(String.format("Argument of type [%s] is [%s]", arg.getClass().getName(), arg.toString()));
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
				new BatchingResultSender(batchSize, resultSender).sendArrayResults(result);
			}
			else if (Iterable.class.isAssignableFrom(result.getClass())) {
				new BatchingResultSender(batchSize, resultSender).sendResults((Iterable<?>) result);
			}
			else {
				resultSender.lastResult(result);
			}
		}
	}
}
