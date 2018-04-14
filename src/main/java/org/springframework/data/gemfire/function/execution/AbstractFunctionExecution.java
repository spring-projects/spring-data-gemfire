/*
 * Copyright 2002-2018 the original author or authors.
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

import java.util.Collections;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.geode.cache.execute.Execution;
import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.execute.FunctionException;
import org.apache.geode.cache.execute.FunctionService;
import org.apache.geode.cache.execute.ResultCollector;
import org.springframework.util.Assert;

/**
 * Base class for * Creating a GemFire {@link Execution} using {@link FunctionService}.  Protected setters support
 * method chaining.
 *
 * @author David Turanski
 * @author John Blum
 */
abstract class AbstractFunctionExecution {

	private final static String NO_RESULT_MESSAGE = "Cannot return any result as the Function#hasResult() is false";

	private long timeout;

	private Function function;

	protected final Log logger = LogFactory.getLog(this.getClass());

	private Object[] args;

	private volatile ResultCollector<?, ?> resultCollector;

	private String functionId;

	public AbstractFunctionExecution(Function function, Object... args) {

		Assert.notNull(function, "Function cannot be null");

		this.function = function;
		this.functionId = function.getId();
		this.args = args;
	}

	public AbstractFunctionExecution(String functionId, Object... args) {

		Assert.hasText(functionId, "FunctionId cannot be null or empty");

		this.functionId = functionId;
		this.args = args;
	}

	AbstractFunctionExecution() {
	}

	Object[] getArgs() {
		return this.args;
	}

	ResultCollector<?, ?> getCollector() {
		return this.resultCollector;
	}

	Function getFunction() {
		return this.function;
	}

	String getFunctionId() {
		return this.functionId;
	}

	long getTimeout() {
		return this.timeout;
	}

	<T> Iterable<T> execute() {
		return execute(true);
	}

	@SuppressWarnings("unchecked")
	<T> Iterable<T> execute(Boolean returnResult) {

		Execution execution = getExecution();

		execution = execution.setArguments(getArgs());
		execution = getCollector() != null ? execution.withCollector(getCollector()) : execution;
		execution = getKeys() != null ? execution.withFilter(getKeys()) : execution;

		ResultCollector<?, ?> resultCollector;

		if (isRegisteredFunction()) {
			resultCollector = execution.execute(this.functionId);
		}
		else {
			resultCollector = execution.execute(this.function);

			if (!this.function.hasResult()) {
				return null;
			}
		}

		if (!returnResult) {
			return null;
		}

		if (logger.isDebugEnabled()) {
			logger.debug("Using ResultsCollector " + resultCollector.getClass().getName());
		}

		Iterable<T> results = null;

		try {
			if (this.timeout > 0) {
				try {
					results = (Iterable<T>) resultCollector.getResult(this.timeout, TimeUnit.MILLISECONDS);
				}
				catch (FunctionException | InterruptedException cause) {
					throw new RuntimeException(cause);
				}
			}
			else {
				results = (Iterable<T>) resultCollector.getResult();
			}

			return replaceSingletonNullCollectionWithEmptyList(results);
		}
		catch (FunctionException cause) {
			// TODO Come up with a better way to determine that the function should not return a result;
			if (!cause.getMessage().equals(NO_RESULT_MESSAGE)) {
				throw cause;
			}
		}

		return results;
	}

	@SuppressWarnings("unchecked")
	<T> T executeAndExtract() {

		Iterable<T> results = execute();

		if (results == null || !results.iterator().hasNext()) {
			return null;
		}

		Object result = results.iterator().next();

		if (result instanceof Throwable) {
			throw new FunctionException(String.format("Execution of Function %s failed",
				(this.function != null ? this.function.getClass().getName()
					: String.format("with ID [%s]", this.functionId))), (Throwable) result);
		}

		return (T) result;
	}

	protected abstract Execution getExecution();

	protected AbstractFunctionExecution setArgs(Object... args) {
		this.args = args;
		return this;
	}

	protected AbstractFunctionExecution setFunction(Function function) {
		this.function = function;
		return this;
	}

	protected AbstractFunctionExecution setFunctionId(String functionId) {
		this.functionId = functionId;
		return this;
	}

	protected AbstractFunctionExecution setResultCollector(ResultCollector<?, ?> resultCollector) {
		this.resultCollector = resultCollector;
		return this;
	}

	protected AbstractFunctionExecution setTimeout(long timeout) {
		this.timeout = timeout;
		return this;
	}

	protected Set<?> getKeys() {
		return null;
	}

	private boolean isRegisteredFunction() {
		return this.function == null;
	}

	private <T> Iterable<T> replaceSingletonNullCollectionWithEmptyList(Iterable<T> results) {

		if (results != null) {

			Iterator<T> it = results.iterator();

			if (!it.hasNext()) {
				return results;
			}

			if (it.next() == null && !it.hasNext()) {
				return Collections.emptyList();
			}
		}

		return results;
	}
}
