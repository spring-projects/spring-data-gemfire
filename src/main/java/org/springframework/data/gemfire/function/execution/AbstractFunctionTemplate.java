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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.geode.cache.execute.Function;
import org.apache.geode.cache.execute.ResultCollector;

/**
 * The base class for GemFire FunctionTemplates used to invoke GemFire Functions.
 *
 * @author David Turanski
 * @author John Blum
 * @see org.apache.geode.cache.execute.Function
 * @see org.apache.geode.cache.execute.ResultCollector
 */
abstract class AbstractFunctionTemplate implements GemfireFunctionOperations {

	protected Log log = LogFactory.getLog(this.getClass());

	protected long timeout;

	protected volatile ResultCollector<?, ?> resultCollector;

	@Override
	public <T> Iterable<T> execute(Function function, Object... args) {
		 return execute(getFunctionExecution().setArgs(args).setFunction(function));
	}

	@Override
	public <T> T executeAndExtract(Function function, Object... args) {
		return executeAndExtract(getFunctionExecution().setArgs(args).setFunction(function));
	}

	@Override
	public <T> Iterable<T> execute(String functionId, Object... args) {
		return execute(getFunctionExecution().setArgs(args).setFunctionId(functionId));
	}

	@Override
	public <T> T executeAndExtract(String functionId, Object... args) {
		return executeAndExtract(getFunctionExecution().setArgs(args).setFunctionId(functionId));
	}

	@Override
	public void executeWithNoResult(String functionId, Object... args) {
		execute(getFunctionExecution().setArgs(args).setFunctionId(functionId), false);
	}

	@Override
	public <T> T execute(GemfireFunctionCallback<T> callback) {
		return callback.doInGemfire(getFunctionExecution().getExecution());
	}

	protected <T> Iterable<T> execute(AbstractFunctionExecution execution) {
		 return execution.setTimeout(timeout).setResultCollector(resultCollector).execute();
	}

	protected <T> Iterable<T> execute(AbstractFunctionExecution execution, boolean returnResult) {
		 return execution.setTimeout(timeout).setResultCollector(resultCollector).execute(returnResult);
	}

	protected <T> T executeAndExtract(AbstractFunctionExecution execution) {
		 return execution.setTimeout(timeout).setResultCollector(resultCollector).executeAndExtract();
	}

	public void setResultCollector(ResultCollector<?,?> resultCollector) {
		this.resultCollector = resultCollector;
	}

	public ResultCollector<?,?> getResultCollector() {
		return this.resultCollector;
	}

	public void setTimeout(long timeout) {
		this.timeout = timeout;
	}

	protected abstract AbstractFunctionExecution getFunctionExecution();

}
