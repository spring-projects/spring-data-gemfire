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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.gemstone.gemfire.cache.execute.Execution;
import com.gemstone.gemfire.cache.execute.Function;

/**
 * 
 * The base class for Gemfire function templates used to invoke Gemfire functions
 * @author David Turanski
 *
 */
abstract class AbstractFunctionTemplate  implements GemfireFunctionOperations {

	protected Log log = LogFactory.getLog(this.getClass());
	 
	protected long timeout;
	
	@Override
	public <T> Iterable<T> execute(Function function, Object... args) {
		 FunctionExecution functionExecution = getFunctionExecution()
				 .setArgs(args)
				 .setFunction(function)
				 .setTimeout(timeout);
		 return execute(functionExecution);
	}

	@Override
	public <T> T executeAndExtract(Function function, Object... args) {
		 FunctionExecution functionExecution = getFunctionExecution()
				 .setArgs(args)
				 .setFunction(function)
				 .setTimeout(timeout);
		return executeAndExtract(functionExecution);		
	}

	@Override
	public <T> Iterable<T> execute(String functionId, Object... args) {
		FunctionExecution functionExecution = getFunctionExecution()
				 .setArgs(args)
				 .setFunctionId(functionId)
				 .setTimeout(timeout);
		 return execute(functionExecution);		 
	}
	 
	@Override
	public <T> T executeAndExtract(String functionId, Object... args) {
		 FunctionExecution functionExecution = getFunctionExecution()
				 .setArgs(args)
				 .setFunctionId(functionId)
				 .setTimeout(timeout);
		return executeAndExtract(functionExecution);		
	}

	@Override
	public <T> T execute(GemfireFunctionCallback<T> callback) {
		Execution execution = getFunctionExecution().getExecution();
		return callback.doInGemfire(execution);
	}
	 
	
	protected <T> Iterable<T> execute(FunctionExecution execution) {
		 execution.setTimeout(timeout);
		 return execution.execute();
	}
	
	protected <T> T executeAndExtract(FunctionExecution execution) {
		 execution.setTimeout(timeout);
		 return execution.executeAndExtract();
	}
	
	public void setTimeout(long timeout) {
		this.timeout = timeout;
	}
	
	protected abstract FunctionExecution getFunctionExecution();
}
