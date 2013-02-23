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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.gemstone.gemfire.cache.execute.Execution;
import com.gemstone.gemfire.cache.execute.Function;
import com.gemstone.gemfire.cache.execute.ResultCollector;

/**
 * 
 * The base class for Gemfire function templates used to invoke Gemfire functions
 * @author David Turanski
 *
 */
abstract class AbstractFunctionTemplate  implements GemfireFunctionOperations {

	protected Log log = LogFactory.getLog(this.getClass());
	 
	protected long timeout;
	protected volatile ResultCollector<?, ?> resultCollector;
	
	@Override
	public <T> Iterable<T> execute(Function function, Object... args) {
		 AbstractFunctionExecution functionExecution = getFunctionExecution()
				 .setArgs(args)
				 .setFunction(function);
		 return execute(functionExecution);
	}

	@Override
	public <T> T executeAndExtract(Function function, Object... args) {
		 AbstractFunctionExecution functionExecution = getFunctionExecution()
				 .setArgs(args)
				 .setFunction(function);
		 		
		return this.<T> executeAndExtract(functionExecution);		
	}

	@Override
	public <T> Iterable<T> execute(String functionId, Object... args) {
		AbstractFunctionExecution functionExecution = getFunctionExecution()
				 .setArgs(args)
				 .setFunctionId(functionId);
		 return execute(functionExecution);		 
	}
	 
	@Override
	public <T> T executeAndExtract(String functionId, Object... args) {
		 AbstractFunctionExecution functionExecution = getFunctionExecution()
				 .setArgs(args)
				 .setFunctionId(functionId);
		return this.<T>executeAndExtract(functionExecution);		
	}

	@Override
	public <T> T execute(GemfireFunctionCallback<T> callback) {
		Execution execution = getFunctionExecution().getExecution();
		return callback.doInGemfire(execution);
	}
	 
	
	protected <T> Iterable<T> execute(AbstractFunctionExecution execution) {
		 execution.setTimeout(timeout)
		 .setResultCollector(resultCollector);
		 return execution.execute();
	}
	
	protected <T> T executeAndExtract(AbstractFunctionExecution execution) {
		 execution.setTimeout(timeout)
		 .setResultCollector(resultCollector);
		 return execution.<T>executeAndExtract();
	}
	
	public void setTimeout(long timeout) {
		this.timeout = timeout;
	}
	
	public void setResultCollector(ResultCollector<?,?> resultCollector) {
		this.resultCollector = resultCollector;
	}
	
	public ResultCollector<?,?> getResultCollector() {
		return this.resultCollector;
	}
	
	protected abstract AbstractFunctionExecution getFunctionExecution();
}
