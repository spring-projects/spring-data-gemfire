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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.execute.Execution;
import com.gemstone.gemfire.cache.execute.Function;
import com.gemstone.gemfire.cache.execute.FunctionException;
import com.gemstone.gemfire.cache.execute.FunctionService;
import com.gemstone.gemfire.cache.execute.ResultCollector;

/**
 * Base class for * Creating a GemFire {@link Execution} using {@link FunctionService}
 * Protected setters support method chaining
 * @author  David Turanski
 */

abstract class AbstractFunctionExecution {
	protected final Log logger = LogFactory.getLog(this.getClass());
	
	private volatile ResultCollector<?, ?> resultCollector;
	private Object[] args;
	private  Function function;
	private  String functionId;
	private long timeout;

	public AbstractFunctionExecution(Function function, Object... args) {
		Assert.notNull(function,"function cannot be null");
		this.function  = function;
		this.functionId = function.getId();
		this.args = args;
	}
	
	public AbstractFunctionExecution(String functionId, Object... args) {
		Assert.isTrue(StringUtils.hasLength(functionId),"functionId cannot be null or empty");
		this.functionId  = functionId;
		this.args = args;
	}
	
	AbstractFunctionExecution() {
	}
	
	ResultCollector<?, ?> getCollector() {
		return resultCollector;
	}

	Object[] getArgs() {
		return args;
	}

	String getFunctionId() {
		return functionId;
	}
	
	Function getFunction() {
		return function;
	}
	
	long getTimeout() {
		return timeout;
	}	
		
	@SuppressWarnings("unchecked")
	<T> Iterable<T> execute() {
		Execution execution = this.getExecution();
		if (getKeys() != null) {
			execution = execution.withFilter(getKeys());
		}
		if (getCollector() != null) {
			execution = execution.withCollector(getCollector());
		}
		
		ResultCollector<?,?> resultCollector = null;
		
		execution = execution.withArgs(getArgs());
		
		if (isRegisteredFunction()){
			
			resultCollector =  (ResultCollector<?,?>) execution.execute(functionId);
		} else {
			resultCollector = (ResultCollector<?,?>) execution.execute(function);
		}
		
		logger.debug("ResultsCollector:" + resultCollector.getClass().getName());
		
		Iterable<T> results = null;
		
		if (this.timeout > 0 ){
			try {
				results= (Iterable<T>)resultCollector.getResult(this.timeout, TimeUnit.MILLISECONDS);
			}
			catch (FunctionException e) {
				throw new RuntimeException(e);
			}
			catch (InterruptedException e) {
				throw new RuntimeException(e);
			}
		} else {
			 
			results =  (Iterable<T>) resultCollector.getResult();
		}
		
		return replaceSingletonNullCollectionWithEmptyList(results);
		
	}
	
	<T> T executeAndExtract() {
		Iterable<T> results = this.execute();
		if (results == null || !results.iterator().hasNext()) {
			return null;
		}
		
		return results.iterator().next();
	}
	
	protected abstract Execution getExecution();
	
	protected AbstractFunctionExecution setFunctionId(String functionId) {
		this.functionId = functionId;
		return this;
	}
	
	protected AbstractFunctionExecution setFunction(Function function) {
		this.function = function;
		return this;
	}
	
	protected AbstractFunctionExecution setArgs(Object... args) {
		this.args = args;
		return this;
	}
	
	protected Set<?> getKeys() {
		return null;
	}
		
	protected AbstractFunctionExecution setTimeout(long timeout) {
		this.timeout = timeout;
		return this;
	}
	
	protected AbstractFunctionExecution setResultCollector(ResultCollector<?,?> resultCollector) {
		this.resultCollector = resultCollector;
		return this;
	}


	/**
	 * @return
	 */
	private boolean isRegisteredFunction() {
		 return function == null;
	}
	
	private <T> Iterable<T> replaceSingletonNullCollectionWithEmptyList(Iterable<T> results) {
		if (results == null) {
			return results;
		}
		Iterator<T> it = results.iterator();
		
		if (!it.hasNext()) {
			return results;
		}
		
		if (it.next()==null && !it.hasNext()) {
			return new ArrayList<T>();
		}
		
		return results;
		
	}
	 
}