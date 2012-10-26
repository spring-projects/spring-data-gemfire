/*
 * Copyright 2002-2011 the original author or authors.
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

import java.io.Serializable;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

import com.gemstone.gemfire.cache.execute.Execution;
import com.gemstone.gemfire.cache.execute.Function;
import com.gemstone.gemfire.cache.execute.FunctionException;
import com.gemstone.gemfire.cache.execute.ResultCollector;

/**
 * @author  David Turanski
 */

public abstract class FunctionExecution<T> {
	protected final Log logger = LogFactory.getLog(this.getClass());
	
	private volatile ResultCollector<?, ?> collector;
	private final Serializable[] args;
	private  Function function;
	private final String functionId;
	private long timeout;

	public FunctionExecution(Function function, Serializable... args) {
		Assert.notNull(function,"function cannot be null");
		this.function  = function;
		this.functionId = function.getId();
		this.args = args;
	}
	
	public FunctionExecution(String functionId, Serializable... args) {
		Assert.isTrue(StringUtils.hasLength(functionId),"functionId cannot be null or empty");
		this.functionId  = functionId;
		this.args = args;
	}
	
	
	public ResultCollector<?, ?> getCollector() {
		return collector;
	}

	public Serializable[] getArgs() {
		return args;
	}

	public String getFunctionId() {
		return functionId;
	}
	
	public Function getFunction() {
		return function;
	}
 
	public void setCollector(ResultCollector<?,?> collector) {
		this.collector = collector;
	} 
	
		
	@SuppressWarnings("unchecked")
	public List<T> execute() {
		Execution execution = this.getExecution();
		if (getKeys() != null) {
			execution = execution.withFilter(getKeys());
		}
		if (getCollector() != null) {
			execution = execution.withCollector(getCollector());
		}
		
		ResultCollector<?,?> resultsCollector = null;
		
		execution = execution.withArgs(getArgs());
		
		if (isRegisteredFunction()){
			
			resultsCollector =  (ResultCollector<?,?>) execution.execute(functionId);
		} else {
			resultsCollector = (ResultCollector<?,?>) execution.execute(function);
		}
		
		if (this.timeout > 0 ){
			try {
				return (List<T>)resultsCollector.getResult(this.timeout, TimeUnit.MILLISECONDS);
			}
			catch (FunctionException e) {
				throw new RuntimeException(e);
			}
			catch (InterruptedException e) {
				throw new RuntimeException(e);
			}
		} else {
			return (List<T>)resultsCollector.getResult();
		}
	}
	
	public T executeAndExtract() {
		return this.execute().get(0);
	}
	
	protected abstract Execution getExecution();
	
	protected Set<?> getKeys() {
		return null;
	}
		
	public void setTimeout(long timeout) {
		this.timeout = timeout;
	}

	public long getTimeout() {
		return timeout;
	}

	/**
	 * @return
	 */
	private boolean isRegisteredFunction() {
		 return function == null;
	}
	 
}