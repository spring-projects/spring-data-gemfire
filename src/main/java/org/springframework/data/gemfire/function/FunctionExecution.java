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
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

 

import com.gemstone.gemfire.cache.execute.Execution;
import com.gemstone.gemfire.cache.execute.Function;
import com.gemstone.gemfire.cache.execute.ResultCollector;

/**
 * @author  David Turanski
 */

public abstract class FunctionExecution {
	protected final Log logger = LogFactory.getLog(this.getClass());
	
	private volatile ResultCollector<?, ?> collector;
	private final Serializable[] args;
	private final Function function;
	private boolean functionRegistered;
	
	

	public FunctionExecution(Function function, Serializable... args) {
		this.function  = function;
		this.args = args;
	}
	
	
	public ResultCollector<?, ?> getCollector() {
		return collector;
	}

	public Serializable[] getArgs() {
		return args;
	}

	public String getFunctionId() {
		return function.getId();
	}
	
	public Function getFunction() {
		return function;
	}
 
	
 
	public void setCollector(ResultCollector<?, ?> collector) {
		this.collector = collector;
	} 
	
	public boolean isFunctionRegistered() {
		return functionRegistered;
	}

	public void setFunctionRegistered(boolean functionRegistered) {
		this.functionRegistered = functionRegistered;
	}
	
	public Object execute() {
		Execution execution = this.getExecution();
		if (getKeys() != null) {
			execution = execution.withFilter(getKeys());
		}
		if (getCollector() != null) {
			execution = execution.withCollector(getCollector());
		}
		 ResultCollector<?,?> resultsCollector = null;
		 
		if (isFunctionRegistered()){
			 resultsCollector =  execution.withArgs(getArgs()).execute(function.getId());
		} else {
			resultsCollector =  execution.withArgs(getArgs()).execute(function);
		}
		if (function.hasResult()){
			return resultsCollector.getResult();
		} else {
			return null;
		}
	}
	
	protected abstract Execution getExecution();
	
	protected Set<?> getKeys() {
		return null;
	}

}