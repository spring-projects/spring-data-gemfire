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


import com.gemstone.gemfire.cache.execute.Function;

/**
 * 
 * An interface for invoking Gemfire functions
 * 
 * @author David Turanski
 *
 * @param <T> the preferred return type
 */
public interface GemfireFunctionOperations {

	/**
	 * Execute an unregistered function
	 * @param function the function
	 * @param args calling arguments
	 * @return the contents of the results collector
	 */
	public abstract <T> Iterable<T> execute(Function function, Object... args);
	
    /**
     * Execute an unregistered function with an expected singleton result
     * @param function the function
     * @param args calling arguments
     * @return the first item in the results collector 
     */
	public abstract <T> T executeAndExtract(Function function, Object... args);

	/**
	 * Execute a function registered with an ID
	 * @param functionId the function ID
	 * @param args the calling arguments
	 * @return the results
	 */
	public abstract <T> Iterable<T> execute(String functionId, Object... args);
	
	/**
	 * Execute a function registered with an ID and with an expected singleton result
	 * @param functionId the function ID
	 * @param args the calling arguments
	 * @return the first item in the results collector
	 */
	public abstract <T> T executeAndExtract(String functionId, Object... args);	
	
	
    /**
     * Execute a function using a native GemFire {@link Execution} instance
     * @param callback a callback providing the execution instance
     * @return the execution result
     */
	public abstract <T> T execute(GemfireFunctionCallback<T> callback);

}