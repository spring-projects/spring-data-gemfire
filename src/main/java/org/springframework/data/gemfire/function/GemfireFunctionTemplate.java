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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.RegionService;
import com.gemstone.gemfire.cache.execute.Execution;
import com.gemstone.gemfire.cache.execute.Function;
import com.gemstone.gemfire.cache.execute.FunctionService;

/**
 * @author David Turanski
 *
 */
public class GemfireFunctionTemplate<T> implements InitializingBean, GemfireFunctionOperations<T> {
	/** Logger available to subclasses */
	protected final Log log = LogFactory.getLog(getClass());
	private RegionService cache; 
	private long timeout;
	
	 
	/**
	 *  
	 * @param cache
	 */
	public GemfireFunctionTemplate (RegionService cache) {
		this.cache = cache;
		afterPropertiesSet();
	}
	
 
	public void afterPropertiesSet() {	
	}
	
	
 	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.GemfireFunctionOperations#executeOnRegion(com.gemstone.gemfire.cache.execute.Function, java.lang.String, java.io.Serializable)
	 */
 	public  List<T> executeOnRegion(Function function, String regionId, Serializable... args) {
		Region<?,?> region = getRegion(regionId);
		Assert.notNull(region,"Region '" + regionId + "' not found");
	 		RegionFunctionExecution<T> execution = new RegionFunctionExecution<T>(region, function, args);
	 		execution.setTimeout(this.timeout);
	 	return execution.execute();
	}
 	
 	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.GemfireFunctionOperations#executeOnRegionAndExtract(com.gemstone.gemfire.cache.execute.Function, java.lang.String, java.io.Serializable)
	 */
 	public T executeOnRegionAndExtract(Function function, String regionId, Serializable... args) {
		Region<?,?> region = getRegion(regionId);
		Assert.notNull(region,"Region '" + regionId + "' not found");
	 		RegionFunctionExecution<T> execution = new RegionFunctionExecution<T>(region, function, args);
	 		execution.setTimeout(this.timeout);
		return execution.executeAndExtract();
	}
	
	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.GemfireFunctionOperations#executeOnRegion(com.gemstone.gemfire.cache.execute.Function, java.lang.String, java.util.Set, java.io.Serializable)
	 */
	public List<T> executeOnRegion(Function function, String regionId, Set<?> keys, Serializable... args) {
		Region<?,?> region = getRegion(regionId);
		Assert.notNull(region,"Region '" + regionId + "' not found");
        
		RegionFunctionExecution<T> execution = new RegionFunctionExecution<T>(region, function, args);
		execution.setKeys(keys);
		execution.setTimeout(this.timeout);
		return execution.execute();
	}
	
	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.GemfireFunctionOperations#executeOnRegion(java.lang.String, java.lang.String, java.io.Serializable)
	 */
	public List<T>  executeOnRegion(String functionId, String regionId, Serializable... args) {
		Region<?,?> region = getRegion(regionId);
		Assert.notNull(region,"Region '" + regionId + "' not found");
	 	
		RegionFunctionExecution<T> execution = new RegionFunctionExecution<T>(region, functionId, args);
	 	execution.setTimeout(this.timeout);
	 	return execution.execute();
	}
	
	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.GemfireFunctionOperations#executeOnRegion(java.lang.String, java.lang.String, java.util.Set, java.io.Serializable)
	 */
	public List<T> executeOnRegion(String functionId, String regionId, Set<?> keys, Serializable... args) {
		Region<?,?> region = getRegion(regionId);
		Assert.notNull(region,"Region '" + regionId + "' not found");
        
		RegionFunctionExecution<T> execution = new RegionFunctionExecution<T>(region, functionId, args);
		execution.setKeys(keys);
		execution.setTimeout(this.timeout);
		return execution.execute();
	}
	
	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.GemfireFunctionOperations#executeOnRegion(java.lang.String, org.springframework.data.gemfire.function.GemfireFunctionCallback)
	 */
	public T executeOnRegion(String regionId, GemfireFunctionCallback<T> callback ) {
		Region<?,?> region = getRegion(regionId);
		Assert.notNull(region,"Region '" + regionId + "' not found");
		Execution execution = FunctionService.onRegion(region);
		return callback.doInGemfire(execution);
	}
	
	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.GemfireFunctionOperations#executeOnServers(com.gemstone.gemfire.cache.execute.Function, java.io.Serializable)
	 */
	public List<T> executeOnServers(Function function, Serializable... args) {
		ServersFunctionExecution<T> execution = new ServersFunctionExecution<T>(this.cache, function, args);
		execution.setTimeout(this.timeout);
		return execution.execute();
	}
	
	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.GemfireFunctionOperations#executeOnServers(java.lang.String, java.io.Serializable)
	 */
	public List<T> executeOnServers(String functionId, Serializable... args) {
		ServersFunctionExecution<T> execution = new ServersFunctionExecution<T>(this.cache, functionId, args);
		execution.setTimeout(this.timeout);
		return execution.execute();
	}
	
	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.GemfireFunctionOperations#executeOnServers(org.springframework.data.gemfire.function.GemfireFunctionCallback)
	 */
	public T executeOnServers(GemfireFunctionCallback<T> callback ) {
		Execution execution = FunctionService.onServers(this.cache);
		return callback.doInGemfire(execution);
	}
	
	 
	public Region<?,?> getRegion(String regionId) {
		return this.cache.getRegion(regionId);
	}


	public void setTimeout(long timeout) {
		this.timeout = timeout;
	}


	public long getTimeout() {
		return timeout;
	}
	
	 
	 
}
