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
import org.springframework.beans.factory.InitializingBean;
import org.springframework.util.Assert;

import com.gemstone.gemfire.cache.Cache;
import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.execute.Execution;
import com.gemstone.gemfire.cache.execute.Function;
import com.gemstone.gemfire.cache.execute.FunctionService;
import com.gemstone.gemfire.distributed.DistributedSystem;

/**
 * @author David Turanski
 *
 */
public class GemfireFunctionTemplate implements InitializingBean {
	/** Logger available to subclasses */
	protected final Log log = LogFactory.getLog(getClass());
	private Cache cache; 
	private DistributedSystem distributedSystem;
	 
	public <K, V> GemfireFunctionTemplate (Cache cache) {
		this.cache = cache;
		this.distributedSystem = cache.getDistributedSystem();
		afterPropertiesSet();
	}
	
 
	public void afterPropertiesSet() {
		
	}
	
 
	public Object executeOnRegion(Function function, String regionId, Serializable... args) {
		Region<?,?> region = getRegion(regionId);
		Assert.notNull(region,"Region '" + regionId + "' not found");
	 
        
		RegionFunctionExecution execution = new RegionFunctionExecution(region, function, args);
		return execution.execute();
	}
	
	public Object executeOnRegion(Function function, String regionId, Set<?> keys, Serializable... args) {
		Region<?,?> region = getRegion(regionId);
		Assert.notNull(region,"Region '" + regionId + "' not found");
        
		RegionFunctionExecution execution = new RegionFunctionExecution(region, function, args);
		execution.setKeys(keys);
		return execution.execute();
	}
	
	public Object executeOnRegion(String regionId, GemfireFunctionCallback callback ) {
		Region<?,?> region = getRegion(regionId);
		Assert.notNull(region,"Region '" + regionId + "' not found");
		Execution execution = FunctionService.onRegion(region);
		return callback.doInGemfire(execution);
	}
	
	public Object executeOnMembers(Function function, Serializable... args) {
		MembersFunctionExecution execution = new MembersFunctionExecution(this.distributedSystem, function, args);
		return execution.execute();
	}
	
	public Object executeOnMembers(GemfireFunctionCallback callback ) {
		Execution execution = FunctionService.onMembers(this.distributedSystem);
		return callback.doInGemfire(execution);
	}
	
	public Object executeOnMember(GemfireFunctionCallback callback ) {
		Execution execution = FunctionService.onMember(this.distributedSystem,this.distributedSystem.getDistributedMember());
		return callback.doInGemfire(execution);
	}
	
	public Region<?,?> getRegion(String regionId) {
		return this.cache.getRegion(regionId);
	}
	
	 
	 
}
