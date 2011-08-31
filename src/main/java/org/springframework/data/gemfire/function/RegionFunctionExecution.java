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

import com.gemstone.gemfire.cache.Region;
import com.gemstone.gemfire.cache.execute.Execution;
import com.gemstone.gemfire.cache.execute.Function;
import com.gemstone.gemfire.cache.execute.FunctionService;

/**
 * @author David Turanski
 * 
 */
public class RegionFunctionExecution<T> extends FunctionExecution<T> {


	private final Region<?, ?> region;
	private volatile Set<?> keys;
	
	public RegionFunctionExecution(Region<?, ?> region, Function function, Serializable... args) {
		super(function, args);
		this.region = region;
	}
	
	public RegionFunctionExecution(Region<?, ?> region, String functionId, Serializable... args) {
		super(functionId, args);
		this.region = region;
	}
	
	public void setKeys(Set<?> keys) {
		this.keys = keys;
	}
	
	protected Set<?> getKeys() {
		return this.keys;
	}

	/* (non-Javadoc)
	 * @see org.springframework.data.gemfire.function.FunctionExecution#getExecution()
	 */
	@Override
	protected Execution getExecution() {
		return FunctionService.onRegion(region);
	}
}
